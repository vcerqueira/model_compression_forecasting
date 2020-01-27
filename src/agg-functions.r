source("src/utils.r")
require(tsensembler)


get_combinations <- 
  function(Y_hat, Y_hat_in, Y_in, Y, in_Xy, Xy, loss_fun, lambda) {
    if (any(is.infinite(as.matrix(Y_hat)))) {
      Y_hat <- as.matrix(Y_hat)
      Y_hat[is.infinite(Y_hat)] <- NA
      Y_hat <- as.data.frame(Y_hat)
    }
    
    if (any(is.infinite(as.matrix(Y_hat_in)))) {
      Y_hat_in <- as.matrix(Y_hat_in)
      Y_hat_in[is.infinite(Y_hat_in)] <- NA
      Y_hat_in <- as.data.frame(Y_hat_in)
    }
    
    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }
    
    if (any(is.na(Y_hat_in))) {
      Y_hat_in <- soft.completion(Y_hat_in)
    }
  
    cat("EWA\n")
    EWA_TF <- 
      Agg.OPERA(Y_hat = Y_hat, Y = Y,Y_hat_in = Y_hat_in,Y_in = Y_in, 
                model = "EWA", 
                loss.type = "absolute",
                use_oob = TRUE, get_committee = FALSE)
    
    cat("Stacking\n")
    Stacking_h <- 
      Agg.Stacking(Y_hat = Y_hat, Y = Y, 
                   Y_hat_IN = Y_hat_in, Y_IN = Y_in)
        
    
    cat("ADE\n")
    ADE_ae <-
      Agg.ADE(
        Y_hat = Y_hat,
        Y = Y,
        Xy = Xy,
        in_Yhat = Y_hat_in,
        in_Y = Y_in,
        in_Xy = in_Xy
      )
    
    cat("oracle\n")
    ORACLE <- Agg.BestSnoop(Y_hat = Y_hat, Y = Y)
    
    cat("BLAST\n")
    BLAST_h <- Agg.BLAST(Y_hat = Y_hat, Y = Y, lambda = 50)
    
    cat("SIMPLE TRIM\n")
    SimpleTrim_h <- 
      Agg.SimpleTrim(Y_hat = Y_hat, Y = Y, omega = .5)
    
    cat("Loss Train\n")
    LossTrain_h <- 
      Agg.LossTrain(Y_hat = Y_hat, in_Yhat = Y_hat_in, in_Y = Y_in)
    
    cat("Best\n")
    Best_h <-
      Agg.BestSingle(
        Yhat = Y_hat,
        in_Yhat = Y_hat_in,
        in_Y = Y_in,
        loss_fun = loss_fun
      )
    
    cat("Simple\n")
    S_h <- Agg.Simple(Y_hat = Y_hat)
    
    
    AEC_TT <-
      Agg.AEC(
        Y_hat = Y_hat,
        Y = Y,
        Y_hat_in = Y_hat_in,
        Y_in = Y_in,
        use_oob = TRUE,
        get_committee = TRUE
      )
    
    
    WL_TT <-
      Agg.WindowLoss(
        Y_hat = Y_hat,
        Y = Y,
        Y_hat_in = Y_hat_in,
        Y_in = Y_in,
        use_oob = TRUE,
        get_committee = TRUE)

    MLPOL_SQR_TT <- 
      tryCatch(Agg.OPERA(Y_hat = Y_hat, Y = Y,Y_hat_in = Y_hat_in,Y_in = Y_in, 
                         model = "MLpol", 
                         loss.type = "square",
                         use_oob = TRUE, get_committee = TRUE),
               error = function(e) S_h)
    
    
    RIDGE_TF <- 
      tryCatch(Agg.OPERA(Y_hat = Y_hat, Y = Y,Y_hat_in = Y_hat_in,Y_in = Y_in, 
                         model = "Ridge", 
                         loss.type = "square",
                         use_oob = TRUE, get_committee = FALSE),
               error = function(e) S_h)
    
 
    FS_TF <- 
      Agg.OPERA(Y_hat = Y_hat, Y = Y,Y_hat_in = Y_hat_in,Y_in = Y_in, 
                model = "FS", 
                loss.type = "absolute",
                use_oob = TRUE, get_committee = FALSE)
    
    OGD_TT <- 
      Agg.OPERA(Y_hat = Y_hat, Y = Y,Y_hat_in = Y_hat_in,Y_in = Y_in, 
                model = "OGD", 
                loss.type = "absolute",
                use_oob = TRUE, get_committee = TRUE)
    
    ResComb <- 
      list(
        Stacking = Stacking_h,
        ADE = ADE_ae,
        ORACLE = ORACLE,
        BLAST = BLAST_h,
        SimpleTrim = SimpleTrim_h,
        LossTrain = LossTrain_h,
        Best = Best_h,
        Simple = S_h,
        AEC = AEC_TT,
        WL = WL_TT,
        MLPOL = MLPOL_SQR_TT,
        RIDGE = RIDGE_TF,
        EWA = EWA_TF,
        FS = FS_TF,
        OGD = OGD_TT
      )
    
    ResComb
  }

Agg.Simple <- function(Y_hat) rowMeans(Y_hat)

Agg.BestSingle <- 
  function(Yhat, in_Yhat, in_Y, loss_fun = rmse) {
    n <- ncol(in_Yhat)
    
    in_l_by_M <- 
      sapply(in_Yhat,
             function(yh) {
               loss_fun(in_Y, yh)
             })
    
    rankm <- rank(in_l_by_M)
    
    bestm <- names(sort(rankm))[1]
    
    BestSingle <- Yhat[,bestm]
    
    BestSingle
  }

Agg.LossTrain <-
  function(Y_hat, in_Yhat, in_Y) {
    in_Yhat <- as.data.frame(in_Yhat)
    in_l_by_M <- 
      sapply(in_Yhat,
             function(yh) {
               rmse(in_Y, yh)
             })
    
    #err_0 <- colMeans(in_l_by_M)
    W_0 <- proportion(normalize(-in_l_by_M, na.rm = TRUE))
    
    W <- rep(list(W_0), times = nrow(Y_hat))
    W <- rbind_l(W)
    colnames(W) <- colnames(Y_hat)
    
    y_hat <- combine_predictions(Y_hat, W, committee = NULL)
    
    y_hat
  }


Agg.BLAST <-
  function(Y_hat, Y, lambda = 50) {
    E <- loss_by_model(Y_hat, Y, se)
    
    moving_loss <- roll_mean_matrix(E, lambda = lambda)
    pre_W <- rep(1./ncol(Y_hat), times = ncol(Y_hat))
    
    W <-
      apply(moving_loss, 1,
            function(j) {
              proportion(normalize(-j, na.rm = TRUE))
            })
    
    W <- as.data.frame(t(W))
    W <- rbind.data.frame(pre_W, W[-NROW(W), ])
    W <- select_best(W)
    
    y_hat <- combine_predictions(Y_hat, W, committee = NULL)
    
    y_hat
  }

Agg.SimpleTrim <-
  function(Y_hat, Y, omega=.5) {
    seq. <- seq_along(Y)
    
    W <- vector("list", nrow(Y_hat) - 1)
    for (i in seq.[-1]) {
      mseq <- seq_len(i-1)
      W[[i]] <- apply(Y_hat, 2, function(j) rmse(j[mseq], Y[mseq]))
    }
    
    W <- rbind_l(W)
    W <- rbind(rep(1., time = ncol(Y_hat)), W)
    colnames(W) <- colnames(Y_hat)
    
    W <- t(apply(W, 1, model_weighting, trans="linear", na.rm=T))
    
    W <- as.data.frame(W)
    C <- get_top_models(W, omega = omega)
    
    y_hat <-
      vnapply(seq.,
              function(j) {
                mean(unlist(Y_hat[j, C[[j]]]))
              })
    
    y_hat
  }

Agg.BestSnoop <-
  function(Y_hat, Y) {
    L <-
      sapply(Y_hat,
             function(yh) {
               se(Y, yh)
             })
    
    which_best <- apply(L, 1, which.min)
    
    seq. <- seq_len(nrow(Y_hat))
    BestExpert <-
      vapply(seq.,
             function(i) {
               Y_hat[i, which_best[i]]
             }, double(1L))
    
    BestExpert
  }

Agg.OPERA <-
  function(Y_hat,Y, Y_hat_in, Y_in, model,loss.type ="square", use_oob, get_committee) {
    #c("Ridge","MLpol","EWA","FS","OGD","BOA")
    
    Y_hat0 <- Y_hat
    Y0 <- Y
    
    if (use_oob) {
      Y_hat <- rbind.data.frame(Y_hat_in,Y_hat)
      Y <- c(Y_in, Y)
    } 
    
    if (any(is.na(Y_hat))) {
      Y_hat <- soft.completion(Y_hat)
    }
    
    ## Mixture
    MIXTURE <- mixture(model = model, loss.type = loss.type)
    for (i in 1:length(Y)) {
      MIXTURE <- predict(MIXTURE, newexperts = Y_hat[i, ], newY = Y[i])
    }
    
    W <- MIXTURE$weights
    W <- tail(W, nrow(Y_hat0))
    
    ##
    
    if (get_committee) {
      cat(".")
      W <- as.data.frame(W)
      C <- get_top_models(scores = W, .5)
      yhat <- combine_predictions(Y_hat0, W, committee = C)
    } else {
      yhat <- combine_predictions(Y_hat0, W, committee = NULL)
    }
    
    yhat
  }

Agg.WindowLoss <-
  function(Y_hat,Y,Y_hat_in, Y_in, use_oob, get_committee) {
    Y_hat0 <- as.data.frame(Y_hat)
    
    if (use_oob) {
      Y_hat <- rbind.data.frame(Y_hat_in,Y_hat)
      Y <- c(Y_in, Y)
    } 
    
    ####
    
    Y_hat <- as.data.frame(Y_hat)
    E <- loss_by_model(Y_hat, Y, se)
    n <- ncol(Y_hat)
    
    moving_loss <- roll_mean_matrix(E, lambda = 50)
    pre_W <- rep(1. / n, times = n)
    
    W <-
      apply(moving_loss, 1,
            function(j) {
              proportion(normalize(-j, na.rm = TRUE))
            })
    
    W <- as.data.frame(t(W))
    W <- rbind.data.frame(pre_W, W[-NROW(W),])
    W <- as.matrix(W)
    
    W <- tail(W, nrow(Y_hat0))
    #####
    
    if (get_committee) {
      W <- as.data.frame(W)
      C <- get_top_models(W, .5)
      yhat <- combine_predictions(Y_hat0, W, committee = C)
    } else {
      yhat <- combine_predictions(Y_hat0, W, committee = NULL)
    }
    
    yhat
  }


Agg.AEC <-
  function(Y_hat,Y,Y_hat_in, Y_in, use_oob, get_committee) {
    ff = .9
    
    Y_hat0 <- as.data.frame(Y_hat)
    
    if (use_oob) {
      Y_hat <- rbind.data.frame(Y_hat_in,Y_hat)
      Y <- c(Y_in, Y)
    } 
    
    ###
    
    E <- loss_by_model(Y_hat, Y)
    
    W <-
      lapply(seq_len(ncol(Y_hat)),
             function(o) {
               y_hat <- Y_hat[, o]
               e_y <- E[, o]
               
               r <-
                 vnapply(seq_along(y_hat)[-(1:2)],
                         function(y_w) {
                           var_y <- var(y_hat[seq_len(y_w - 1)], na.rm = T)
                           v <- (1 / sqrt(var_y))
                           
                           v * exp(-((e_y[y_w - 1] ^ 2) / (2 * var_y))) * ff
                         })
               c(1., 1., r)
             })
    
    W <- as.data.frame(W)
    colnames(W) <- colnames(Y_hat)
    
    bad_m <- which(sapply(W, function(j) any(is.na(j))))
    
    if (length(bad_m) > 0) {
      W <- W[,-bad_m]
      Y_hat <- Y_hat[,-bad_m]
    }
    
    ###
    W <- tail(W, nrow(Y_hat0))
    
    if (get_committee) {
      W <- as.data.frame(W)
      C <- get_top_models(W, .5)
      yhat <- combine_predictions(Y_hat0, W, committee = C)
    } else {
      yhat <- combine_predictions(Y_hat0, W, committee = NULL)
    }
    
    yhat
  }


Agg.ADE <-
  function(Y_hat,Y, Xy, in_Yhat, in_Y,in_Xy) {
    require(Metrics)
    require(ranger)
    require(tsensembler)
    require(DMwR)
    
    in_Xy$target <- NULL
    Xy$target <- NULL
    
    lambda <- 50
    omega <- .5
    
    in_E <- in_Yhat
    in_E[] <-
      lapply(in_E,
             function(yh) {
               ae(in_Y, yh)
             })
    
    Xz <-
      lapply(in_E,
             function(l) {
               cbind.data.frame(in_Xy, score = l)
             })
    
    Z <-
      lapply(Xz,
             function(xz) {
               ranger(score ~.,
                      xz,
                      #mtry = NCOL(xz) / 3,
                      num.trees = 500)
             })
    
    seq. <- 1:length(Y)
    N <- ncol(Y_hat)
    
    E_hat <- lapply(Z,
                    function(o) {
                      predict(o, Xy)$predictions
                    })
    
    names(E_hat) <- colnames(Y_hat)
    E_hat <- abs(as.data.frame(E_hat))
    
    
    Yhat_ext <- rbind.data.frame(tail(in_Yhat,lambda), Y_hat)
    Y_ext <- c(tail(in_Y,lambda), Y)
    
    C <- build_committee(Yhat_ext,Y_ext,lambda,omega)
    C <- C[-seq_len(lambda)]
    C <- lapply(C, unname)
    
    
    W <- matrix(0., ncol = N, nrow = length(Y))
    for (j in seq.) {
      W_j <- E_hat[j, C[[j]]]
      W_j <- model_weighting(W_j, "linear")
      
      W[j,  C[[j]]] <- W_j
      W[j, -C[[j]]] <- 0.
    }
    colnames(W) <- colnames(Y_hat)
    
    ssimilarity <- sliding_similarity(Y_hat, lambda)
    ssimilarity <- c(as.list(rep(NA, times=lambda)), ssimilarity)
    W <- sequential_reweighting(ssimilarity, W)
    
    y_hat <- combine_predictions(Y_hat, W, NULL)
    
    y_hat
  }



Agg.Stacking <-
  function(Y_hat, Y, Y_hat_IN, Y_IN) {
    require(ranger)
    colnames(Y_hat_IN) <- paste0("M",1:ncol(Y_hat_IN))
    colnames(Y_hat) <- paste0("M",1:ncol(Y_hat))
    
    ZDF <- cbind.data.frame(Y_hat_IN, METATARGET = Y_IN)
    
    Z <-
      ranger(
        METATARGET ~ .,
        ZDF,
        num.trees = 500,
        mtry = ncol(ZDF) / 3)
    
    ZDF_TEST <- cbind.data.frame(Y_hat, METATARGET = -1)
    
    predict(Z, ZDF_TEST)$predictions
  }

