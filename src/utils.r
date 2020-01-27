rep_holdout_origins <- 
  function(n, nreps, train_size, test_size) {
    tr_size <- as.integer(n * train_size)
    ts_size <- as.integer(n * test_size)
    selection_range <- (tr_size + 1):(n - ts_size + 1)
    origins <- sample(selection_range, nreps)
    
    # mcapprox <-
    #   lapply(origins, function(o) {
    #     train <- x[(o - tr_size):(o - 1),]
    #     test <- x[o:(o + ts_size - 1),]
    # 
    #   })
    
    origins
  }


create_datasets <- 
  function(train, test, k) {
    train <- as.numeric(train)
    test <- as.numeric(test)
    
    tr_k <- embed_timeseries(train, k)
    
    tr_tail <- tail(train, k-1)
    
    ts_k <- embed_timeseries(c(tr_tail,test), k)
    
    list(train=tr_k,test=ts_k)
  }

make_names <- 
  function(x) {
    x <-
      sapply(x,
             function(z) {
               split_by_(as.character(z))[1]
             }, USE.NAMES = FALSE)
    x <- toupper(x)
    
    x
  }

loss_by_model <-
  function(Y_hat, Y, lfun = se) {

  models_loss <-
    lapply(Y_hat,
           function(o) {
             lfun(Y, o)
           })

  as.data.frame(models_loss)
}

ts_holdout <-
  function(x, ratio, frq) {
    len <- NROW(x)
    
    train <- head(x, ceiling(ratio * len))
    test <- tail(x, len - ceiling(ratio * len))
    
    train <- ts(train, frequency = frq)
    test <- ts(test, frequency = frq)
    
    stopifnot(is.ts(train))
    stopifnot(is.ts(test))
    
    list(train=train,test=test)
  }

blocked_prequential <-
  function(form, x, nfolds, FUN, ...) {
    f <- cut(seq_len(NROW(x)), breaks = nfolds, labels = FALSE)
    frq <- frequency(x)
    
    RESULT <- vector("list", nfolds - 1)
    for (i in 1:(nfolds-1)) {
      trID <- which(f %in% seq_len(i))
      tsID <- which(f == i + 1L)
      
      train <- ts(x[trID], frequency = frq)
      test  <- ts(x[tsID], frequency = frq)
      
      RESULT[[i]] <- FUN(form, train, test, ...)
    }
    
    RESULT
  }

  
log_trans <- function(x) sign(x) * log(abs(x) + 1)

proportion <- function(x) x / sum(x, na.rm = T)

  
percentual_difference <-
  function(x,y) {
    ((x - y) / abs(y)) * 100
  }


#https://github.com/M4Competition/M4-methods
mase_cal <- function(insample, outsample, forecasts) {
  stopifnot(stats::is.ts(insample))
  #Used to estimate MASE
  frq <- stats::frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}

