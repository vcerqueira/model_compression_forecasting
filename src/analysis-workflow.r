all_res <- 
  function(ts_FINALRES) {
    x <- ts_FINALRES
    
    y <- x$Y
    
    y_hat_original <- x$Combs$Y_hat
    c_hat <- x$Combs$C_hat
    
    err_org <- 
      sapply(y_hat_original,
             function(yh) {
               rmse(y,yh)
               #mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_dtld <- 
      sapply(c_hat,
             function(yh) {
               rmse(y,yh)
               #mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_pdiff <- c(err_dtld,err_org)
      #percentual_difference(err_dtld, err_org)
    
    err_pdiff
  }

kd_analysis_base <- 
  function(ts_FINALRES, teacher="St.ADE", students="StudentsTR") {
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    
    y <- x$Y
    y_hat_original <- x$Combs$Y_hat
    y_hat_dtld_simple <- x$Combs[[students]][[teacher]]
    
    err_org <- 
      sapply(y_hat_original,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_dtld <- 
      sapply(y_hat_dtld_simple,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_pdiff <- 
      percentual_difference(err_dtld, err_org)
    
    err_pdiff
  }


kd_analysis_err_ind <- 
  function(ts_FINALRES, teacher="St.ADE",students="StudentsTR") {
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    
    y <- x$Y
    y_hat_dtld <- x$Combs[[students]][[teacher]]
    
    err_dtld <- 
      sapply(y_hat_dtld,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_dtld
  }

kd_analysis_err_ori <- 
  function(ts_FINALRES, teacher="St.ADE") {
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    
    y <- x$Y
    y_hat_ori <- x$Combs$Y_hat
    
    err_ori <- 
      sapply(y_hat_ori,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_ori
  }


kd_analysis_err_comb <- 
  function(ts_FINALRES, teacher="St.ADE") {
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    y <- x$Y
    y_hat_comb <- x$Combs$C_hat
    y_hat_comb <- as.data.frame(y_hat_comb)
    
    err_comb <- 
      sapply(y_hat_comb,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_comb
  }


kd_analysis_comb <- 
  function(ts_FINALRES, student="St.cub_50it0_nn", students="StudentsTR") {
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    y <- x$Y
    
    y_hat_comb_ori <- x$Combs$C_hat
    y_hat_comb_ori <- as.data.frame(y_hat_comb_ori)
    
    y_hat_comb_dtl <-
      lapply(x$Combs[[students]],
             function(yhatl) {
               yhatl[, student, drop = TRUE]
             })
    
    y_hat_comb_dtl <- as.data.frame(y_hat_comb_dtl)
    
    
    err_org <- 
      sapply(y_hat_comb_ori,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_dtld <- 
      sapply(y_hat_comb_dtl,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_pdiff <- 
      percentual_difference(err_dtld, err_org)
    
    err_pdiff
  }


kd_analysis_bestVcomb <- 
  function(ts_FINALRES, 
           student="St.cub_50it0_nn", 
           students="StudentsTR",
           teacher="St.Simple") {
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    y <- x$Y
    
    y_hat_comb_ori <- x$Combs$C_hat
    y_hat_comb_ori <- as.data.frame(y_hat_comb_ori)
    
    y_hat_comb_dtl <-
      lapply(x$Combs[[students]],
             function(yhatl) {
               yhatl[, student, drop = TRUE]
             })
    
    yhat_dist <- y_hat_comb_dtl[[teacher]]
    
    err_dist <- mean(mase_cal(y_tr,y, yhat_dist), na.rm=TRUE)
    
    err_org <- 
      sapply(y_hat_comb_ori,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_pdiff <- 
      percentual_difference(err_dist,err_org)
    
    err_pdiff
  }


kd_analysis_comb_avg <- 
  function(ts_FINALRES, 
           student="St.cub_50it0_nn", students="StudentsTR") {
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    y <- x$Y
    
    y_hat_comb_ori <- x$Combs$C_hat
    
    y_hat_comb_dtl <-
      lapply(x$Combs[[students]],
             function(yhatl) {
               yhatl[, student, drop = TRUE]
             })
    
    best_students <- 
      sapply(y_hat_comb_dtl,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
             })
    
    topk <- length(best_students)#10
    nms <- names(sort(best_students)[1:topk])
    
    sbs <- y_hat_comb_dtl[nms]
    
    fmethods <- c(y_hat_comb_ori, sbs)
    
    err <- 
      sapply(fmethods,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err
  }


kd_analysis_combVcomb <- 
  function(ts_FINALRES, student="St.rf_n_500_m_10") {
    
    x <- ts_FINALRES
    
    y_tr <- x$OTHER$TRAIN_RAW
    y <- x$Y
    
    y_hat_comb1 <-
      lapply(x$Combs$Students,
             function(yhatl) {
               yhatl[, student, drop = TRUE]
             })
    
    y_hat_comb1 <- as.data.frame(y_hat_comb1)
    
    y_hat_comb2 <-
      lapply(x$Combs$StudentsTR,
             function(yhatl) {
               yhatl[, student, drop = TRUE]
             })
    
    y_hat_comb2 <- as.data.frame(y_hat_comb2)
    
    err_c1 <- 
      sapply(y_hat_comb1,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_c2 <- 
      sapply(y_hat_comb2,
             function(yh) {
               mean(mase_cal(y_tr,y, yh), na.rm=TRUE)
               #mae(y, yh) / mean(y_tr)
             })
    
    err_pdiff <- 
      percentual_difference(err_c1, err_c2)
    
    err_pdiff
  }
