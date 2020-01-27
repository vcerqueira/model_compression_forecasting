load("data/datasets.rdata")

library(devtools)
#install_github("vcerqueira/tsensembler")
library(tsensembler)
library(forecast)

source("src/utils.r")
source("src/ensemble-specs.r")
source("src/predicting-workflow.r")
source("src/arima-et-al.r")

form <- target ~.

nreps <- 10
trs <- .5
tss <- .2

IDS <- 1:90

RESULTS <- vector("list", length(ts_list))
for (i in IDS) {#
  cat(i,"\n\n\n\n\n\n")
  
  x <- ts_list[[i]]
  #x <- head(x,400)
  n <- length(x)
  frq <- frequency(x)
  ORIGINS <- rep_holdout_origins(n, nreps, trs, tss)
  
  TRs <- ceiling(trs * n)
  TSs <- ceiling(tss * n)
  
  ITER_RESULT <- vector("list", nreps)
  for (it in 1:nreps) {
    #cat(it,"\n")
    o <- ORIGINS[it]
    
    TRAIN <- x[(o - TRs):(o - 1)]
    TEST <- x[o:(o + TSs - 1)]
    
    TRAIN <- ts(TRAIN, frequency = frq)
    TEST <- ts(TEST, frequency = frq)
    
    YHAT_TRAD <-
      trad_methods(train_y = TRAIN,
                   test_y = TEST)
    #TRAIN <- TS_SPLIT$train
    #TEST <- TS_SPLIT$test
    
    YHAT_TEST <-
      get_predictions(
        form = form,
        train = TRAIN,
        test = TEST,
        specs = MODELSPECS
      )
    
    BP_RESULTS <- 
      blocked_prequential(
        form = form,
        x = TRAIN,
        nfolds = 10,
        FUN = get_predictions,
        specs = MODELSPECS
      )
    
    YHAT_VAL <- lapply(BP_RESULTS, function(x) x$Y_hat)
    YHAT_VAL <- do.call(rbind, YHAT_VAL)
    
    Y_VAL <- lapply(BP_RESULTS, function(x) x$Y)
    Y_VAL <- do.call(c, Y_VAL)
    
    VALK <- lapply(BP_RESULTS, function(x) x$TESTK)
    VALK <- do.call(rbind, VALK)
    
    ITER_RESULT[[it]] <- 
      list(YHAT_TEST=YHAT_TEST$Y_hat,
           Y_TEST = YHAT_TEST$Y,
           YHAT_TRAIN = YHAT_TEST$Y_hat_TR,
           Y_TRAIN = YHAT_TEST$Y_TR,
           MODELSIZE = YHAT_TEST$MODELSIZE,
           TESTTIME = YHAT_TEST$TESTTIME,
           YHAT_VAL = YHAT_VAL,
           Y_VAL = Y_VAL,
           VALK = VALK,
           TRAIN_RAW = TRAIN,
           TEST_RAW = TEST,
           TESTK = YHAT_TEST$TESTK,
           TRAINK = YHAT_TEST$TRAINK,
           YHAT_TRAD = YHAT_TRAD,
           FRQ = frq,
           OOB = BP_RESULTS)
  }
  
  RESULTS[[i]] <- ITER_RESULT
  
  #SIGNATURE <- paste0("F",IDS[1],"_",IDS[length(IDS)])
  #save(RESULTS, file = paste0("KD_BASE_",SIGNATURE,".rdata"))
  save(RESULTS, file = "BaseResults.rdata")
}
