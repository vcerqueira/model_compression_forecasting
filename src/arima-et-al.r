trad_methods <- 
  function(train_y, test_y) {
    require(forecast)
    
    stopifnot(class(train_y) == "ts")
    stopifnot(class(test_y) == "ts")
    
    model_aa <- tryCatch(auto.arima(train_y),
                         error = function(e) {
                           Arima(train_y, order = c(1,0,0))
                         })
    
    model_ets <-
      tryCatch(
        ets(train_y),
        error = function(e) {
          Arima(train_y, order = c(1, 0, 0))
        }
      )
    
    model_tbats <- 
      tryCatch(tbats(train_y),
               error = function(e) {
                 Arima(train_y, order = c(1,0,0))
               })
    
    preds_aa <- 
      tryCatch(as.vector(fitted(Arima(test_y, model=model_aa))),
               error = function(e) {
                 m <- Arima(train_y, order = c(1, 0, 0))
                 as.vector(fitted(Arima(test_y, model=m)))
               })
    
    preds_ets <- 
      tryCatch(as.vector(fitted(ets(test_y, model=model_ets))),
               error = function(e) {
                 m <- Arima(train_y, order = c(1, 0, 0))
                 as.vector(fitted(Arima(test_y, model=m)))
               })
    preds_tb <- 
      tryCatch(as.vector(fitted(tbats(test_y, model=model_tbats))),
               error = function(e) {
                 m <- Arima(train_y, order = c(1, 0, 0))
                 as.vector(fitted(Arima(test_y, model=m)))
               })
    
    YHAT <-
      list(ARIMA = preds_aa,
           ETS = preds_ets,
           TBATS = preds_tb)
    
    YHAT
  }