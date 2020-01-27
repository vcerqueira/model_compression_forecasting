get_predictions <- 
  function(form, train, test, specs) {
    frq <- frequency(train)
    k <- 15
    
    DS <-
      create_datasets(train = train,
                      test = test,
                      k = k)
    
    trainx <- DS$train[complete.cases(DS$train),]
    
    M0 <-
      build_base_ensemble(
        form = form,
        data = trainx,
        specs = specs,
        num_cores = 1
      )
    
    MODELSIZE <- 
      sapply(M0@base_models,
           function(x) {
             object.size(x)
           })
    
    Y_hat <- predict(M0, DS$test)
    
    Y_hat_TR <- predict(M0, trainx)
    
    ttimecost <- attr(Y_hat,"Times")
    
    Y <- as.vector(test)
    
    
    list(
      Y_hat = Y_hat,
      Y = Y,
      Y_hat_TR = Y_hat_TR,
      Y_TR = trainx$target,
      TRAINK = trainx,
      TESTK = DS$test,
      MODELSIZE = MODELSIZE,
      TESTTIME = ttimecost
    )
  }