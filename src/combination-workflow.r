get_final_predictions <- 
  function(RESULTX, specs) {
    x <- RESULTX
    x$VALK$target <- -1
    x$TESTK$target <- -1
    
    cat("Standard Model Combination\n")
    C_hat <-
      get_combinations(
        Y_hat = x$YHAT_TEST,
        Y_hat_in = x$YHAT_VAL,
        Y_in = x$Y_VAL,
        Y = x$Y_TEST,#full_Y,
        in_Xy = x$VALK,
        Xy = x$TESTK,
        loss_fun = rmse,
        lambda = 50
      )
    
    cat("Getting Teacher From TRAINING\n")
    Teachers_TR <-
      get_combinations(
        Y_hat = x$YHAT_TRAIN,
        Y_hat_in = x$YHAT_TRAIN,
        Y_in = x$Y_TRAIN,
        Y = x$Y_TRAIN,
        in_Xy = x$TRAINK,
        Xy = x$TRAINK,
        loss_fun = rmse,
        lambda = 50)
    
    
    cat("Getting Teacher From Validation\n")
    Teachers <-
      get_combinations(
        Y_hat = x$YHAT_VAL,
        Y_hat_in = x$YHAT_VAL,
        Y_in = x$Y_VAL,
        Y = x$Y_VAL,
        in_Xy = x$VALK,
        Xy = x$VALK,
        loss_fun = rmse,
        lambda = 50)
    
    
    teaching_data <- x$VALK
    teaching_data_tr <- x$TRAINK
    
    cat("Teaching Students\n")
    Students <- vector("list", length(Teachers))
    for (i in 1:length(Teachers)) {
      cat("Student", i, "of ", length(Teachers), "\n")
      teacher_i <- Teachers[[i]]
      if (any(is.infinite(teacher_i))) {
        teacher_i[is.infinite(teacher_i)] <- NA
      }
      
      if (any(is.na(teacher_i))) {
        teacher_i[is.na(teacher_i)] <- mean(teacher_i, na.rm=TRUE)
      }
      
      teaching_data$target <- teacher_i
      
      M <-
        build_base_ensemble(
          form = target ~ .,
          data = teaching_data,
          specs = specs,
          num_cores = 1
        )
      
      ST_hat <- predict(M, x$TESTK)
      colnames(ST_hat) <- paste0("St.", colnames(ST_hat))
      
      Students[[i]] <- ST_hat
    }
    
    names(Students) <- paste0("St.", names(Teachers))
    
    ####
    
    cat("Teaching Students in Training Data\n")
    StudentsTR <- vector("list", length(Teachers_TR))
    for (i in 1:length(Teachers_TR)) {
      cat("Student", i, "of ", length(Teachers_TR), "\n")
      teacher_i <- Teachers_TR[[i]]
      if (any(is.infinite(teacher_i))) {
        teacher_i[is.infinite(teacher_i)] <- NA
      }
      
      if (any(is.na(teacher_i))) {
        teacher_i[is.na(teacher_i)] <- mean(teacher_i, na.rm=TRUE)
      }
      
      teaching_data_tr$target <- teacher_i
      
      M <-
        build_base_ensemble(
          form = target ~ .,
          data = teaching_data_tr,
          specs = specs,
          num_cores = 1
        )
      
      ST_hat <- predict(M, x$TESTK)
      colnames(ST_hat) <- paste0("St.", colnames(ST_hat))
      
      StudentsTR[[i]] <- ST_hat
    }
    names(StudentsTR) <- paste0("St.", names(Teachers_TR))
    
    COMBS <-
      list(
        C_hat = C_hat,
        Y_hat = x$YHAT_TEST,
        Students = Students,
        StudentsTR=StudentsTR)
    
    list(Combs=COMBS, 
         Y=x$Y_TEST, 
         OTHER=RESULTX)
  }


