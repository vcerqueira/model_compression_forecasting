box_plot3 <-
  function(x,take_logs) {
    require(reshape2)
    require(ggplot2)
    
    xp <- melt(as.data.frame(x))
    
    # if (!is.null(new_labels)) {
    #   levels(xp$variable) <- new_labels
    # }
    
    if (take_logs) {
      xp$value <- log_trans(xp$value)
    }
    
    p <- ggplot(xp, aes(x = 1, y = value)) +
      facet_wrap(~ variable,
                 nrow = 1,
                 scales = "free_x") + 
      geom_boxplot() +
      geom_hline(yintercept = 0, col = "red") +
      theme_minimal() +
      labs(x = "",
           y = "Log. Percentual Diff.") +
      theme(axis.text.x  = element_blank()) +
      theme(
        axis.text.y  = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(angle = 90, size = 12)
      )
    
    p
  }


kd_bayesrank_plot <- 
  function(X) {
    xDF <- melt(X)
    
    colnames(xDF) <-
      c("Result", "Method", "value")
    
    ggplot(xDF, aes(x = Method,
                    y = value,
                    fill = Result)) +
      geom_col(position = "fill") +
      ylab("Proportion of probability") +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x  = element_text(
        angle = 90,
        size = 10,
        hjust = 1
      ),
      legend.position = "top") +
      theme(axis.text.y  = element_text(size = 11),
            axis.title.y = element_text(size = 11))
    
  }


avg_rank_plot <- 
  function(avg,sdev,col0="#33CCCC", col_per_st=FALSE) {
    require(ggplot2)
    
    ord <- names(sort(avg))
    methods <- names(avg)
    
    ds <- data.frame(avg=avg,sdev=sdev, methods=methods, row.names = NULL)
    ds$methods <- factor(ds$methods, levels = ord)
    
    if(col_per_st) {
      meths <- ds$methods
      ids <- grep("^ST\\.", meths)
      cols <- rep("#33CCCC",times=length(meths))
      cols[ids] <- "darkorange3"
      cols
    } else {
      cols <- col0
    }
    
    ggplot(data = ds,
           aes(x = methods,
               y = avg)) +
      geom_bar(stat="identity",
               fill=cols) +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 45,
        size = 11)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      geom_errorbar(aes(ymin = avg - sdev,
                        ymax = avg + sdev),
                    width = .5,
                    position = position_dodge(.9)) +
      labs(x="",
           y="Avg. Rank",
           title = "")
  }


avg_rank_plot2 <- 
  function(avg,sdev,col0="#33CCCC", col_per_trad=FALSE) {
    require(ggplot2)
    
    ord <- names(sort(avg))
    methods <- names(avg)
    
    ds <- data.frame(avg=avg,sdev=sdev, methods=methods, row.names = NULL)
    ds$methods <- factor(ds$methods, levels = ord)
    
    if(col_per_trad) {
      meths <- ds$methods
      ids <- grep("ARIMA|ETS|TBATS", meths)
      cols <- rep("#33CCCC",times=length(meths))
      cols[ids] <- "hotpink4"
      cols
    } else {
      cols <- col0
    }
    
    ggplot(data = ds,
           aes(x = methods,
               y = avg)) +
      geom_bar(stat="identity",
               fill=cols) +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 45,
                                        size = 11)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      geom_errorbar(aes(ymin = avg - sdev,
                        ymax = avg + sdev),
                    width = .5,
                    position = position_dodge(.9)) +
      labs(x="",
           y="Avg. Rank",
           title = "")
  }


bp_ind_models <-
  function(FRESULTS, fix_names=FALSE) {
    require(reshape2)
    require(ggplot2)
    
    err <-
      sapply(FRESULTS,
             function(x) {
               x1<<-x#<-x1
               yhat <- x$Combs$Y_hat
               y <- x$Y
               y_tr <- x$OTHER$TRAIN_RAW
               
               sapply(yhat, function(yh) {
                 mean(mase_cal(y_tr,y,yh), na.rm=TRUE)
                 #rmse(y, yh)
               })
             })
    
    err <- t(err)
    
    ind_rank <- t(apply(err, 1, rank))
    
    avgrank_sort <- names(sort(apply(ind_rank,2,median)))
    
    if (fix_names)  {
      spl <-
        sapply(avgrank_sort,
               function(z) {
                 split_by_(as.character(z))[1]
               }, USE.NAMES = FALSE)
      # 
      spl <- toupper(spl)
      ids<-which(spl=="MVR")
      spl[ids[1]] <- "PLS"
      spl[ids[2]] <- "PCR"
      # 
      spl <- gsub("NNET","MLP", spl)
      spl <- gsub("SVM","SVR", spl)
      spl <- gsub("CUB","RBR", spl)
      # spl[46] <- "PLS"
      # spl[47] <- "PCR"
      # spl[35] <- "PLS"
    }
    
    ind_rank <- ind_rank[,avgrank_sort]
    ind_rank <- as.data.frame(ind_rank)
    
    x_melted <- melt(ind_rank)
    
    p <- ggplot(x_melted, aes(variable, value))
    
    p <- p  +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Rank") +
      theme(axis.text.x  = element_text(size=10,
                                        angle=90)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) #+
    #scale_x_discrete(labels=spl)
    
    if (fix_names) {
      p <- p +
        scale_x_discrete(labels=spl)
    }
    
    p
  }


bp_ind_models_mc <-
  function(FRESULTS, fix_names=FALSE) {
    #FRESULTS<-x
    require(reshape2)
    require(ggplot2)
    
    err <-
      sapply(FRESULTS,
             function(FRESULTS_i) {
               
               err_i <-
                 sapply(FRESULTS_i,
                        function(x) {
                          x1 <<- x#<-x1
                          yhat <- x$Combs$Y_hat
                          y <- x$Y
                          y_tr <- x$OTHER$TRAIN_RAW
                          
                          sapply(yhat, function(yh) {
                            #mean(mase_cal(y_tr,y,yh), na.rm=TRUE)
                            rmse(y, yh)
                          })
                        })
               
               rowMeans(err_i)
             })
    
    err <- t(err)
    
    ind_rank <- t(apply(err, 1, rank))
    
    avgrank_sort <- names(sort(apply(ind_rank,2,median)))
    
    if (fix_names)  {
      spl <-
        sapply(avgrank_sort,
               function(z) {
                 split_by_(as.character(z))[1]
               }, USE.NAMES = FALSE)
      # 
      spl <- toupper(spl)
      ids<-which(spl=="MVR")
      spl[ids[1]] <- "PLS"
      spl[ids[2]] <- "PCR"
      # 
      spl <- gsub("NNET","MLP", spl)
      spl <- gsub("SVM","SVR", spl)
      spl <- gsub("CUB","RBR", spl)
      # spl[46] <- "PLS"
      # spl[47] <- "PCR"
      # spl[35] <- "PLS"
    }
    
    ind_rank <- ind_rank[,avgrank_sort]
    ind_rank <- as.data.frame(ind_rank)
    
    x_melted <- melt(ind_rank)
    
    p <- ggplot(x_melted, aes(variable, value))
    
    p <- p  +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Rank") +
      theme(axis.text.x  = element_text(size=10,
                                        angle=90)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) #+
    #scale_x_discrete(labels=spl)
    
    if (fix_names) {
      p <- p +
        scale_x_discrete(labels=spl)
    }
    
    p
  }

bp_ind_models_mc0 <-
  function(FRESULTS, fix_names=FALSE) {
    #FRESULTS<-x
    require(reshape2)
    require(ggplot2)
    
    err <-
      sapply(FRESULTS,
             function(FRESULTS_i) {
               
               err_i <-
                 sapply(FRESULTS_i,
                        function(x) {
                          x1 <<- x#<-x1
                          yhat <- x$Combs$Y_hat
                          y <- x$Y
                          y_tr <- x$OTHER$TRAIN_RAW
                          
                          sapply(yhat, function(yh) {
                            #mean(mase_cal(y_tr,y,yh), na.rm=TRUE)
                            rmse(y, yh)
                          })
                        })
               
               rowMeans(err_i)
             })
    
    err <- t(err)
    
    ind_rank <- t(apply(err, 1, rank))
    
    avgrank_sort <- names(sort(apply(ind_rank,2,median)))
    
    if (fix_names)  {
      spl <-
        sapply(avgrank_sort,
               function(z) {
                 split_by_(as.character(z))[1]
               }, USE.NAMES = FALSE)
      # 
      spl <- toupper(spl)
      ids<-which(spl=="MVR")
      spl[ids[1]] <- "PLS"
      spl[ids[2]] <- "PCR"
      # 
      spl <- gsub("NNET","MLP", spl)
      spl <- gsub("SVM","SVR", spl)
      spl <- gsub("CUB","RBR", spl)
      # spl[46] <- "PLS"
      # spl[47] <- "PCR"
      # spl[35] <- "PLS"
    }
    
    ind_rank <- ind_rank[,avgrank_sort]
    ind_rank <- as.data.frame(ind_rank)
    
    x_melted <- melt(ind_rank)
    
    list(x_melted=x_melted,spl=spl)
  }

bp_comb_models_mc0 <-
  function(FRESULTS, fix_names=FALSE) {
    require(reshape2)
    require(ggplot2)
    
    #x <- FRESULTS
    
    err <- sapply(FRESULTS,
                  function(FRESULTS_i) {
                    err_i <-
                      sapply(FRESULTS_i,
                             function(x) {
                               yhat <- x$Combs$C_hat
                               yhat <- as.data.frame(yhat)
                               y <- x$Y
                               y_tr <- x$OTHER$TRAIN_RAW
                               
                               sapply(yhat, function(yh) {
                                 rmse(y, yh)
                                 #mean(mase_cal(y_tr,y,yh), na.rm=TRUE)
                               })
                             })
                    
                    rowMeans(err_i)
                  })
    
    err <- t(err)
    
    ind_rank <- t(apply(err, 1, rank))
    
    avgrank_sort <- names(sort(apply(ind_rank,2,median)))
    
    if (fix_names)  {
      spl <-
        sapply(avgrank_sort,
               function(z) {
                 split_by_(as.character(z))[1]
               }, USE.NAMES = FALSE)
      # 
      spl <- toupper(spl)
      # 
      # spl <- gsub("NNET","MLP", spl)
      # spl <- gsub("GBM","GBR", spl)
      # spl <- gsub("CUB","RBR", spl)
      # spl[46] <- "PLS"
      # spl[47] <- "PCR"
      # spl[35] <- "PLS"
    }
    
    ind_rank <- ind_rank[,avgrank_sort]
    ind_rank <- as.data.frame(ind_rank)
    
    x_melted <- melt(ind_rank)
    
    x_melted
  }


bp_comb_models <-
  function(FRESULTS, fix_names=FALSE) {
    require(reshape2)
    require(ggplot2)
    
    #x <- FRESULTS
    
    err <-
      sapply(FRESULTS,
             function(x) {
               yhat <- x$Combs$C_hat
               yhat <- as.data.frame(yhat)
               y <- x$Y
               y_tr <- x$OTHER$TRAIN_RAW
               
               sapply(yhat, function(yh) {
                 #rmse(y, yh)
                 mean(mase_cal(y_tr,y,yh), na.rm=TRUE)
               })
             })
    
    err <- t(err)
    
    ind_rank <- t(apply(err, 1, rank))
    
    avgrank_sort <- names(sort(apply(ind_rank,2,median)))
    
    if (fix_names)  {
      spl <-
        sapply(avgrank_sort,
               function(z) {
                 split_by_(as.character(z))[1]
               }, USE.NAMES = FALSE)
      # 
      spl <- toupper(spl)
      # 
      # spl <- gsub("NNET","MLP", spl)
      # spl <- gsub("GBM","GBR", spl)
      # spl <- gsub("CUB","RBR", spl)
      # spl[46] <- "PLS"
      # spl[47] <- "PCR"
      # spl[35] <- "PLS"
    }
    
    ind_rank <- ind_rank[,avgrank_sort]
    ind_rank <- as.data.frame(ind_rank)
    
    x_melted <- melt(ind_rank)
    
    p <- ggplot(x_melted, aes(variable, value))
    
    p <- p  +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Rank") +
      theme(axis.text.x  = element_text(size=10,
                                        angle=90)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) #+
    #scale_x_discrete(labels=spl)
    
    if (fix_names) {
      p <- p +
        scale_x_discrete(labels=spl)
    }
    
    p
  }

bp_comb_models_mc <-
  function(FRESULTS, fix_names=FALSE) {
    require(reshape2)
    require(ggplot2)
    
    #x <- FRESULTS
    
    err <- sapply(FRESULTS,
           function(FRESULTS_i) {
             err_i <-
               sapply(FRESULTS_i,
                      function(x) {
                        yhat <- x$Combs$C_hat
                        yhat <- as.data.frame(yhat)
                        y <- x$Y
                        y_tr <- x$OTHER$TRAIN_RAW
                        
                        sapply(yhat, function(yh) {
                          rmse(y, yh)
                          #mean(mase_cal(y_tr,y,yh), na.rm=TRUE)
                        })
                      })
             
             rowMeans(err_i)
           })
    
    err <- t(err)
    
    ind_rank <- t(apply(err, 1, rank))
    
    avgrank_sort <- names(sort(apply(ind_rank,2,median)))
    
    if (fix_names)  {
      spl <-
        sapply(avgrank_sort,
               function(z) {
                 split_by_(as.character(z))[1]
               }, USE.NAMES = FALSE)
      # 
      spl <- toupper(spl)
      # 
      # spl <- gsub("NNET","MLP", spl)
      # spl <- gsub("GBM","GBR", spl)
      # spl <- gsub("CUB","RBR", spl)
      # spl[46] <- "PLS"
      # spl[47] <- "PCR"
      # spl[35] <- "PLS"
    }
    
    ind_rank <- ind_rank[,avgrank_sort]
    ind_rank <- as.data.frame(ind_rank)
    
    x_melted <- melt(ind_rank)
    
    p <- ggplot(x_melted, aes(variable, value))
    
    p <- p  +
      geom_boxplot() +
      theme_minimal() +
      labs(x="",
           y="Rank") +
      theme(axis.text.x  = element_text(size=10,
                                        angle=90)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) #+
    #scale_x_discrete(labels=spl)
    
    if (fix_names) {
      p <- p +
        scale_x_discrete(labels=spl)
    }
    
    p
  }
