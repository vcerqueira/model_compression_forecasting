load("/Users/vcerqueira/Desktop/resutls_2812.rdata")
load("/Users/vcerqueira/Desktop/KD_ARIMA.rdata")

#FINALRESULTS <- x

source("src/bayes-analysis-methods.r")
source("src/utils.r")
source("src/plots.r")
source("src/analysis-workflow.r")

require(Metrics)
require(tsensembler)
require(reshape2)
require(ggplot2)

#### EDA BASE MODELS
load("/Users/vcerqueira/Desktop/rank_dist_plots0.rdata")

RankDistInd <- bp_ind_models_mc(FINALRESULTS, fix_names = TRUE)
RankDistComb <- bp_comb_models_mc(FRESULTS = FINALRESULTS, fix_names = FALSE)

RankDistInd0 <- bp_ind_models_mc0(FINALRESULTS, fix_names = TRUE)
RankDistComb0 <- bp_comb_models_mc0(FRESULTS = FINALRESULTS, fix_names = FALSE)

x <- RankDistComb0
ids<-which(RankDistComb0$variable == "ORACLE")
x <- x[-ids,]
x$variable <- gsub("_TT$","",x$variable)
x$variable <- gsub("_TF$","",x$variable)
x$variable <- gsub("_SQR$","",x$variable)
x$variable <- factor(x$variable, levels = nms)

library(reshape2)
nms <- names(sort(sapply(split(x,x$variable), function(z) median(z$value))))

p <- ggplot(x, aes(variable, value))

p  +
  geom_boxplot() +
  theme_minimal() +
  labs(x="",
       y="Rank") +
  theme(axis.text.x  = element_text(size=10,
                                    angle=90)) +
  theme(axis.text.y  = element_text(size = 12),
        axis.title.y = element_text(size = 12)) #+


#### BASE MODELS IN ORIGINAL DATA V DISTILLED

A <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, all_res)
           
           rowMeans(err)
         })

sort(round(rowMeans(apply(A,2,rank)),2))

kd_base <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_base,
                    teacher = "St.Stacking",
                    students = "StudentsTR")
           
           rowMeans(err)
         })

# kd_base <- sapply(FINALRESULTS, kd_analysis_base, 
#                   teacher = "St.ADE",
#                   students="StudentsTR")
kd_base <- t(kd_base)
kd_base <- as.data.frame(kd_base)
kd_base <- kd_base[complete.cases(kd_base),]

cn <- colnames(kd_base)

spl <- make_names(cn)
spl0 <- spl <- gsub("ST.","", spl)
for (unique_name in unique(spl)) {
  spl[spl == unique_name] <- 
    paste0(spl[spl == unique_name], 
           1:length(spl[spl == unique_name]))
}


kd_bayessign_base <-
  sapply(kd_base,
         function(x) {
           BA <-
             BayesianSignTest(
               diffVector = x,#err_base - err_ca,
               rope_min = -1,
               rope_max = 1
             )
           
           BA <- unlist(BA)
           names(BA) <- c("ST wins", "draw", "ST loses")
           
           print(BA)
           BA
         }) 

spl0[24] <- "PLS"
spl0[25] <- "PCR"
spl0 <- gsub("NNET", "MLP", spl0)
spl0 <- gsub("CUB", "RBR", spl0)
spl0 <- gsub("SVM", "SVR", spl0)
# kd_bayesrank_plot(kd_bayesrank_base) + 
#   scale_x_discrete(labels=spl)
kd_bayesrank_plot(kd_bayessign_base) + 
  scale_x_discrete(labels=spl0)
#BayesSign_IndPaired_1.pdf

### PERC DIFF BOXPLOT
kd_baseNOOUT <- 
  lapply(kd_base,
         function(x) {
           outs <- x %in% boxplot.stats(x)$out
           x[outs] <- NA
           x
         })

box_plot3(x = kd_baseNOOUT, take_logs = FALSE)
box_plot3(x = kd_base, take_logs = TRUE)
#PD_IndPaired.pdf
## RANKS IND BY TEACHER


kd_ind_err <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_err_ind,
                    teacher = "St.Stacking",
                    students = "StudentsTR")
           
           rowMeans(err)
         })

# kd_ind_err <- sapply(FINALRESULTS, kd_analysis_err_ind, 
#                      teacher="St.ADE",
#                      students="StudentsTR")
kd_ind_err <- t(kd_ind_err)

ranks <- apply(kd_ind_err,1,rank)
nms <- make_names(names(sort(rowMeans(ranks))))
nms[21] <- "ST.PLS"
nms[22] <- "ST.PCR"
nms <- gsub("NNET", "MLP", nms)
nms <- gsub("CUB", "RBR", nms)
nms <- gsub("SVM", "SVR", nms)

avg_rank_plot(rowMeans(ranks), 
              apply(ranks,1,sd),col0 = "darkorange3") + 
  theme(axis.text.x  = element_text(angle = 90,
                                    size = 10)) + 
  scale_x_discrete(labels=nms)
#AvgRankDistInd.pdf

## RANKS IND ORIGINAL DATA

kd_ind_err_ori <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_err_ori)
           
           rowMeans(err)
         })

# kd_ind_err_ori <- sapply(FINALRESULTS, 
#                          kd_analysis_err_ori)
kd_ind_err_ori <- t(kd_ind_err_ori)

ranks <- apply(kd_ind_err_ori,1,rank)

nms <- make_names(names(sort(rowMeans(ranks))))
nms[14] <- "PLS"
nms[16] <- "PCR"
nms <- gsub("NNET", "MLP", nms)
nms <- gsub("CUB", "RBR", nms)
nms <- gsub("SVM", "SVR", nms)
avg_rank_plot(rowMeans(ranks), apply(ranks,1,sd)) + 
  theme(axis.text.x  = element_text(angle = 90,
                                    size = 10)) + 
  scale_x_discrete(labels=nms) 

## RANKS 1V1
# ? 
cerr <- cbind(kd_ind_err_ori, kd_ind_err)

ranks <- apply(cerr,1,rank)
avgrank <- rowMeans(ranks)
sdevrank <- apply(ranks,1,sd)

nms <- make_names(names(avgrank))
nms[24] <- "PLS"
nms[25] <- "PCR"
nms[54] <- "ST.PLS"
nms[55] <- "ST.PCR"
nms <- gsub("NNET", "MLP", nms)
nms <- gsub("CUB", "RBR", nms)
nms <- gsub("SVM", "SVR", nms)

ord <- order(avgrank)[1:30]

nms <- nms[ord]

avg_rank_plot(avg = avgrank[ord], 
              sdev = sdevrank[ord],
              col0="#33CCCC",
              col_per_st = T) +
  theme(axis.text.x  = element_text(angle = 90,
                                    size = 10)) + 
  scale_x_discrete(labels=nms) 
#AvgRankMergedInd.pdf

## RANKS COMB

kd_ind_comb <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_err_comb)
           
           rowMeans(err)
         })

# kd_ind_comb <- sapply(FINALRESULTS, kd_analysis_err_comb)
kd_ind_comb <- t(kd_ind_comb)
kd_ind_comb <- kd_ind_comb[,-3]

ranks <- apply(kd_ind_comb,1,rank)
avg_rank_plot(rowMeans(ranks), apply(ranks,1,sd)) + 
  theme(axis.text.x  = element_text(angle = 90, size = 11))


##### COMB WITH ORIGINAL VS COMB WITH DIST

kd_comb <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_comb,
                    student="St.cub_25it0_nn",
                    students="StudentsTR")
           
           rowMeans(err)
         })

# kd_comb <- sapply(FINALRESULTS, kd_analysis_comb,
#                   student="St.cub_25it0_nn",
#                   students="StudentsTR")
kd_comb <- t(kd_comb)
kd_comb <- as.data.frame(kd_comb)
kd_comb <- kd_comb[complete.cases(kd_comb),]

###
# 
# #all.equal(rownames(kd_ind_err),rownames(kd_ind_comb))
# colnames(kd_ind_comb)
# kd_ind_err0 <- kd_ind_err <- kd_ind_err
# colnames(kd_ind_err) <- spl
# 
# err_merged <- cbind(kd_ind_err, kd_ind_comb)
# ranks <- apply(err_merged,1,rank)
# 
# avgrank <- rowMeans(ranks)
# sdevrank <- sd(ranks)
# 
# ord <- order(avgrank)[1:30]
# 
# avg_rank_plot(avg = rowMeans(ranks)[ord], 
#               sdev = apply(ranks,1,sd)[ord], 
#               col_per_st = T)# + 
#   #theme(axis.text.x  = element_text(angle = 90,
#    #                                 size = 11))

kd_comb_avg <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_comb_avg,
                    student="St.cub_25it0_nn",
                    students="StudentsTR")
           
           rowMeans(err)
         })

# kd_comb_avg <- sapply(FINALRESULTS, 
#                       kd_analysis_comb_avg,
#                       student="St.cub_25it0_nn",
#                       students="StudentsTR")

kd_comb_avg <- kd_comb_avg[-c(3,23),]
rownames(kd_comb_avg) <- 
  gsub("St\\.","ST\\.", rownames(kd_comb_avg))
rownames(kd_comb_avg) <- 
  gsub("_ae","", rownames(kd_comb_avg))
rownames(kd_comb_avg) <- 
  gsub("_TT","", rownames(kd_comb_avg))
rownames(kd_comb_avg) <- 
  gsub("_TF","", rownames(kd_comb_avg))
rownames(kd_comb_avg) <- 
  gsub("_SQR","", rownames(kd_comb_avg))

ranks <- apply(kd_comb_avg,2,rank)
ord <- order(rowMeans(ranks))

avg_rank_plot(avg = rowMeans(ranks)[ord], 
              sdev = apply(ranks,1,sd)[ord], 
              col_per_st = TRUE) + 
  theme(axis.text.x  = element_text(angle = 90,
                                    size = 12))

##### COMB WITH ORIGINAL VS best dist

kd_bestvcomb <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_bestVcomb,
                    student="St.cub_25it0_nn",
                    students="StudentsTR",
                    teacher="St.Stacking")
           
           rowMeans(err)
         })

# kd_bestvcomb <- sapply(FINALRESULTS, kd_analysis_bestVcomb,
#                        student="St.cub_25it0_nn",
#                        students="StudentsTR",
#                        teacher="St.Simple")
kd_bestvcomb <- t(kd_bestvcomb)
kd_bestvcomb <- as.data.frame(kd_bestvcomb)
kd_bestvcomb <- kd_bestvcomb[complete.cases(kd_bestvcomb),]

kd_bestvcomb_BS <-
  sapply(kd_bestvcomb,
         function(x) {
           x1<<-x#<-x1
           BA <-
             BayesianSignTest(
               diffVector = x,#err_base - err_ca,
               rope_min = -1,
               rope_max = 1
             )
           
           BA <- unlist(BA)
           names(BA) <- c("ST wins", "draw", "ST loses")
           print(BA)
           
           BA
         }) 

kd_bestvcomb_BS <- kd_bestvcomb_BS[,-3]

colnames(kd_bestvcomb_BS) <- 
  gsub("_TT","", colnames(kd_bestvcomb_BS))
colnames(kd_bestvcomb_BS) <- 
  gsub("_TF","", colnames(kd_bestvcomb_BS))
colnames(kd_bestvcomb_BS) <- 
  gsub("_SQR","", colnames(kd_bestvcomb_BS))

kd_bestvcomb_BS <- kd_bestvcomb_BS[,order(colnames(kd_bestvcomb_BS))]

kd_bayesrank_plot(kd_bestvcomb_BS) 



## BAYES

# kd_bayesrank_comb <-
#   sapply(kd_comb,
#          function(x) {
#            BA <-
#              BayesianSignedRank(
#                diffVector = x,#err_base - err_ca,
#                rope_min = -1,
#                rope_max = 1
#              )
#            
#            BA <- unlist(BA)
#            names(BA) <- c("ST wins", "draw", "ST loses")
#            print(BA)
#            
#            BA
#          }) 

kd_bayessign_comb <-
  sapply(kd_comb,
         function(x) {
           BA <-
             BayesianSignTest(
               diffVector = x,#err_base - err_ca,
               rope_min = -1,
               rope_max = 1
             )
           
           BA <- unlist(BA)
           names(BA) <- c("ST wins", "draw", "ST loses")
           print(BA)
           
           BA
         }) 


#kd_bayesrank_plot(kd_bayesrank_comb)

kd_bayessign_comb <- kd_bayessign_comb[,-3]


colnames(kd_bayessign_comb) <- 
  gsub("_TT","", colnames(kd_bayessign_comb))
colnames(kd_bayessign_comb) <- 
  gsub("_TF","", colnames(kd_bayessign_comb))
colnames(kd_bayessign_comb) <- 
  gsub("_SQR","", colnames(kd_bayessign_comb))

kd_bayessign_comb <- kd_bayessign_comb[,order(colnames(kd_bestvcomb_BS))]

kd_bayessign_comb <- kd_bayessign_comb[,order(colnames(kd_bayessign_comb))]
nms <- gsub("St\\.","",colnames(kd_bayessign_comb))
kd_bayesrank_plot(kd_bayessign_comb) + 
  scale_x_discrete(labels=nms) 


### PERC DIFF BOXPLOT

kd_comb_noOUT <-
  lapply(kd_comb,
         function(x) {
           outs <- x %in% boxplot.stats(x)$out
           x[outs] <- NA
           x
         })

kd_comb_noOUT <- as.data.frame(kd_comb_noOUT)

colnames(kd_comb) <- gsub("St\\.","", colnames(kd_comb))
box_plot3(x = kd_comb_noOUT, take_logs = FALSE)
box_plot3(x = kd_comb, take_logs = TRUE)



###
##### tr v vld

kd_combVcomb <- 
  sapply(FINALRESULTS,
         function(x) {
           err <-
             sapply(x, kd_analysis_combVcomb,
                    student = "St.cub_25it0_nn")
           
           rowMeans(err)
         })

load("/Users/vcerqueira/Desktop/TR_V_VL_.rdata")
kd_combVcomb <- t(kd_combVcomb)
kd_combVcomb <- as.data.frame(kd_combVcomb)
kd_combVcomb <- kd_combVcomb[complete.cases(kd_combVcomb),]
colnames(kd_combVcomb) <-
  gsub("^St\\.", "",colnames(kd_combVcomb))
colnames(kd_combVcomb) <-
  gsub("_TT", "",colnames(kd_combVcomb))
colnames(kd_combVcomb) <-
  gsub("_TF", "",colnames(kd_combVcomb))
colnames(kd_combVcomb) <-
  gsub("_SQR", "",colnames(kd_combVcomb))
kd_combVcomb <- kd_combVcomb[,-3]
kd_combVcomb <- kd_combVcomb[,order(colnames(kd_combVcomb))]

box_plot3(x = kd_combVcomb, take_logs = TRUE)



######### VS SOTA

#load("/Users/vcerqueira/Desktop/KD_ARIMA.rdata")
colnames(kd_comb_avg)

#FINALRESULTS1 <- FINALRESULTS
#
names(ARIMAF) <- paste0("TS",1:length(ARIMAF))#names(FINALRESULTS1)
#length(FINALRESULTS1)

FR <- vector("list", 90)
for (i in 1:90) {
  cat("i:", i,"\n")
  #i<-1
  if (length(FINALRESULTS[[i]]) != 10) {
    next
  }
  
  if (length(ARIMAF[[i]]) != 10) {
    next
  }
  
  for (j in 1:10) {
    cat("j:", j,"\n")
    #j<-1
    y <- FINALRESULTS[[i]][[j]]$OTHER$Y_TEST
    y_tr <- FINALRESULTS[[i]][[j]]$OTHER$TRAIN_RAW
    
    yhat_arima <- as.data.frame(ARIMAF[[i]][[j]]$YHAT)
    
    # St.Simple <- 
    #   FINALRESULTS1[[i]]$Combs$StudentsTR$St.Simple$St.cub_25it0_nn
    # 
    # ADE <- FINALRESULTS1[[i]]$Combs$C_hat$ADE
    # Best <- FINALRESULTS1[[i]]$Combs$C_hat$Best
    # yhatl2 <- list(ADE=ADE,Best=Best,ST.Simple=St.Simple)
    
    #Yhat_base <- FINALRESULTS1[[i]]$Combs$Y_hat
    Yhat_base <- FINALRESULTS[[i]][[j]]$Combs$StudentsTR$St.Stacking
    
    yhatl <- cbind(Yhat_base,yhat_arima)
    yhatl <- as.data.frame(yhatl)
    #head(yhatl)
    
    err <- sapply(yhatl,
                  function(yh) {
                    #rmse(y, yh)
                    mean(mase_cal(y_tr,y,yh), na.rm=TRUE)
                  })
    
    FR[[i]] <- err
  }
}

load("/Users/vcerqueira/Desktop/MC_v_ARIMA_summary.rdata")

FR1 <- FR[!sapply(FR,is.null)]


FRb <- do.call(rbind, FR1)

FRb <- as.data.frame(FRb)
colnames(FRb)
FRF <- FRb[,c(30:33)]
colnames(FRF)[1] <- "ST.Stacking"

FRF <- lapply(FRF,
              function(x) {
                percentual_difference(FRF$ST.Stacking, x)
              })

FRF <- as.data.frame(FRF[-1])
head(FRF)


B_frf <-
  sapply(FRF,
         function(x) {
           BA <-
             BayesianSignTest(
               diffVector = x,#err_base - err_ca,
               rope_min = -1,
               rope_max = 1
             )
           
           BA <- unlist(BA)
           names(BA) <- c("ST.Stacking wins", "draw", "ST.Stacking loses")
           print(BA)
           
           BA
         }) 

kd_bayesrank_plot(B_frf)

avgrank <- rowMeans(apply(FRb, 1, rank))
sdevrank <- apply(apply(FRb, 1, rank),1,sd)
# sapply(1:89,
#        function(i) {
#          #i<-82
#          x.a <- as.vector(ARIMAF[[i]]$Y)
#          x.b <- as.vector(FINALRESULTS1[[i]]$Y)
#          #mean(as.vector(x.a$Y) - x.b$Y)
#          all.equal(x.a, x.b)
#        })

nms <- make_names(names(avgrank))
nms[24] <- "ST.PLS"
nms[25] <- "ST.PCR"
nms <- gsub("NNET", "MLP", nms)
nms <- gsub("CUB", "RBR", nms)
nms <- gsub("SVM", "SVR", nms)

ord <- order(avgrank)[1:33]
nms <- nms[ord]

avg_rank_plot2(avg = avgrank[ord], 
               sdev = sdevrank[ord],
               col0="#33CCCC",
               col_per_trad = T) +
  theme(axis.text.x  = element_text(angle = 90,
                                    size = 11)) + 
  scale_x_discrete(labels=nms) 


###



############
#### CC ####
############

comp_cost <- 
  lapply(FINALRESULTS,
         function(x) {
           
           ATTRS <- attributes(x$Combs$Y_hat)
           
           tot_time <- sum(ATTRS$Times)
           m5_time <- ATTRS$Times["cub_25it0_nn"]
           
           ratio_m5_time <- unname((m5_time/tot_time) * 100)
           
           sizes <- x$OTHER$MODELSIZE
           
           tot_size <- sum(sizes)
           m5_size <- sizes["cub_25it0_nn"]
           
           ratio_m5_size <- unname((m5_size/tot_size) * 100)
           
           m5_size_mb <- unname(sizes["cub_25it0_nn"]/ (1024^2))
           
           c(ratio_m5_time=ratio_m5_time,
             ratio_m5_size=ratio_m5_size,
             m5_size_mb=m5_size_mb)
         })

comp_cost <- do.call(rbind, comp_cost)
head(comp_cost)
boxplot(comp_cost$size)

comp_cost <- as.data.frame(comp_cost)

require(reshape2)
require(ggplot2)

colnames(comp_cost) <- c("Time","Space","Size(Mb)")
comp_costm <- melt(as.data.frame(comp_cost))

ggplot(comp_costm, aes(x = 1, y = value)) +
  facet_wrap(~ variable,
             nrow = 1,
             scales = "free") + 
  geom_boxplot() +
  #geom_hline(yintercept = 0, col = "red") +
  theme_bw() +
  labs(x = "",
       y = "% Relative to Simple") +
  theme(axis.text.x  = element_blank()) +
  theme(
    axis.text.y  = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text.x = element_text(size = 12)
  )

