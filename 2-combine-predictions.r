load("BaseResults.rdata")

source("src/agg-functions.r")
source("src/utils.r")
source("src/ensemble-specs.r")
source("src/combination-workflow.r")

library(opera)

nreps <- length(RESULTS[[1]])

IDS <- 1:90
FINALRESULTS <- vector("list", length(RESULTS))
for (i in IDS) {
  cat(i, "\n\n\n\n\n\n")
  results_i <- RESULTS[[i]]
  
  ITER_RESULT <- vector("list", nreps)
  for (it in 1:nreps) {
    fcomb <- 
      get_final_predictions(RESULTX = results_i[[it]],
                            specs = MODELSPECS)
    
    ITER_RESULT[[it]] <- fcomb
  }
  
  FINALRESULTS[[i]] <- ITER_RESULT
  
  SIGNATURE <- paste0("F",IDS[1],"_",IDS[length(IDS)])
  save(FINALRESULTS, file = paste0("KD_COMB_30M_2611_FALL_",SIGNATURE,".rdata"))
}
