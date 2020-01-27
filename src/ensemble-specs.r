require(tsensembler)

nall_kernels <- c("polydot","laplacedot","rbfdot")#,
base_predictors2 <- c("bm_mars","bm_ppr","bm_svr","bm_glm",
                      "bm_gaussianprocess", "bm_randomforest",
                      "bm_pls_pcr","bm_ffnn","bm_cubist")


pars_predictors2 <- list(bm_gaussianprocess = list(kernel = nall_kernels, tol = c(.001)),
                         bm_svr = list(kernel = nall_kernels, C=c(1), epsilon=c(.1)),
                         bm_ppr = list(nterms = c(2,4,6),
                                       sm.method = c("supsmu","gcvspline")),
                         bm_mars = list(degree = c(1, 3), nk = c(10,20,30), 
                                        thresh=c(0.001),
                                        pmethod=c("forward")),
                         bm_ffnn = list(hidden1 = c(5,7,10),
                                        hidden2 = c(0)),
                         bm_glm = list(alpha = c(0,.5,1),
                                       family = c("gaussian")),
                         bm_randomforest = list(num.trees = c(500),
                                                mtry = c(5,10)),
                         bm_pls_pcr = list(method = c("simpls","svdpc")),
                         bm_cubist  = list(committees= c(1,25)))

MODELSPECS <- model_specs(base_predictors2,pars_predictors2)
MODELSPECS