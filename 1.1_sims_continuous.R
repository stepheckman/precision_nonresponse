# CREATE POPS AND SELECT ALL SAMPLES 
# METHODS:
# 1 -- deterministic RP, sample ybar used for pop
# 3 -- stochastic RP, sample ybar used for pop
# 2 versions of Lee method -- stochastic/deterministic


source("0_setup.R")

ymean <- 10

source("1.1_functions_cntns.R")

mcmapply(loop, 
         # passed to loop function
         rho = params$rho, 
         delta = params$delta, 
         lambda = params$lambda, 
         yvar = params$yvar, 
         i = params$i, 
         mc.cores = num.cores)
 
# save workspace
#save.image(file=paste0("cnts_results_", date, ".RData"))
