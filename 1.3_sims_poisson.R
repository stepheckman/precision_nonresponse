#################################################################
# Poisson Y (Count)
#################################################################

source("1.3_functions_poisson.R")

# cov(Y,Z) Z becomes RP
rhos <- c(0, 0.15) 

# mean RP -- controls RR
deltas <- c(-2, 0)

# variability in cluster means
lambdas <- c(0, 0.6)

# variance in Y (mean = variance for Poisson)
yvars <- c(3, 5)

params <- expand.grid(rho=rhos, delta=deltas, lambda=lambdas, yvar = yvars)
params$i <- row.names(params)
mcmapply(loop, 
         # passed to loop function
         rho = params$rho, 
         delta = params$delta, 
         lambda = params$lambda, 
         yvar = params$yvar, 
         i = params$i, 
         # num of cores
         mc.cores = num.cores)


cat(sprintf("Done with poisson\n"))