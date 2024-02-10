source("0_setup.R")

source("1.2_functions_binom.R")

# cov(Y,Z) Z becomes RP
rhos <- c(0, 0.3) 

# mean RP -- controls RR
deltas <- c(-2, 0)

# variability in cluster means -- controls ICC
# For binary outcome Y, this is variation around binom_p values
lambdas <- c(0, 0.10)

# specify population proportion
binom_ps <- c(0.5, 0.8)

params <- expand.grid(rho=rhos, delta=deltas, lambda=lambdas, binom_p=binom_ps)
params$i <- row.names(params)
mcmapply(loop, 
         # passed to loop function
         rho = params$rho, 
         delta = params$delta, 
         lambda = params$lambda, 
         binom_p = params$binom_p,
         i = params$i, 
         # num of cores
         mc.cores = num.cores)


cat(sprintf("Done with binomial\n"))
