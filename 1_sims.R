source("0_setup.R")

cat(sprintf("\n\nSTART: 1_sims.R\n\n"))

source("1.1_sims_continuous.R")
source("1.2_sims_binomial.R")
source("1.3_sims_poisson.R")

cat(sprintf("\n\nEND: 1_sims.R\n\n"))