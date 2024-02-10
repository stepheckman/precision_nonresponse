source("0_setup.R")

# creates 3 rds files for each pop:
# pop - poulation file, case level; n = 1,000,000
# samps_pop - all samples from pop, case level; n = 1,000 * sims
# samps_sum - summary of each sample, sample level; n = sims
source("1_sims.R")
# run on multicore machine

# then go to Stata and run linearized standard errors (continuous only)
# for this line to work, set StataPath and StataVersion in .Rprofile
RStata("./stata/lin_stderr.do")

# process simulation outputs
source("2_process.R")
# can run on desktop

# run SE estimation methods on LISS and GSS data sets
source("3_examples.R")
# can run on desktop

# create graphs
source("4_results.R")
# can run on desktop

# then knit for_paper.Rmd
# creates html file with numbers needed for paper
#knit("for_paper.Rmd")