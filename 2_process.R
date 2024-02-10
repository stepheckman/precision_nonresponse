
source("0_setup.R")

cat(sprintf("\n\nSTART: 2_process.R\n\n"))

############################################################
# PROCESS RESULTS, CREATE 2 OUTPUT DATASETS EACH FOR CNTS, BINOMIAL, POISSON
# CONTINUOUS RESULTS

cresults <- data.frame()
num.pops <- nrow(params)

for (i in 1:num.pops) {
  
  # population data set from 1_sims
  pop <- readRDS(paste("./continuous/pop", i, ".rds", sep = ""))

  if ("clusterid" %in% colnames(pop)) {
    fit <- lmer(y ~ 1 + (1 | clusterid), pop)  
    icc0 <- as.numeric(icc(fit)[1])
  } else {
    icc0 = 0
  }
  
  # summarize pop to one obs
  p <- pop %>%
    summarise(Y.mean = mean(y), 
              theta = mean(Yvar),
              Y.r.mean = mean(y_r.det, na.rm = TRUE), 
              rr.sto.true = mean(rp)) %>%
    mutate(icc = icc0,
           # true bias in mean is diff between R mean and overall mean -- at pop level
           Bias.mean.det = Y.r.mean - Y.mean, 
           Bias.mean.sto = cov(pop %>% select(c("y","rp")))[1,2] / rr.sto.true)
  
  samps.sum <- readRDS(paste("./continuous/samps_sum", i, ".rds", sep = "")) %>%
    select(-starts_with("rbias")) %>%
    mutate(pop = strtoi(pop),
           # relative bias each sample
           rbias1.mean =  bias1.mean / p$Y.mean * 100,
           rbias3.mean =  bias3.mean / p$Y.mean * 100) 
  
  samps.covars <- cov(samps.sum %>% 
                        select(y, y_r.det, y_nr.det, y_r.sto, y_nr.sto))
                      
  s <- samps.sum  %>%
    mutate(
      # MEANS
      # CIs built from Lee SEs
      uci1.mean.lee = bias1.mean + se1.mean.lee*1.96,
      lci1.mean.lee = bias1.mean - se1.mean.lee*1.96,
      # replication -- JK
      uci1.mean.rep = bias1.mean + se1.mean.rep*1.96,
      lci1.mean.rep = bias1.mean - se1.mean.rep*1.96,
      # replication -- boot
      uci1.mean.boot = bias1.mean + se1.mean.boot*1.96,
      lci1.mean.boot = bias1.mean - se1.mean.boot*1.96,
      # linearized
      uci1.mean.lin = bias1.mean + se1.mean.lin*1.96,
      lci1.mean.lin = bias1.mean - se1.mean.lin*1.96
      ) %>%
    # true value inside CI?
    mutate(#MEANS
           inside.mean.lee1 = if_else(lci1.mean.lee <= p$Bias.mean.det & 
                                   p$Bias.mean.det <= uci1.mean.lee,
                                 1, 0),
           inside.mean.rep1 = if_else(lci1.mean.rep <= p$Bias.mean.det & 
                                   p$Bias.mean.det <= uci1.mean.rep,
                                 1, 0),
           inside.mean.boot1 = if_else(lci1.mean.boot <= p$Bias.mean.det & 
                                        p$Bias.mean.det <= uci1.mean.boot,
                                      1, 0),
           inside.mean.lin1 = if_else(lci1.mean.lin <= p$Bias.mean.det & 
                                   p$Bias.mean.det <= uci1.mean.lin,
                                 1, 0)) %>%
    # summarize all samples for this pop to 1 obs
    summarise(bias1.mean_sd = sd(bias1.mean),
              bias1.mean_mean = mean(bias1.mean),
              bias1.mean_q025 = quantile(bias1.mean, .025),
              bias1.mean_q975 = quantile(bias1.mean, .975),
              bias3.mean_sd = sd(bias3.mean),
              bias3.mean_mean = mean(bias3.mean),
              bias3.mean_q025 = quantile(bias3.mean, .025),
              bias3.mean_q975 = quantile(bias3.mean, .975),
              # repeat with relative bias
              rbias1.mean_mean = mean(rbias1.mean),
              rbias1.mean_sd = sd(rbias1.mean),
              rbias1.mean_q025 = quantile(rbias1.mean, .025),
              rbias1.mean_q975 = quantile(rbias1.mean, .975),
              rbias3.mean_mean = mean(rbias3.mean),
              rbias3.mean_sd = sd(rbias3.mean),
              rbias3.mean_q025 = quantile(rbias3.mean, .025),
              rbias3.mean_q975 = quantile(rbias3.mean, .975),
              rr.det = mean(resp.det),
              rr.sto = mean(resp.stoch),
              # MEANS
              # percent of samples where true bias is inside 95% CI
              cov1.lee_mean = mean(inside.mean.lee1),
              cov1.rep_mean = mean(inside.mean.rep1),
              cov1.boot_mean = mean(inside.mean.boot1),
              cov1.lin_mean = mean(inside.mean.lin1),
              lambda = mean(lambda),
              rho = mean(rho),
              delta = mean(delta),
              # get mean of var.y across all samples
              samp_var.y = mean(var.y)) %>%
    mutate(cov.y.yr = samps.covars[1,2],
           cov.yr.ynr = samps.covars[2,3]) 
  
  cresults <- cresults %>%
    bind_rows(s %>% 
                bind_cols(p) %>% 
                mutate(pop = i))
  
  cat(sprintf("Done with pop %s\n", i))
}

# make pop-method level data set
cresults.long <- cresults %>%
  select(-c(starts_with("cov1"))) %>%
  pivot_longer(c(starts_with("bias1"), starts_with("bias3"), 
                 starts_with("rbias1"), starts_with("rbias3"), ends_with("_mean")),
               names_to = c(".value", ".measure"), 
               # separator between value and method is "_"
               names_sep = "([_])") %>%
  pivot_longer(c(starts_with("bias1"), starts_with("bias3"), 
                 starts_with("rbias1"), starts_with("rbias3")),
               names_to = c(".value", ".var"), 
               # separator between value and method is "."
               names_sep = "([\\.])") %>%
  pivot_longer(c("bias1", "bias3", "rbias1", "rbias3"),
               names_to = c(".value", ".method"), 
               # separator between value and method is between 4th, 5th position
               names_sep = -1) %>%
  mutate(delta.f = fct_reorder(as.factor(case_when(delta == -2 ~ "RR ~ 30%",
                                                   delta == -1 ~ "RR ~ 50%",
                                                   delta == 0 ~ "RR ~ 70%")), delta),
         rho.f = fct_reorder(as.factor(case_when(rho > 0 ~ "Cov(RP, Y) > 0",
                                                 rho == 0 ~ "Cov(RP, Y) = 0")), -rho),
         lambda.f = fct_reorder(as.factor(case_when(lambda == 0 ~ "No clustering", 
                                                    lambda > 0 ~ "Clustering")), lambda),
         theta.f = fct_reorder(as.factor(case_when(theta == 5 ~ "Var(Y) = 5",
                                                   theta == 20 ~ "Var(Y) = 20")), theta),
         nr.method.f = as.factor(if_else(.method == 1, "Deterministic", "Stochastic"))) %>%
  select(-c(".var", ".method"))

table(cresults.long$delta.f, cresults.long$delta)
table(cresults.long$rho.f, cresults.long$rho)
table(cresults.long$lambda.f, cresults.long$lambda)
tabyl(cresults.long, theta.f)

saveRDS(cresults, paste0("cnts_results", date, ".rds"))
saveRDS(cresults.long, paste0("cnts_results_long", date, ".rds"))

cat(sprintf("\nDone with continuous results\n\n", i))


#########################################################
# BINOMIAL RESULTS

bresults <- data.frame()

for (i in 1:num.pops) {
  
  # population data set from 1_sims
  pop <- readRDS(paste0("./binomial/pop", i, ".rds"))
  
  # 0.16 results do not appear in paper
  if(mean(pop$Yvar == 0.25)) {
    
    if ("clusterid" %in% colnames(pop)) {
      fit <- glmer(y ~ 1 + (1 | clusterid), data = pop, family = "binomial")  
      icc0 <- as.numeric(icc(fit)[1])
    } else {
      icc0 = 0
    }
    
    # summarize pop to one obs
    p <- pop %>%
      summarise(Y.mean = mean(y), 
                Y.var = mean(Yvar),
                Y.r.mean = mean(y_r.det, na.rm = TRUE), 
                rr.sto.true = mean(rp)) %>%
      mutate(icc = icc0) %>%
      mutate(# true bias in mean is diff between R mean and overall mean -- at pop level
        Bias.mean.det = Y.r.mean - Y.mean, 
        Bias.mean.sto = cov(pop %>% select(c("y","rp")))[1,2] / rr.sto.true)
    
    samps.sum <- readRDS(paste0("./binomial/samps_sum", i, ".rds")) %>%
      rename(pop.char = pop) %>%
      mutate(pop = strtoi(pop.char),
             # sample level estimate of bias (det and stochastic)
             rbias1.mean = bias1.mean / p$Y.mean * 100,
             rbias3.mean = bias3.mean / p$Y.mean * 100)
    
    samps.covars <- cov(samps.sum %>% 
                          select(y, y_r.det, y_nr.det, y_r.sto, y_nr.sto))
    
    s <- samps.sum  %>%
      select(-c("pop.char")) %>%
      mutate(
        # CIs built from Lee SEs
        uci1.mean.lee = bias1.mean + se1.mean.lee*1.96,
        lci1.mean.lee = bias1.mean - se1.mean.lee*1.96,
        # linearization
        uci1.mean.lin = bias1.mean + se1.mean.lin*1.96,
        lci1.mean.lin = bias1.mean - se1.mean.lin*1.96,
        # bootstrap
        uci1.mean.boot = bias1.mean + se1.mean.boot*1.96,
        lci1.mean.boot = bias1.mean - se1.mean.boot*1.96,
        # replication
        uci1.mean.rep = bias1.mean + se1.mean.rep*1.96,
        lci1.mean.rep = bias1.mean - se1.mean.rep*1.96) %>%
      # true value inside CI?
      mutate(inside.lee1 = if_else(lci1.mean.lee <= p$Bias.mean.det & 
                                     p$Bias.mean.det <= uci1.mean.lee,
                                   1, 0),
             inside.rep1 = if_else(lci1.mean.rep <= p$Bias.mean.det & 
                                     p$Bias.mean.det <= uci1.mean.rep,
                                   1, 0),
             inside.lin1 = if_else(lci1.mean.lin <= p$Bias.mean.det & 
                                     p$Bias.mean.det <= uci1.mean.lin,
                                   1, 0),
             inside.boot1 = if_else(lci1.mean.boot <= p$Bias.mean.det & 
                                     p$Bias.mean.det <= uci1.mean.boot,
                                   1, 0)) %>%
      # summarize all samples for this pop to 1 obs
      # std deviation bias.det across all samples
      summarise(bias1.mean_sd = sd(bias1.mean),
                bias1.mean_mean = mean(bias1.mean),
                bias1.mean_q025 = quantile(bias1.mean, .025),
                bias1.mean_q975 = quantile(bias1.mean, .975),
                bias3.mean_sd = sd(bias3.mean),
                bias3.mean_mean = mean(bias3.mean),
                bias3.mean_q025 = quantile(bias3.mean, .025),
                bias3.mean_q975 = quantile(bias3.mean, .975),
                # repeat with relative bias
                rbias1.mean_mean = mean(rbias1.mean),
                rbias1.mean_sd = sd(rbias1.mean),
                rbias1.mean_q025 = quantile(rbias1.mean, .025),
                rbias1.mean_q975 = quantile(rbias1.mean, .975),
                rbias3.mean_mean = mean(rbias3.mean),
                rbias3.mean_sd = sd(rbias3.mean),
                rbias3.mean_q025 = quantile(rbias3.mean, .025),
                rbias3.mean_q975 = quantile(rbias3.mean, .975),
                rr.det = mean(resp.det),
                rr.sto = mean(resp.stoch),
                # percent of samples where true bias is inside 95% CI
                cov1.mean_lee = mean(inside.lee1),
                cov1.mean_rep = mean(inside.rep1),
                cov1.mean_lin = mean(inside.lin1),
                cov1.mean_boot = mean(inside.boot1),
                lambda = mean(lambda),
                rho = mean(rho),
                delta = mean(delta)) %>%
      mutate(cov.y.yr = samps.covars[1,2],
             cov.yr.ynr = samps.covars[2,3])
    
    bresults <- bresults %>%
      bind_rows(s %>% 
                  bind_cols(p) %>% 
                  mutate(pop = i))
  }
  cat(sprintf("Done with pop %s\n", i))
}

# make pop-method level data set
bresults.long <- bresults %>%
  pivot_longer(c(starts_with("bias1"), starts_with("bias3"), 
                 starts_with("rbias1"), starts_with("rbias3"), ends_with("_mean")),
               names_to = c(".value", ".measure"), 
               # separator between value and method is "_"
               names_sep = "([_])") %>%
  pivot_longer(c(starts_with("bias1"), starts_with("bias3"), 
                 starts_with("rbias1"), starts_with("rbias3")),
               names_to = c(".value", ".var"), 
               # separator between value and method is "."
               names_sep = "([\\.])") %>%
  pivot_longer(c("bias1", "bias3", "rbias1", "rbias3"),
               names_to = c(".value", ".method"), 
               # separator between value and method is between 4th, 5th position
               names_sep = -1) %>%
  select(-c("bias")) %>%
  mutate(delta.f = fct_reorder(as.factor(case_when(delta == -2 ~ "RR ~ 30%",
                                                   #delta == -1 ~ "RR ~ 50%",
                                                   delta == 0 ~ "RR ~ 70%")), delta),
         rho.f = fct_reorder(as.factor(case_when(rho > 0 ~ "Cov(RP, Y) > 0",
                                                 rho == 0 ~ "Cov(RP, Y) = 0")), -rho),
         lambda.f = fct_reorder(as.factor(case_when(lambda == 0 ~ "No clustering", 
                                                    TRUE ~ "Clustering")), lambda),
         Yvar.f = fct_reorder(as.factor(case_when(round(Y.var,2) == 0.25 ~ "Y ~ Binom(1,0.5)",
                                                  TRUE ~ "Y ~ Binom(1,0.8)")), Y.var)) %>%
  pivot_wider(names_from = ".measure", values_from = "rbias") %>%
  mutate(pop_value = Y.mean,
         method.f = as.factor(if_else(.method == 1, "Deterministic", "Stochastic"))) %>%
  select(-c("Y.mean", ".var", ".method"))

table(bresults.long$delta.f, bresults.long$delta)
table(bresults.long$rho.f, bresults.long$rho)
table(bresults.long$lambda.f, bresults.long$lambda)
table(bresults.long$Yvar.f, bresults.long$Y.var)

saveRDS(bresults, paste0("bin_results", date, ".rds"))
saveRDS(bresults.long, paste0("bin_results_long", date, ".rds"))

cat(sprintf("\nDone with binomial results\n\n", i))


#########################################################
# POISSON RESULTS

presults <- data.frame()

for (i in 1:num.pops) {
  
  # population data set from 1_sims
  pop <- readRDS(paste0("./poisson/pop", i, ".rds"))
  
  # other results do not appear in paper
  if(mean(pop$Yvar == 3)) {
    
    if ("clusterid" %in% colnames(pop)) {
      fit <- glmer(y ~ 1 + (1 | clusterid), data = pop, family = "poisson")  
      icc0 <- as.numeric(icc(fit)[1])
    } else {
      icc0 = 0
    }
    
    # summarize pop to one obs
    p <- pop %>%
      summarise(Y.mean = mean(y), 
                Y.var = mean(Yvar),
                Y.r.mean = mean(y_r.det, na.rm = TRUE), 
                rr.sto.true = mean(rp)) %>%
      mutate(icc = icc0) %>%
      mutate(# true bias in mean is diff between R mean and overall mean -- at pop level
        Bias.mean.det = Y.r.mean - Y.mean, 
        Bias.mean.sto = cov(pop %>% select(c("y","rp")))[1,2] / rr.sto.true)
    
    samps.sum <- readRDS(paste0("./poisson/samps_sum", i, ".rds")) %>%
      rename(pop.char = pop) %>%
      mutate(pop = strtoi(pop.char),
             # sample level estimate of bias (det and stochastic)
             rbias1.mean = bias1.mean / p$Y.mean * 100,
             rbias3.mean = bias3.mean / p$Y.mean * 100)
    
    samps.covars <- cov(samps.sum %>% 
                          select(y, y_r.det, y_nr.det, y_r.sto, y_nr.sto))
    
    s <- samps.sum  %>%
      select(-c("pop.char")) %>%
      mutate(
        # CIs built from Lee SEs
        uci1.mean.lee = bias1.mean + se1.mean.lee*1.96,
        lci1.mean.lee = bias1.mean - se1.mean.lee*1.96,
        # linearization
        uci1.mean.lin = bias1.mean + se1.mean.lin*1.96,
        lci1.mean.lin = bias1.mean - se1.mean.lin*1.96,
        # bootstrap
        uci1.mean.boot = bias1.mean + se1.mean.boot*1.96,
        lci1.mean.boot = bias1.mean - se1.mean.boot*1.96,
        # replication
        uci1.mean.rep = bias1.mean + se1.mean.rep*1.96,
        lci1.mean.rep = bias1.mean - se1.mean.rep*1.96) %>% 
      # true value inside CI?
      mutate(inside.lee1 = if_else(lci1.mean.lee <= p$Bias.mean.det & 
                                     p$Bias.mean.det <= uci1.mean.lee,
                                   1, 0),
             inside.rep1 = if_else(lci1.mean.rep <= p$Bias.mean.det & 
                                     p$Bias.mean.det <= uci1.mean.rep,
                                   1, 0),
             inside.lin1 = if_else(lci1.mean.lin <= p$Bias.mean.det & 
                                     p$Bias.mean.det <= uci1.mean.lin,
                                   1, 0),
             inside.boot1 = if_else(lci1.mean.boot <= p$Bias.mean.det & 
                                      p$Bias.mean.det <= uci1.mean.boot,
                                    1, 0)) %>%
      # summarize all samples for this pop to 1 obs
      # std deviation bias.det across all samples
      summarise(bias1.mean_sd = sd(bias1.mean),
                bias1.mean_mean = mean(bias1.mean),
                bias1.mean_q025 = quantile(bias1.mean, .025),
                bias1.mean_q975 = quantile(bias1.mean, .975),
                bias3.mean_sd = sd(bias3.mean),
                bias3.mean_mean = mean(bias3.mean),
                bias3.mean_q025 = quantile(bias3.mean, .025),
                bias3.mean_q975 = quantile(bias3.mean, .975),
                # repeat with relative bias
                rbias1.mean_mean = mean(rbias1.mean),
                rbias1.mean_sd = sd(rbias1.mean),
                rbias1.mean_q025 = quantile(rbias1.mean, .025),
                rbias1.mean_q975 = quantile(rbias1.mean, .975),
                rbias3.mean_mean = mean(rbias3.mean),
                rbias3.mean_sd = sd(rbias3.mean),
                rbias3.mean_q025 = quantile(rbias3.mean, .025),
                rbias3.mean_q975 = quantile(rbias3.mean, .975),
                rr.det = mean(resp.det),
                rr.sto = mean(resp.stoch),
                # percent of samples where true bias is inside 95% CI
                cov1.mean_lee = mean(inside.lee1),
                cov1.mean_rep = mean(inside.rep1),
                cov1.mean_lin = mean(inside.lin1),
                cov1.mean_boot = mean(inside.boot1),
                lambda = mean(lambda),
                rho = mean(rho),
                delta = mean(delta)) %>%
      mutate(cov.y.yr = samps.covars[1,2],
             cov.yr.ynr = samps.covars[2,3])
    
    presults <- presults %>%
      bind_rows(s %>% 
                  bind_cols(p) %>% 
                  mutate(pop = i))
  }
  cat(sprintf("Done with pop %s\n", i))
}

# make pop-method level data set
presults.long <- presults %>%
  pivot_longer(c(starts_with("bias1"), starts_with("bias3"), 
                 starts_with("rbias1"), starts_with("rbias3"), ends_with("_mean")),
               names_to = c(".value", ".measure"), 
               # separator between value and method is "_"
               names_sep = "([_])") %>%
  pivot_longer(c(starts_with("bias1"), starts_with("bias3"), 
                 starts_with("rbias1"), starts_with("rbias3")),
               names_to = c(".value", ".var"), 
               # separator between value and method is "."
               names_sep = "([\\.])") %>%
  pivot_longer(c("bias1", "bias3", "rbias1", "rbias3"),
               names_to = c(".value", ".method"), 
               # separator between value and method is between 4th, 5th position
               names_sep = -1) %>%
  select(-c("bias")) %>%
  mutate(delta.f = fct_reorder(as.factor(case_when(delta == -2 ~ "RR ~ 30%",
                                                   #delta == -1 ~ "RR ~ 50%",
                                                   delta == 0 ~ "RR ~ 70%")), delta),
         rho.f = fct_reorder(as.factor(case_when(rho > 0 ~ "Cov(RP, Y) > 0",
                                                 rho == 0 ~ "Cov(RP, Y) = 0")), -rho),
         lambda.f = fct_reorder(as.factor(case_when(lambda == 0 ~ "No clustering", 
                                                    TRUE ~ "Clustering")), lambda),
         Yvar.f = fct_reorder(as.factor(case_when(round(Y.var,2) == 0.25 ~ "Y ~ Binom(1,0.5)",
                                                  TRUE ~ "Y ~ Binom(1,0.8)")), Y.var)) %>%
  pivot_wider(names_from = ".measure", values_from = "rbias") %>%
  mutate(pop_value = Y.mean,
         method.f = as.factor(if_else(.method == 1, "Deterministic", "Stochastic"))) %>%
  select(-c("Y.mean", ".var", ".method"))

table(presults.long$delta.f, presults.long$delta)
table(presults.long$rho.f, presults.long$rho)
table(presults.long$lambda.f, presults.long$lambda)

saveRDS(presults, paste0("pois_results", date, ".rds"))
saveRDS(presults.long, paste0("pois_results_long", date, ".rds"))

cat(sprintf("\nDone with poisson results\n\n", i))


cresults <- readRDS(paste0("cnts_results", date, ".rds"))
cresults.long <- readRDS(paste0("cnts_results_long", date, ".rds"))
bresults.long <- readRDS(paste0("bin_results_long", date, ".rds"))
presults.long <- readRDS(paste0("pois_results_long", date, ".rds"))

###################################################
# RQ1 -- data sets for graphs in results code

source("2.1_rq1_datasets.R")

###################################################
# RQ2 -- data sets for graphs in results code

source("2.2_rq2_datasets.R")

cat(sprintf("\n\nEND: 2_process.R\n\n"))