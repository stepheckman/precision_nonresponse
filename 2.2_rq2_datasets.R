# CONTINUOUS

coverage <- cresults %>%
  select(-starts_with("bias"), -starts_with("rbias"), -starts_with("cov.y")) %>%
  pivot_longer(starts_with("cov"),
               names_to = c(".value", ".stat"),
               # separator between value and method is "_"
               names_sep = "([_])") %>%
  pivot_longer(starts_with("cov"),
               names_to = c(".value", ".varmethod"),
               # separator between value and method is "_"
               names_sep = "([.])") %>%
  pivot_longer(starts_with("cov"),
               names_to = c(".value", ".method"),
               # separator between value and method is "_"
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
         nr.method.f = as.factor(if_else(.method == 1, "Deterministic", "Stochastic")),
         method.f = fct_relevel(as.factor(case_when(.varmethod == "lee" ~ "Lee",
                                                    .varmethod == "rep" ~ "Replication",
                                                    .varmethod == "boot" ~ "Bootstrap",
                                                    .varmethod == "lin" ~ "Linearization")),
                                "Lee", "Replication", "Linearization"),
         type.f = as.factor(if_else(.stat == "mean", "Mean", "Median")))


# BINOMIAL

cov.bin <- bresults.long %>%
  filter(method.f == "Deterministic") %>%
  pivot_longer(starts_with("cov1"),
               names_to = c(".value", ".varmethod"),
               # separator between value and method is "_"
               names_sep = "([_])") %>%
  pivot_longer(starts_with("cov1"),
               names_to = c(".value", ".stat"),
               # separator between value and method is "."
               names_sep = "([.])") %>%
  pivot_longer(starts_with("cov1"),
               names_to = c(".value", ".method"),
               names_sep = -1) %>%
  mutate(SE.method.f = fct_relevel(as.factor(case_when(.varmethod == "lee" ~ "Lee",
                                                       .varmethod == "rep" ~ "Replication",
                                                       .varmethod == "boot" ~ "Bootstrap",
                                                       .varmethod == "lin" ~ "Linearization")),
                                   "Lee", "Replication", "Linearization"),
         rho.ff = factor(rho.f, levels = c("Cov(RP, Y) > 0", 
                                           "Cov(RP, Y) = 0")),
         lambda.f = fct_reorder(as.factor(case_when(lambda == 0 ~ "No clustering", 
                                                    lambda > 0 ~ "Clustering")), lambda)) %>%
  select(c(cov, SE.method.f, lambda.f, rho.f, delta.f, rho.ff))

# check range of cov values for axis
summary(cov.bin$cov)


# POISSON

cov.pois <- presults.long %>%
  filter(method.f == "Deterministic") %>%
  pivot_longer(starts_with("cov1"),
               names_to = c(".value", ".varmethod"),
               # separator between value and method is "_"
               names_sep = "([_])") %>%
  pivot_longer(starts_with("cov1"),
               names_to = c(".value", ".stat"),
               # separator between value and method is "_"
               names_sep = "([.])") %>%
  pivot_longer(starts_with("cov1"),
               names_to = c(".value", ".method"),
               # separator between value and method is "_"
               names_sep = -1) %>%
  mutate(SE.method.f = fct_relevel(as.factor(case_when(.varmethod == "lee" ~ "Lee",
                                                       .varmethod == "rep" ~ "Replication",
                                                       .varmethod == "boot" ~ "Bootstrap",
                                                       .varmethod == "lin" ~ "Linearization")),
                                   "Lee", "Replication", "Linearization"),
         rho.ff = factor(rho.f, levels = c("Cov(RP, Y) > 0", 
                                           "Cov(RP, Y) = 0")),
         lambda.f = fct_reorder(as.factor(case_when(lambda == 0 ~ "No clustering", 
                                                    lambda > 0 ~ "Clustering")), lambda)) %>%
  select(c(cov, SE.method.f, lambda.f, rho.f, delta.f, rho.ff))


# check range of cov values for axis
summary(cov.pois$cov)

saveRDS(coverage, paste0("cnts_coverage", date, ".rds"))
saveRDS(cov.bin, paste0("bin_coverage", date, ".rds"))
saveRDS(cov.pois, paste0("pois_coverage", date, ".rds"))


