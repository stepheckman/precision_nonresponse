# CONTINUOUS

# these 2 pops mentioned in paper
sampdist.a <- readRDS("./continuous/samps_sum11.rds") %>%
  select(bias1.mean, bias3.mean, resp.det, resp.stoch) %>%
  mutate(pop = 9) 
sampdist.b <- readRDS("./continuous/samps_sum13.rds") %>%
  select(bias1.mean, bias3.mean, resp.det, resp.stoch) %>%
  mutate(pop = 3)
sampdist <- sampdist.a %>% 
  bind_rows(sampdist.b) %>%
  left_join(cresults %>%
              select(pop, Y.mean), 
            by="pop") %>%
  mutate(pop.f = as.factor(if_else(pop == "9", "Population A", "Population B")),
         rbias.1 = bias1.mean/Y.mean * 100,
         rbias.3 = bias3.mean/Y.mean * 100) %>%
  select(starts_with("rbias"), starts_with("resp"), pop, pop.f) %>%
  rename(rr.1 = resp.det, 
         rr.3 = resp.stoch) %>%
  pivot_longer(starts_with("rbias") | starts_with("rr"),
               names_to = c(".value", ".method"), 
               names_sep = "([\\.])") %>%
  mutate(method = as.factor(if_else(.method == "1","Deterministic", "Stochastic"))) %>%
  mutate(pm = interaction(pop.f, method, sep = ", ")) %>%
  mutate(pm2 = fct_relevel(pm, "Population A, Stochastic", after = 1))

rbias <- cresults.long %>%
  select(-bias) %>%
  pivot_wider(names_from = ".measure", values_from = "rbias") %>%
  rename(mean.rbias = mean)


# BINOMIAL
bdists <- bresults.long %>%
  filter(method.f == "Deterministic") %>%
  select(rho.f, q025, q975, lambda.f, delta.f, mean, pop_value, Y.var) %>%
  mutate(lo = q025/pop_value,
         hi = q975/pop_value,
         biasr = mean/pop_value) %>%
  mutate(width = hi - lo)


# POISSON

pdists <- presults.long %>%
  filter(method.f == "Deterministic") %>%
  select(rho.f, q025, q975, lambda.f, delta.f, mean, pop_value, Y.var) %>%
  mutate(lo = q025/pop_value,
         hi = q975/pop_value,
         biasr = mean/pop_value) %>%
  mutate(width = hi - lo)

saveRDS(sampdist, paste0("cnts_sampdist", date, ".rds"))
saveRDS(rbias, paste0("cnts_rbias", date, ".rds"))
saveRDS(bdists, paste0("bin_rbias", date, ".rds"))
saveRDS(pdists, paste0("pois_rbias", date, ".rds"))

