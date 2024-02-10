
source("0_setup.R")

cat(sprintf("\n\nSTART: 4_results.R\n\n"))

sampdist <- readRDS(paste0("cnts_sampdist", date, ".rds"))
rbias <- readRDS(paste0("cnts_rbias", date, ".rds"))
bdists <- readRDS(paste0("bin_rbias", date, ".rds"))
pdists <- readRDS(paste0("pois_rbias", date, ".rds"))

coverage <- readRDS(paste0("cnts_coverage", date, ".rds"))
cov.bin <- readRDS(paste0("bin_coverage", date, ".rds"))
cov.pois <- readRDS(paste0("pois_coverage", date, ".rds"))

pt.size   <- 2 # size of symbols 
text.size <- 7  # size of text from facet wrap label


############################################################
# RQ1

# CONTINUOUS RESULTS

# graph example sampling dist for 2 test pops
density.AB <- sampdist %>%
  ggplot(aes(x=rbias, linetype=method)) +
  geom_density(alpha=0.6) +
  theme_bw() +
  ylab("Density") +
  xlab("Relative Nonresponse Bias (in Percent)") + 
  labs(linetype = "Estimate") +
  facet_wrap(~pop.f) + 
  theme_light() +
  theme(aspect.ratio = 1.5)

# graph 95% of distribution of NR bias estimates, all pops
rq1.gph <- rbias %>%
  filter(nr.method.f == "Deterministic") %>%
  ggplot(aes(y=rho.f, x=mean.rbias)) + 
  #scale_y_discrete(limits = rev(levels(results.long$rho.f))) +
  geom_point() +
  geom_errorbarh(aes(xmin=q025, xmax=q975)) +
  facet_grid(theta.f + lambda.f ~ delta.f, 
             labeller = labeller(lambda.f = label_wrap_gen(8))) + 
  # rbias already expressed as pct (see 1.1 code)
  xlab("Relative Nonresponse Bias (in Percent)") + 
  ylab("") + 
  theme_light() +
  theme(aspect.ratio = .81)



# Binomial 0.5 #############################
rq1.b5 <- bdists %>%
  filter(round(Y.var,2) == 0.25) %>%
  ggplot(aes(y=rho.f, x=biasr)) + 
  geom_point() +
  geom_errorbarh(aes(xmin=lo, xmax=hi)) +
  facet_grid(lambda.f ~ delta.f, 
             labeller = labeller(lambda.f = label_wrap_gen(8))) + 
  labs(x="", y="", title="Bernoulli, p = 0.5") + 
  theme_light() 


# Poisson 3 #############################
rq1.p3 <- pdists %>%
  filter(round(Y.var,2) == 3) %>%
  ggplot(aes(y=rho.f, x=biasr)) + 
  geom_point() +
  geom_errorbarh(aes(xmin=lo, xmax=hi)) +
  facet_grid(lambda.f ~ delta.f, 
             labeller = labeller(lambda.f = label_wrap_gen(8))) + 
  labs(x="Relative Nonresponse Bias (in Percent)", 
       title=expression("Poisson," ~ omega ~ "= 3"),
       y="") +
  theme_light()

rq1.bp <- rq1.b5 / rq1.p3



############################################################
# COVERAGE RESULTS

# CONTINUOUS

# graph coverage rates for lee / rep / lin methods
cov.gph <- coverage %>%
  filter(nr.method.f == "Deterministic" & 
           !is.na(cov) & 
           method.f != "Bootstrap" &
           type.f == "Mean") %>%
  ggplot(aes(y=rho.f, x=cov, shape = delta.f)) +
  geom_point() +
  scale_shape_discrete(solid=F,
                       name = "") +
  facet_grid(theta.f + lambda.f ~ method.f, 
             labeller = labeller(lambda.f = label_wrap_gen(8))) +
  scale_x_continuous(limits = c(0.92, 1),
                     breaks=c(0.92,0.95,0.98),
                     labels=c("92", "95", "98")) +
  xlab("Coverage Rate (in Percent)") + 
  ylab("") +
  labs(linetype = "") + 
  geom_vline(xintercept=0.95, color = "black") + 
  theme_light()  +
  theme(legend.position="bottom") +
  theme(aspect.ratio = .74)

# BINOMIAL & POISSON RESULTS

cov.b50.gph <- cov.bin %>%
  filter(SE.method.f != "Bootstrap") %>% 
  ggplot(aes(y=rho.f, x=cov, shape = delta.f)) +
  geom_point(size=pt.size) +
  scale_shape_discrete(solid=F,
                       name = "") +
  facet_grid(lambda.f ~ SE.method.f, 
              labeller = labeller(lambda.f = label_wrap_gen(text.size))) +
  scale_x_continuous(limits = c(0.92, 1),
                     breaks=c(0.92,0.95,0.98),
                     labels=c("92", "95", "98")) +
  labs(x="", y="", title="Binomial, p = 0.5") +
  geom_vline(xintercept=0.95, color = "grey") + 
  theme_light()  

cov.p3.gph <- cov.pois %>%
  filter(SE.method.f != "Bootstrap") %>% 
  ggplot(aes(y=rho.f, x=cov, shape = delta.f)) +
  geom_point(size=pt.size) +
  scale_shape_discrete(solid=F,
                       name = "") +
  facet_grid(lambda.f ~ SE.method.f,
  labeller = labeller(lambda.f = label_wrap_gen(text.size))) +
  scale_x_continuous(limits = c(0.92, 1),
                     breaks=c(0.92,0.95,0.98),
                     labels=c("92", "95", "98")) +
  labs(x="Coverage Rate (in Percent)", y="", title=expression("Poisson," ~ omega ~ "= 3"))+
  geom_vline(xintercept=0.95, color = "grey") + 
  theme_light() 

# combine graphs
cov.bp <- cov.b50.gph / cov.p3.gph +
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

cov.bp 



ggsave("./outputs/dens_AB.eps",
       plot = density.AB,
       height=8.09, width=5, units = "in")

ggsave("./outputs/dens_AB.png",
       plot = density.AB)

ggsave("./outputs/rq1_cntns.eps",
       plot = rq1.gph,
       height=6.36, width=4.27, units = "in")

ggsave("./outputs/rq1_cntns.png",
       plot = rq1.gph,
       height=6.36, width=4.27, units = "in")


ggsave("./outputs/rq1_bp.eps",
       plot = rq1.bp,
       height = 6.41, width = 5.17, units = "in")

ggsave("./outputs/rq1_bp.png",
       plot = rq1.bp,
       height = 6.41, width = 5.17, units = "in")


ggsave("./outputs/rq2_cntns.eps",
       plot = cov.gph,
       height = 6.41, width = 4.77, units = "in")

ggsave("./outputs/rq2_cntns.png",
       plot = cov.gph,
       height = 6.41, width = 4.77, units = "in")


ggsave("./outputs/rq2_bp.eps",
       plot = cov.bp,
       height = 5.83, width = 4.66, units = "in")

ggsave("./outputs/rq2_bp.png",
       plot = cov.bp,
       height = 5.83, width = 4.66, units = "in")



