require(performance)
require(MASS)
require(doBy)
require(foreign)
require(readstata13)
require(survey)
require(lme4)
require(pps)
library(parallel)
require(tidyr)
require(janitor)
require(ggplot2)
require(skimr)
require(forcats)
require(vtable)
require(patchwork)
require(knitr)
require(sampling)
require(dplyr)

date <- "20210701"

set.seed(20161014)

num.cores <- 16



# set up parameters for different populations

# cov(Y,Z) Z becomes RP
rhos <- c(0, 1) 

# mean RP -- controls RR
deltas <- c(-2, 0)


# variance in Y
yvars <- c(5, 20)

p <- expand.grid(rho=rhos, delta=deltas, yvar = yvars) 

params <- bind_rows(p, p, .id = "app") %>% 
        # variability in cluster means (lambda)
        mutate(lambda = if_else(app == 1, 0, yvar/5)) %>% 
        select(-app)
params$i <- row.names(params)
params

# must be an easier way to return standard error
myse <- function(var, dsgn) {
        sqrt(myvar(var, dsgn))
}

myvar <- function(var, dsgn) {
        vcov(svymean(var, design=dsgn, na.rm=TRUE))
}

mymean <- function(var, dsgn) {
        svymean(var, design=dsgn, na.rm = TRUE)[[1]]
}

# function to add global X label to patchwork plot
# from https://github.com/thomasp85/patchwork/issues/43
# author: mingsu
add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.03, Ygap = 0.03, ...) {
        ylabgrob <- patchwork::plot_spacer()
        if (!is.null(Ylab)) {
                ylabgrob <- ggplot() +
                        geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
                        theme_void()
        }
        if (!is.null(Xlab)) {
                xlabgrob <- ggplot() +
                        geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
                        theme_void()
        }
        if (!is.null(Ylab) & is.null(Xlab)) {
                return((ylabgrob + patchworkGrob(pwobj)) + 
                               patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
        }
        if (is.null(Ylab) & !is.null(Xlab)) {
                return((ylabgrob + pwobj) + 
                               (xlabgrob) +
                               patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                                      widths = c(0, 100),
                                                      design = "
                                   AB
                                   CC
                                   "
                               ))
        }
        if (!is.null(Ylab) & !is.null(Xlab)) {
                return((ylabgrob + pwobj) + 
                               (xlabgrob) +
                               patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                                      widths = 100 * c(Ygap, 1 - Ygap),
                                                      design = "
                                   AB
                                   CC
                                   "
                               ))
        }
        return(pwobj)
}



# full size samples
popobs <- 1000000
A <- 1000
sampobs <- 1000
b <- 10
a <- sampobs/b
sims <- 2000
# not all culsters same size
B <- c(rep_len(400, A/5),
       rep_len(500, A/5),
       rep_len(600, A/5),
       rep_len(1000, A/5),
       rep_len(2500, A/5))


# # test size samples
# popobs <- 10000
# A <- 100
# sampobs <- 100
# b <- 20
# a <- sampobs/b
# sims <- 30
# # not all culsters same size
# B <- c(rep_len(40, A/5),
#        rep_len(50, A/5),
#        rep_len(60, A/5),
#        rep_len(100, A/5),
#        rep_len(250, A/5))