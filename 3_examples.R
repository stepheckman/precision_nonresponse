source("0_setup.R")

cat(sprintf("\n\nSTART: 3_examples.R\n\n"))

source("3.1_prepGSS.R")

source("3.2_prepLISS.R")


##############################################################################
# GSS

# voted.2004
bias.voted2004 <- mymean(~voted.2004, gss.dsg.r) - mymean(~voted.2004, gss.dsg)

# lee std error
lee.voted2004 <- (1-gss.rr) * sqrt((myvar(~voted.2004, gss.dsg.r) + myvar(~voted.2004, gss.dsg.nr)))

rep.voted2004 <- SE(svycontrast(svyby(~voted.2004, ~set, rep.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                quote(`1` - `2`)))

lin.voted2004 <- SE(svycontrast(svyby(~voted.2004, 
                                      ~set, 
                                      subset(lin.gss.dsg,!is.na(voted.2004)), 
                                      svymean, 
                                      na.rm = TRUE, 
                                      covmat = TRUE),
                                quote(`1` - `2`)))

boot.voted2004 <- SE(svycontrast(svyby(~voted.2004, ~set, boot.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                quote(`1` - `2`)))

model.voted2004 <- glmer(voted.2004 ~ 1 + (1 | cluster), data = gss2, 
                         family = binomial)

icc.voted2004 <- as.numeric(icc(model.voted2004)[1])



# own.guns
bias.owngun <- mymean(~own.gun, gss.dsg.r) - mymean(~own.gun, gss.dsg)

# lee std error
lee.owngun <- (1-gss.rr) * sqrt((myvar(~own.gun, gss.dsg.r) + myvar(~own.gun, gss.dsg.nr)))

rep.owngun <- SE(svycontrast(svyby(~own.gun, ~set, rep.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                quote(`1` - `2`)))

lin.owngun <- SE(svycontrast(svyby(~own.gun, 
                                      ~set, 
                                      subset(lin.gss.dsg,!is.na(own.gun)), 
                                      svymean, 
                                      na.rm = TRUE, 
                                      covmat = TRUE),
                                quote(`1` - `2`)))

boot.owngun <- SE(svycontrast(svyby(~own.gun, ~set, boot.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                 quote(`1` - `2`)))

model.owngun <- glmer(own.gun ~ 1 + (1 | cluster), data = gss2, 
                      family = binomial)

icc.owngun <- as.numeric(icc(model.owngun)[1])


# finances better
bias.finbetter <- mymean(~fin.better, gss.dsg.r) - mymean(~fin.better, gss.dsg)

lee.finbetter <- (1-gss.rr) * sqrt((myvar(~fin.better, gss.dsg.r) + myvar(~fin.better, gss.dsg.nr)))

rep.finbetter <- SE(svycontrast(svyby(~fin.better, ~set, rep.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                             quote(`1` - `2`)))


lin.finbetter <- SE(svycontrast(svyby(~fin.better, 
                                      ~set, 
                                      subset(lin.gss.dsg,!is.na(fin.better)), 
                                      svymean, 
                                      na.rm = TRUE, 
                                      covmat = TRUE),
                                quote(`1` - `2`)))

boot.finbetter <- SE(svycontrast(svyby(~fin.better, ~set, boot.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                 quote(`1` - `2`)))

model.finbetter <- glmer(fin.better ~ 1 + (1 | cluster), 
                      data = gss2,
                      family = binomial)

icc.finbetter <- as.numeric(icc(model.finbetter)[1])


# own dwelling
bias.prestig <- mymean(~prestig, gss.dsg.r) - mymean(~prestig, gss.dsg)

lee.prestig <- (1-gss.rr) * sqrt((myvar(~prestig, gss.dsg.r) + myvar(~prestig, gss.dsg.nr)))

rep.prestig <- SE(svycontrast(svyby(~prestig, ~set, rep.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                quote(`1` - `2`)))


lin.prestig <- SE(svycontrast(svyby(~prestig, 
                                      ~set, 
                                      subset(lin.gss.dsg,!is.na(prestig)), 
                                      svymean, 
                                      na.rm = TRUE, 
                                      covmat = TRUE),
                                quote(`1` - `2`)))

boot.prestig <- SE(svycontrast(svyby(~prestig, ~set, boot.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                 quote(`1` - `2`)))

model.prestig <- lmer(prestig ~ 1 + (1 | cluster), 
                       data = gss2)

icc.prestig <- as.numeric(icc(model.prestig)[1])


# after life
bias.afterlife <- mymean(~after.life, gss.dsg.r) - mymean(~after.life, gss.dsg)

lee.afterlife <- (1-gss.rr) * sqrt((myvar(~after.life, gss.dsg.r) + myvar(~after.life, gss.dsg.nr)))

rep.afterlife <- SE(svycontrast(svyby(~after.life, ~set, rep.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                              quote(`1` - `2`)))


lin.afterlife <- SE(svycontrast(svyby(~after.life, 
                                      ~set, 
                                      subset(lin.gss.dsg,!is.na(after.life)), 
                                      svymean, 
                                      na.rm = TRUE, 
                                      covmat = TRUE),
                                quote(`1` - `2`)))

boot.afterlife <- SE(svycontrast(svyby(~after.life, ~set, boot.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                 quote(`1` - `2`)))

model.afterlife <- glmer(after.life ~ 1 + (1 | cluster), 
                         data = gss2,
                         family = binomial)

icc.afterlife <- as.numeric(icc(model.afterlife)[1])


# TV hours
bias.tvhrs <- mymean(~tvhours, gss.dsg.r) - mymean(~tvhours, gss.dsg)

lee.tvhrs <- (1-gss.rr) * sqrt((myvar(~tvhours, gss.dsg.r) + myvar(~tvhours, gss.dsg.nr)))

rep.tvhrs <- SE(svycontrast(svyby(~tvhours, ~set, rep.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                quote(`1` - `2`)))


lin.tvhrs <- SE(svycontrast(svyby(~tvhours, 
                                      ~set, 
                                      subset(lin.gss.dsg,!is.na(tvhours)), 
                                      svymean, 
                                      na.rm = TRUE, 
                                      covmat = TRUE),
                                quote(`1` - `2`)))

boot.tvhrs <- SE(svycontrast(svyby(~tvhours, ~set, boot.gss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                 quote(`1` - `2`)))

model.tvhrs <- lmer(tvhours ~ 1 + (1 | cluster), gss2)

icc.tvhrs <- as.numeric(icc(model.tvhrs)[1])



##############################################################################
# LISS

# TV hours
bias.tvhrs2 <- mymean(~tv.hours, liss.dsg.r) - mymean(~tv.hours, liss.dsg)

lee.tvhrs2 <- (1-liss.rr) * sqrt((myvar(~tv.hours, liss.dsg.r) + myvar(~tv.hours, liss.dsg.nr)))

rep.tvhrs2 <- SE(svycontrast(svyby(~tv.hours, ~set, rep.liss.dsg, svymean, 
                                  na.rm = TRUE, covmat = TRUE),
                            quote(`1` - `2`)))

lin.tvhrs2 <- SE(svycontrast(svyby(~tv.hours, 
                                      ~set, 
                                      subset(lin.liss.dsg,!is.na(tv.hours)), 
                                      svymean, 
                                      na.rm = TRUE, 
                                      covmat = TRUE),
                                quote(`1` - `2`)))

boot.tvhrs2 <- SE(svycontrast(svyby(~tv.hours, ~set, boot.liss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                                 quote(`1` - `2`)))

# immigrants make thing worse
bias.immig <- mymean(~immigrants, liss.dsg.r) - mymean(~immigrants, liss.dsg)

lee.immig <- (1-liss.rr) * sqrt((myvar(~immigrants, liss.dsg.r) + myvar(~immigrants, liss.dsg.nr)))

rep.immig <- SE(svycontrast(svyby(~immigrants, ~set, rep.liss.dsg, svymean, 
                                  na.rm = TRUE, covmat = TRUE),
                            quote(`1` - `2`)))

lin.immig <- SE(svycontrast(svyby(~immigrants, 
                                   ~set, 
                                   subset(lin.liss.dsg,!is.na(immigrants)), 
                                   svymean, 
                                   na.rm = TRUE, 
                                   covmat = TRUE),
                             quote(`1` - `2`)))

boot.immig <- SE(svycontrast(svyby(~immigrants, ~set, boot.liss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                              quote(`1` - `2`)))

# drunk driving
bias.drunk <- mymean(~drunk.driving, liss.dsg.r) - mymean(~drunk.driving, liss.dsg)

lee.drunk <- (1-liss.rr) * sqrt((myvar(~drunk.driving, liss.dsg.r) + myvar(~drunk.driving, liss.dsg.nr)))

rep.drunk <- SE(svycontrast(svyby(~drunk.driving, ~set, rep.liss.dsg, svymean, 
                                  na.rm = TRUE, covmat = TRUE),
                            quote(`1` - `2`)))


lin.drunk <- SE(svycontrast(svyby(~drunk.driving, 
                                   ~set, 
                                   subset(lin.liss.dsg,!is.na(drunk.driving)), 
                                   svymean, 
                                   na.rm = TRUE, 
                                   covmat = TRUE),
                             quote(`1` - `2`)))

boot.drunk <- SE(svycontrast(svyby(~drunk.driving, ~set, boot.liss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                              quote(`1` - `2`)))

# times eating in restaurant
bias.restaurant <- mymean(~restaurant, liss.dsg.r) - mymean(~restaurant, liss.dsg)

lee.restaurant <- (1-liss.rr) * sqrt((myvar(~restaurant, liss.dsg.r) + myvar(~restaurant, liss.dsg.nr)))

rep.restaurant <- SE(svycontrast(svyby(~restaurant, ~set, rep.liss.dsg, svymean, 
                                  na.rm = TRUE, covmat = TRUE),
                            quote(`1` - `2`)))

lin.restaurant <- SE(svycontrast(svyby(~restaurant, 
                                   ~set, 
                                   subset(lin.liss.dsg,!is.na(restaurant)), 
                                   svymean, 
                                   na.rm = TRUE, 
                                   covmat = TRUE),
                             quote(`1` - `2`)))

boot.restaurant <- SE(svycontrast(svyby(~restaurant, ~set, boot.liss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                              quote(`1` - `2`)))

# exercise more than 2 times
bias.ex2 <- mymean(~exercise, liss.dsg.r) - mymean(~exercise, liss.dsg)

lee.ex2 <- (1-liss.rr) * sqrt((myvar(~exercise, liss.dsg.r) + myvar(~exercise, liss.dsg.nr)))

rep.ex2 <- SE(svycontrast(svyby(~exercise, ~set, rep.liss.dsg, svymean, 
                                  na.rm = TRUE, covmat = TRUE),
                            quote(`1` - `2`)))

lin.ex2 <- SE(svycontrast(svyby(~exercise, 
                                   ~set, 
                                   subset(lin.liss.dsg,!is.na(exercise)), 
                                   svymean, 
                                   na.rm = TRUE, 
                                   covmat = TRUE),
                             quote(`1` - `2`)))

boot.ex2 <- SE(svycontrast(svyby(~exercise, ~set, boot.liss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                              quote(`1` - `2`)))

# support obama
bias.obama <- mymean(~obama, liss.dsg.r) - mymean(~obama, liss.dsg)

lee.obama <- (1-liss.rr) * sqrt((myvar(~obama, liss.dsg.r) + myvar(~obama, liss.dsg.nr)))

rep.obama <- SE(svycontrast(svyby(~obama, ~set, rep.liss.dsg, svymean, 
                                na.rm = TRUE, covmat = TRUE),
                          quote(`1` - `2`)))


lin.obama <- SE(svycontrast(svyby(~obama, 
                                   ~set, 
                                   subset(lin.liss.dsg,!is.na(obama)), 
                                   svymean, 
                                   na.rm = TRUE, 
                                   covmat = TRUE),
                             quote(`1` - `2`)))

boot.obama <- SE(svycontrast(svyby(~obama, ~set, boot.liss.dsg, svymean, na.rm = TRUE, covmat = TRUE),
                              quote(`1` - `2`)))


# create columns
var <- c("after.life", "prestige", "fin.better", "own.gun", "tv.hours", "voted.2004")
fmean <- c(mymean(~after.life, gss.dsg),
           mymean(~prestig, gss.dsg),
           mymean(~fin.better, gss.dsg),
           mymean(~own.gun, gss.dsg),
           mymean(~tvhours, gss.dsg),
           mymean(~voted.2004, gss.dsg))
bias <- c(bias.afterlife, bias.prestig, bias.finbetter, bias.owngun, bias.tvhrs, bias.voted2004)
lee <- c(lee.afterlife, lee.prestig, lee.finbetter, lee.owngun, lee.tvhrs, lee.voted2004)
rep <- c(rep.afterlife, rep.prestig, rep.finbetter, rep.owngun, rep.tvhrs, rep.voted2004)
lin <- c(lin.afterlife, lin.prestig, lin.finbetter, lin.owngun, lin.tvhrs, lin.voted2004)
icc <- c(icc.afterlife, icc.prestig, icc.finbetter, icc.owngun, icc.tvhrs, icc.voted2004)
n <- c(length(fitted(model.afterlife)),
       length(fitted(model.prestig)),
       length(fitted(model.finbetter)),
       length(fitted(model.owngun)),
       length(fitted(model.tvhrs)),
       length(fitted(model.voted2004)))



gss.liss <- data.frame(var, fmean, bias, n, lee, lin, rep, stringsAsFactors = F) %>%
  mutate(survey = "GSS") %>%
  bind_rows(data.frame(
    var = c("tv.hours","immigrants","drunk.driving","restaurant","exercise", "obama"),
    fmean = c(mymean(~tv.hours, liss.dsg),
              mymean(~immigrants, liss.dsg),
              mymean(~drunk.driving, liss.dsg),
              mymean(~restaurant, liss.dsg),
              mymean(~exercise, liss.dsg),
              mymean(~obama, liss.dsg)),
    bias = c(bias.tvhrs2, bias.immig, bias.drunk, bias.restaurant, bias.ex2, bias.obama),
    lee = c(lee.tvhrs2, lee.immig, lee.drunk, lee.restaurant, lee.ex2, lee.obama),
    lin = c(lin.tvhrs2, lin.immig, lin.drunk, lin.restaurant, lin.ex2, lin.obama),
    rep = c(rep.tvhrs2, rep.immig, rep.drunk, rep.restaurant, rep.ex2, rep.obama),
    n = c(sum(!is.na(liss$tv.hours)),
          sum(!is.na(liss$immigrants)),
          sum(!is.na(liss$drunk.driving)),
          sum(!is.na(liss$restaurant)),
          sum(!is.na(liss$exercise)),
          sum(!is.na(liss$obama))),
    stringsAsFactors = F) %>%
    mutate(survey = "LISS")) %>%
  mutate(rbias = bias / fmean,
         cilo.lee = bias - 2 * lee, 
         cihi.lee = bias + 2 * lee, 
         cilo.rep = bias - 2 * rep, 
         cihi.rep = bias + 2 * rep,
         cilo.lin = bias - 2 * lin, 
         cihi.lin = bias + 2 * lin,
         rr = if_else(survey == "LISS", liss.rr, gss.rr),
         var2 = case_when(var == "after.life" ~ "Believe afterlife",
                          var == "prestige"~"Prestige",
                          var == "fin.better"~"Finances better",
                          var == "own.gun"~"Own gun",
                          var == "tv.hours"~"Hours TV",
                          var == "voted.2004"~"Voted 2004",
                          var == "tv.hours"~"Hours TV",
                          var == "immigrants"~"Immig. improve",
                          var == "drunk.driving"~"Drove drunk",
                          var == "restaurant"~"Times restaurant",
                          var == "exercise"~"Times exercise",
                          var == "obama"~"Approve Obama")) %>%
  rename(se.lee = lee,
         se.rep = rep,
         se.lin = lin) %>%
  mutate(ratio.rep = se.lee / se.rep,
         ratio.lin = se.lee / se.lin)

gss.liss.long <- gss.liss %>%
  pivot_longer(c(starts_with(c("rci", "ci"))),
               names_to = c(".value", ".method"), 
               names_sep = "([.])") %>%
  mutate(meth = case_when(.method == "lee" ~ "Lee", 
                          .method == "rep" ~"Replication", 
                          .method == "lin" ~"Linearization"),
         v = if_else(survey == "LISS", 1, 1.2))

ex.gss <- gss.liss.long %>%
  filter(survey == "GSS")

ex.liss <- gss.liss.long %>%
  filter(survey == "LISS")


saveRDS(gss.liss, "gss_liss.rds")
saveRDS(gss2, "gss2.rds")
saveRDS(liss, "liss2.rds")

cat(sprintf("\n\nEND: 3_examples.R\n\n"))