
bias <- function(cluster, wt, y_r, y) {
  
  df <- data_frame(cluster, wt, y_r, y)
  
  dsgn <- svydesign(ids = ~cluster, data = df, weights = ~wt)
  
  mymean(~y_r, dsgn) - mymean(~y, dsgn)
}

# make pop data frame -- unclustered
pop.fn <- function(covar, delta, yvar, rpvar) {
  # cov is covariance between Y and Z (Z turns into RP)
  # rpmean is mean of Z -- related to mean RP
  # rpvar is variance of Z -- related to variance of RP
  
  # draw Y,Z from multivar normal
  sigma <- matrix(c(yvar, covar, covar, rpvar), nrow=2, ncol=2)
  dt <- data.frame(MASS::mvrnorm(n = popobs, c(ymean, delta), sigma))
  names(dt) <- c("y", "z")
  
  dt2 <- dt %>%
    mutate(yorig = y,
           # make RP for each case from cntns Z
           rp = exp(1 + z)/(1+exp(1 + z)), 
           rrand.det = runif(n()),
           #yrand = runif(n()),
           #y.demean = y - ymean
           ) %>%
    mutate(# respondent if random draw < rp
           resp.det = as.numeric(rrand.det < rp),
           # make dichotomus y2
           #y2 = as.numeric(yrand< exp(1 + y.demean)/(1 + exp(1 + y.demean)))
           ) %>% 
    mutate(
      # y_r should be missing if nonresponder
      y_r.det = if_else(resp.det == 1, y, NA_real_), 
      # y_nr should be missing if nonresponder
      y_nr.det = if_else(resp.det == 0, y, NA_real_), 
      rho = covar, 
      delta = delta, 
      Yvar = yvar)
  
  # check out dist of RP
  plot(density(dt2$rp))
  summary(dt2$rp)
  
  dt2
}

# make clustered pop data frame
pop.clust.fn <- function(covar, delta, yvar, zvar, clustsd) {
  # code adapted from Brady West
  
  # covar: covariance between Y & Z
  # zvar: variance of Z in population 
  # zmean: mean of Z in population (controls RR)
  # clustsd: how diff clusters are from each other on Y (std error of cluster means)
  
  # A is number of clusters
  # B vector gives number of cases in each cluster
  
  # initialize data vectors
  y <- numeric(popobs)
  z <- numeric(popobs)
  clusterid <- numeric(popobs)
  
  from.row <- 1
  
  # data generation -- step through clusters
  for (i in 1:A) {
    
    # from row & to row for this cluster
    to.row <- from.row + B[i] - 1
    
    # select random effect for this cluster
    # clustsd is lambda parameter
    cl.effect <- rnorm(1,mean=0,sd=clustsd) 
    
    # draw Y,Z from multivar normal in this cluster
    clustmean <- ymean + cl.effect 
    # covar here is rho parameter
    # zvar is constanst at 1 (rpvar)
    sigma <- matrix(c(yvar, covar, covar, zvar), nrow=2, ncol=2)
    yz <- data.frame(MASS::mvrnorm(n = B[i], c(clustmean, delta), sigma))
    y[from.row:to.row] = yz[,1]
    z[from.row:to.row] = yz[,2]
    clusterid[from.row:to.row] = i
    
    #cat(sprintf("i: = %f\nfrom: = %f;\n to: = %f\n\n", i, from.row, to.row))
    
    from.row = to.row + 1
  }
  
  d <- data.frame(y, z, clusterid)
  names(d) <- c("y", "z", "clusterid")
  #summaryBy(data=d, FUN=c(mean,var), y+z~clusterid)
  #summary(d)
  
  # turn z into rp
  d2 <- d %>% 
    mutate(yorig = y,
      # make RP for each case from cntns Z
      rp = exp(1 + z)/(1+exp(1 + z)), 
      rrand.det = runif(n()),
      #yrand = runif(n())
      ) %>%
    mutate(
      # respondent if random draw < rp
      resp.det = as.numeric(rrand.det < rp),
      # make dichotomus y2
      #y2 = as.numeric(yrand < exp(1 + y)/(1 + exp(1 + y)))
      ) %>% 
    mutate(
      # y_r should be missing if nonresponder
      y_r.det = if_else(resp.det == 1, y, NA_real_), 
      # y_nr should be missing if nonresponder
      y_nr.det = if_else(resp.det == 0, y, NA_real_), 
      rho = covar, 
      delta = delta, 
      lambda = clustsd,
      Yvar = yvar)
  
  summary(d2$rp)
  # check out dist of RP
  plot(density(d2$rp))
  
  d2
}


loop <- function(rho, delta, lambda, yvar, i) {
    
  # z var -- does not vary
  rpvar <- 1
  
  cat(sprintf("rho=%f; delta=%f; lambda=%f; yvar=%f\n", rho, delta, lambda, yvar))
  
  if (lambda == 0) {
    # make unclustered population according to these settings
    popdt <- pop.fn(rho, delta, yvar, rpvar)
  } else {
    # make clustered population according to these settings
    popdt <- pop.clust.fn(rho, delta, yvar, rpvar, lambda)
    
    # set up clustered sample
    clustsizes <- popdt %>% 
      group_by(clusterid)  %>%
      summarise(length = n(), .groups = 'drop') %>%
      mutate(pi1 = a * length/popobs,
             pi2 = b/length) 
  }
  
  # true pop mean of Y
  Y.mean <- mean(popdt$y)
    
  # output pop to dataset
  saveRDS(popdt,file=paste("./continuous/pop", i, ".rds", sep = "") )
  
  
  ######################################################################
  # select samples from this pop
  
  # to store all samples (all cases for each sample) from this pop
  # final data set has sims * sampobs cases
  samp.all <- data.frame()
  
  # summarize sampled cases into sample level dataset
  # final data set has sims obs
  samplvl.results <- data.frame()
  
  # select all SRS samples for this pop, put in 1 data frame
  # samp.num indicates the diff samples (1:sims)
  
  # select each sample
  for (s in 1:sims) {
    
    if (lambda == 0) {
      # select sample 
      samp <- popdt[srswor(sampobs, popobs)==1,] %>% 
        mutate(wt = 1,
               samp.num = s)
    } else {
      
      selclusters <- ppss(t(as.vector(clustsizes[,2])), a)
      
      # all cases in selected clusters
      tempsamp <- filter(popdt, clusterid %in% selclusters)
      
      # sel cases in selected clusters
      st <- sampling::strata(data=tempsamp, c("clusterid"), 
                             size=rep(b, each=a), method="srswor")
      tempsamp2 <- getdata(tempsamp, st)
      
      samp <- inner_join(tempsamp2, clustsizes, by = "clusterid") %>%
        mutate(pi = pi1 * pi2) %>%
        mutate(wt = 1/pi) %>%
        mutate(samp.num = s)
    }
    
    # get results for this sample
    samp2 <- samp %>% 
      # for stochastic response, find which cases respond in this survey
      mutate(rrand.stoch = runif(sampobs)) %>%
      mutate(resp.stoch = as.numeric(rrand.stoch < rp)) %>%
      # make new y variables for stochastic response
      # y_r.stoch should be missing if nonresponder
      mutate(y_r.sto = case_when(resp.stoch == 1 ~ y),
             # y_nr.stoch should be missing if responder
             y_nr.sto = case_when(resp.stoch == 0 ~ y),
             id = row_number())
    
    # append results for this sample to big dataset
    samp.all <- samp.all %>%
      bind_rows(samp2)
    
    
    #########################################################
    # summarize sample into 1 obs
    
    if (lambda == 0) {
      # SRS sample design 
      svyd <- svydesign(ids = ~1, data = samp2, weights = ~wt)
    } else {
      # clustered sample design 
      svyd <- svydesign(ids = ~clusterid, data = samp2, weights = ~wt)
    }
    
    # quantiles not working in mutate
    ymed <- svyquantile(~y, svyd, quantile = .5, na.rm = TRUE)
    ymed.det <- svyquantile(~y_r.det, svyd, quantile = .5, na.rm = TRUE)
    ymed.sto <- svyquantile(~y_r.sto, svyd, quantile = .5, na.rm = TRUE)
    
    # summarize to one obs for this sample
    samp.sum <- samp2 %>% 
      select(samp.num, y, rp, y_r.det, y_nr.det, resp.det,
             y_r.sto, y_nr.sto, resp.stoch)  %>%
      summarise_all(mean, na.rm = TRUE) %>%
      mutate(cluster = lambda, 
             y.median = ymed,
             y_r.det.median = ymed.det,
             y_r.sto.median = ymed.sto)
    
    # variance on full mean, respondent mean, for this sample
    samp.sum$var.y = myvar(~y, svyd)
    samp.sum$var.y_r.det = myvar(~y_r.det, svyd)
    samp.sum$var.y_r.sto = myvar(~y_r.sto, svyd)
    
    # Lee standard errors on mean -- det and stoch
    samp.sum$se1.mean.lee = (1-mymean(~resp.det, svyd)) * 
      sqrt(myvar(~y_r.det, svyd) + myvar(~y_nr.det, svyd))
    samp.sum$se3.mean.lee = (1-mymean(~resp.stoch, svyd)) * 
      sqrt(myvar(~y_r.sto, svyd) + myvar(~y_nr.sto, svyd))
    
    # estimates of bias for this sample
    samp.sum$bias1.mean = mymean(~y_r.det, svyd) - mymean(~y, svyd)
    samp.sum$bias3.mean = mymean(~y_r.sto, svyd) - mymean(~y, svyd)
    
    # replication SEs for this sample
    samp3 <- samp2 %>%
      # join samp2 to itself so each case appears 2x
      bind_rows(samp2, .id = "set") %>%
      # make new weights -- 0 for NRs in first occurrence
      mutate(wt2 = case_when(
        # these are original cases -- Rs and NRs
        # create weight = base weight for Rs, 0 for NRs
        set == 1 ~ resp.det * wt,
        # these are duplicate cases that were just appended 
        # here wt is just base weight
        set == 2 ~ wt))
    
    # set up survey objects
    if (lambda == 0) {
      # SRS sample -- now clustered by id, since each case appears 2x
      lin.dsg <- svydesign(ids = ~id, data = samp3, weights = ~wt2)
    } else {
      # PPS sample
      # according to Jill's write up, clustering unit remains cluster indicator
      lin.dsg <- svydesign(ids = ~clusterid, data = samp3, weights = ~wt2)
    }
    rep.dsg <- as.svrepdesign(lin.dsg, 
                              type = "JK1")
    boot.dsg <- as.svrepdesign(lin.dsg,
                               type = "bootstrap")
    
    ##########################################################
    # estimate bias as difference between set == 1, 2
    
    # means
    bias1.mean.rep <- svyby(~y, ~set, rep.dsg, svymean, covmat = TRUE)
    bias1.mean.lin <- svyby(~y, ~set, lin.dsg, svymean, covmat = TRUE)
    bias1.mean.boot <- svyby(~y, ~set, boot.dsg, svymean, covmat = TRUE)
        
    # get stderrs on bias estimate onto summary data set
    samp.sum$se1.mean.rep <- SE(svycontrast(bias1.mean.rep, quote(`1` - `2`)))
    samp.sum$se1.mean.lin <- SE(svycontrast(bias1.mean.lin, quote(`1` - `2`)))
    samp.sum$se1.mean.boot <- SE(svycontrast(bias1.mean.boot, quote(`1` - `2`)))
    
    # append obs for this sample to all previous samples for this pop
    samplvl.results <- samplvl.results %>% 
      # reduces each sample to one obs
      bind_rows(samp.sum) 
  }
  
  # add parameters for this pop to summary data sets
  samp.all2 <- samp.all %>%
    mutate(lambda = lambda,
           rho = rho,
           delta = delta, 
           Yvar = yvar,
           pop = strtoi(i))
  
  # further processing of data set containing 1 obs for each sample
  samplevel.results2 <- samplvl.results %>%
    mutate(lambda = lambda,
           rho = rho,
           delta = delta, 
           Yvar = yvar,
           pop = strtoi(i),
           rbias1.mean = bias1.mean / Y.mean * 100,
           rbias3.mean = bias3.mean / Y.mean * 100)
  
  # output samples to saved dataset
  saveRDS(samp.all2,
          file=paste("./continuous/samps_pop", i, ".rds", sep = ""))
  
  # # make stata version of this dataset to calc linearized stderr in stata
  # save.dta13(samp.all2, 
  #            paste("./stata/samps_pop", i, ".dta", sep = ""),
  #            convert.factors = TRUE, convert.underscore = TRUE, compress = TRUE,
  #            version=15)
  
  # output samples to saved dataset
  saveRDS(samplevel.results2,
          file=paste("./continuous/samps_sum", i, ".rds", sep = ""))
  
  cat(sprintf("Done with pop %s\n", i))
  
  return()
}

