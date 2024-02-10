# C:\Users\seckman\Dropbox\papers\Test Significance of Bias\analysis\LISS\Wave 1\PC wave 1\jr13a_EN_1.0p.dta
l1.pc <- read.dta13("./example_data/jr13a_EN_1.0p.dta")

# C:\Users\seckman\Dropbox\papers\Test Significance of Bias\analysis\LISS\Wave 1\Smartphone wave 1\jq13a_EN_2.0p.dta
l2.web <- read.dta13("./example_data/jq13b_EN_2.0p.dta") %>%
  dplyr::select(nomem_encr) %>%
  mutate(wave2 = 1)


# codebook: C:\Users\seckman\Dropbox\papers\Test Significance of Bias\analysis\LISS\Wave 1\PC wave 1\codebook_jr13a_EN_1.1.pdf

liss <- l1.pc %>%
  left_join(l2.web, by="nomem_encr") %>%
  mutate(wave2 = if_else(is.na(wave2), FALSE, TRUE),
         drunk.driving = case_when(jr13a038 == 1 ~ 1,
                                   jr13a038 == 2 ~ 0), 
         immigrants = case_when(jr13a045 <= 5  ~ 0,
                                jr13a045 >5 ~ 1), 
         tv.hours = case_when(!is.na(jr13a031) & !is.na(jr13a030) ~ (jr13a030 * 60 + jr13a031)/60, 
                              is.na(jr13a031) & !is.na(jr13a030) ~ jr13a030/60,
                              !is.na(jr13a031) & is.na(jr13a030) ~ jr13a031),
         exercise = case_when(jr13a029 %in% c(1,2) ~ 0,
                              jr13a029 %in% c(3,4) ~ 1),
         restaurant2 = case_when(jr13a027 < 90 ~ jr13a027,
                                 TRUE ~ as.double(NA))) %>%
  rename(id = nomem_encr, 
         restaurant = jr13a027, 
         obama = jr13a043,
         bush = jr13a044) %>%
  dplyr::select(id, wave2, restaurant, exercise, drunk.driving, immigrants, tv.hours, obama, bush)

liss.rr <- mean(liss$wave2)
liss.rr

# unclustered sample design 
liss.dsg <- svydesign(ids = ~id, 
                     data = liss)
liss.dsg.r <- svydesign(ids = ~id, 
                       data = liss %>% filter(wave2 == TRUE))
liss.dsg.nr <- svydesign(ids = ~id, 
                        data = liss %>% filter(wave2 == FALSE))



# data set for std err estimation
rep.liss <- liss %>%
  # append to itself so each case appears 2x
  bind_rows(liss, .id = "set") %>%
  # make new weights -- 0 for NRs in first occurrence
  mutate(wt2 = case_when(
    # these are original cases -- Rs and NRs
    # create weight = base weight for Rs, 0 for NRs
    set == 1 & wave2 == TRUE ~ 1,
    set == 1 & wave2 == FALSE ~ 0,
    # these are duplicate cases that were just appended 
    # here wt is just base weight
    set == 2 ~ 1))

lin.liss.dsg <- svydesign(ids = ~id, 
                          data = rep.liss, 
                          weights = ~wt2)
rep.liss.dsg <- as.svrepdesign(lin.liss.dsg, type = c("JK1"))
boot.liss.dsg <- as.svrepdesign(lin.liss.dsg, type = c("bootstrap"))



