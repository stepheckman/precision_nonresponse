
# panel data (2006 -2010) downloaded from GSS website in Stata format
# https://gss.norc.org/get-the-data/stata
gss <- read.dta13("./example_data/GSS_panel2010w123.dta") %>%
  dplyr::select(c(vpsu, vstrat), 
         starts_with(c("vote04", "prestg80_1", "finalter", "owngun", "dwelown", "postlife", "tvhours", "id_"))) %>%
  mutate(wave2 = !is.na(id_2)) %>%
  pivot_longer(ends_with(c("_1", "_2", "_3")),
               names_to = c(".value", "wave"),
               names_sep = "([_])") %>%
  rename(idnew = id) %>%
  filter(wave == 1) %>%
  group_by(vstrat, vpsu) %>%
  mutate(cluster = cur_group_id()) %>%
  ungroup()

# need 2006 cross sectional weight variable
# downloaded 2006 data from GSS website

# functions downloaded from GSS website
# see wtss2.tar.gz
read.dct <- function(dct, labels.included = "yes") {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  switch(labels.included,
         yes = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
           classes <- c("numeric", "character", "character", "numeric", "character")
           N <- 5
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
         },
         no = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
           classes <- c("numeric", "character", "character", "numeric")
           N <- 4
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
         })
  temp_metadata <- setNames(lapply(1:N, function(x) {
    out <- gsub(pattern, paste("\\", x, sep = ""), temp)
    out <- gsub("^\\s+|\\s+$", "", out)
    out <- gsub('\"', "", out, fixed = TRUE)
    class(out) <- classes[x] ; out }), NAMES)
  temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
  temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
  read.fwf(dat, widths = metadata_var[["ColWidth"]], col.names = metadata_var[["ColName"]])
}

GSS_metadata <- read.dct("./example_data/GSS.dct")
GSS_ascii <- read.dat("./example_data/GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
gss.wts <- GSS_ascii %>%
  filter(YEAR == 2006) %>%
  rename(idnew = ID_, 
         wt2006 = WTSSNR)

# wave 1 responses with response in wave 2, 3 indicated
# confirmed in stata with xtdescribe that there are no Rs who responded in q1, 3 but not 2
gss2 <- gss %>% 
  left_join(gss.wts, by="idnew") %>%
  mutate(voted.2004 = case_when(vote04 == "voted" ~ 1,
                                vote04 == "DID NOT VOTE" ~ 0),
         own.gun = case_when(owngun == "yes" ~ 1,
                             owngun == "no" ~ 0),
         fin.better = case_when(finalter == "better" ~ 1,
                                finalter %in% c("worse", "STAYED SAME") ~ 0), 
         dwel.own = case_when(dwelown == "OWN OR IS BUYING" ~ 1,
                              dwelown == "PAYS RENT" ~ 0,
                              dwelown == "other" ~ 0),
         after.life = case_when(postlife == "yes" ~ 1,
                                postlife == "no" ~ 0)) %>%
  rename(prestig = prestg80)

gss.rr <- weighted.mean(gss2$wave2, gss2$wt2006)
gss.rr

summary(gss2$wt2006)

# clustered sample design 
gss.dsg <- svydesign(ids = ~vpsu, nest = TRUE,
                     data = gss2, weights = ~wt2006)
gss.dsg.r <- svydesign(ids = ~vpsu, nest = TRUE,
                       data = gss2 %>% filter(wave2 == TRUE), weights = ~wt2006)
gss.dsg.nr <- svydesign(ids = ~vpsu, nest = TRUE,
                        data = gss2 %>% filter(wave2 == FALSE), weights = ~wt2006)


# data set for stderr estimation
rep.gss <- gss2 %>%
  # append to itself so each case appears 2x
  bind_rows(gss2, .id = "set") %>%
  # make new weights -- 0 for NRs in first occurrence
  mutate(wt2 = case_when(
    # these are original cases -- Rs and NRs
    # create weight = base weight for Rs, 0 for NRs
    set == 1 & wave2 == TRUE ~ wt2006,
    set == 1 & wave2 == FALSE ~ 0,
    # these are duplicate cases that were just appended 
    # here wt is just base weight
    set == 2 ~ wt2006))

lin.gss.dsg <- svydesign(ids = ~vpsu, data = rep.gss, weights = ~wt2)
rep.gss.dsg <- as.svrepdesign(lin.gss.dsg, type = c("JK1"))
boot.gss.dsg <- as.svrepdesign(lin.gss.dsg, type = c("bootstrap"))



