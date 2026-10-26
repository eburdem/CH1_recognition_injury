#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 01_download_and_build_the_data
#' ------------------------------
getwd()
library(haven)
library(dplyr)

cumulation_issp <- read_dta("data/raw/issp/cumulation/ZA8790_v1-0-0b.dta")
dim(cumulation_issp)
names(cumulation_issp)
table(cumulation_issp$year)
table(cumulation_issp$TOPBOT, useNA= "ifany")

issp_ch1 <- cumulation_issp %>% filter(year %in% c(1999,2009,2019))
nrow(issp_ch1)
saveRDS(issp_ch1, "data/interim/issp_1999_2009_2019_raw.rds")

#' Clean and Harmonize

names(issp_ch1)

count(issp_clean, ind_belief)

issp_clean <- issp_ch1 %>% 
  transmute(
    wave       = year,
    sss        = if_else(TOPBOT %in% 1:10, TOPBOT, NA_real_),
    credential = case_when(DEGREE == 5 ~ 1, DEGREE %in% 0:4 ~ 0), # tertiary = degree 5 only
    ind_belief = if_else(v6 %in% 1:5, 6 - v6, NA_real_)
  )
  zap_labels()

attr(issp_ch1$v6, "labels")
attr(issp_ch1$DEGREE, "labels")
attr(issp_ch1$TOPBOT, "labels")

table(issp_ch1$v6, issp_clean$ind_belief, useNA = "ifany")
table(issp_ch1$DEGREE, issp_clean$credential, useNA = "ifany")

tapply(issp_clean$sss, issp_clean$ind_belief, mean, na.rm = TRUE)
tapply(issp_clean$sss, issp_clean$credential, mean, na.rm = TRUE)
