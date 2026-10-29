#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 02_checks
#' ---------

library(dplyr)
merged <- readRDS("data/interim/issp_analysis.rds")
issp_raw      <- readRDS("data/interim/issp_raw.rds")
country_level <- readRDS("data/interim/country_level_vars.rds")

#' Labels
attr(issp_raw$v6, "labels")
attr(issp_raw$degree, "labels")
attr(issp_raw$topbot, "labels")
attr(issp_raw$income, "labels")
attr(issp_raw$sex, "labels")
#' Old vs new recodes
table(issp_raw$v6,     merged$ind_belief, useNA = "ifany")
table(issp_raw$degree, merged$credential, useNA = "ifany")
table(issp_raw$income, merged$income,     useNA = "ifany")
table(issp_raw$sex,    merged$gender,     useNA = "ifany")
summary(issp_raw$age); summary(merged$age); summary(merged$age_sq)

#' Direction Checks
tapply(merged$sss, merged$ind_belief, mean, na.rm = TRUE)
tapply(merged$sss, merged$credential, mean, na.rm = TRUE)
tapply(merged$ind_belief, merged$credential, mean, na.rm = TRUE)

#' Structure
nrow(merged) == nrow(issp_raw)                     
table(merged$wave)                              
table(merged$country, merged$wave)
table(merged$wave, is.na(merged$ind_belief))     
anyDuplicated(country_level[, c("country", "wave")])   
sort(unique(merged$country))

#' Missingness
colMeans(is.na(merged))
summary(merged$sss); summary(merged$ind_belief); summary(merged$isei)
table(merged$income, useNA = "ifany")
sort(table(merged$isco88[is.na(merged$isei)]), decreasing = TRUE)[1:10]

#' Stratification: direction and values
merged %>%
  distinct(country, stratification) %>%
  arrange(stratification) %>%
  print(n = 40)

#' Country-level missingness
merged %>%
  group_by(country) %>%
  summarise(
    miss_strat = mean(is.na(stratification)),
    miss_gdp   = mean(is.na(gdp_pc)),
    miss_tert  = mean(is.na(tertiary)),
    miss_gini  = mean(is.na(gini))
  ) %>%
  print(n = 35)

#' Tertiary gaps in the waves used by the main model
merged %>%
  filter(wave %in% c(2009, 2019), !is.na(stratification)) %>%
  group_by(country, wave) %>%
  summarise(tert_missing = all(is.na(tertiary)), .groups = "drop") %>%
  filter(tert_missing)

country_level %>%
  filter(country %in% c("Australia", "Germany", "Japan", "New Zealand", "Portugal", "USA"),
         wave %in% 2006:2012) %>%
  arrange(country, wave) %>%
  print(n = 50)

#' Tertiary cut
tmp <- data.frame(country = merged$country, wave = merged$wave,
                  degree = as.numeric(issp_raw$degree))   # needs issp_raw loaded
tmp %>%
  filter(wave %in% c(2009, 2019), country %in% unique(d3$country)) %>%
  group_by(country) %>%
  summarise(pct_4 = 100 * mean(degree == 4), pct_5 = 100 * mean(degree == 5)) %>%
  arrange(desc(pct_4)) %>% print(n = 30)


table(issp_raw$degree, merged$credential_alt, useNA = "ifany")


