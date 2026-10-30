#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 07_educyrs_variant
#' -----------------

library(haven)
library(dplyr)

issp_raw <- readRDS("data/interim/issp_raw.rds")
analysis <- readRDS("data/interim/issp_analysis.rds")

#' print(attr(issp_raw$educyrs, "labels"))   # check what 95/96 mean

ey <- issp_raw %>%
  transmute(iso  = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))),
            id   = caseid,
            wave = year,
            educyrs = if_else(educyrs >= 0 & educyrs <= 30, educyrs, NA_real_)) %>%
  zap_labels()

n_before <- nrow(analysis)
analysis <- analysis %>%
  left_join(ey, by = c("iso", "id", "wave"), relationship = "one-to-one")
stopifnot(nrow(analysis) == n_before)

analysis <- analysis %>%
  mutate(educyrs_cube = (educyrs / 10)^3)      # scaled so coefficients are readable

saveRDS(analysis, "data/interim/issp_educyrs.rds")

#' Checks: is EDUCYRS stable across waves where DEGREE was not?
analysis %>%
  filter(wave %in% c(2009, 2019), !is.na(stratification)) %>%
  group_by(country, wave) %>%
  summarise(mean_years = mean(educyrs, na.rm = TRUE),
            pct_missing = 100 * mean(is.na(educyrs)),
            pct_tertiary = 100 * mean(credential, na.rm = TRUE), .groups = "drop") %>%
  print(n = 60)

