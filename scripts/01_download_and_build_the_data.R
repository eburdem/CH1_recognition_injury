#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 01_download_and_build_the_data
#' ------------------------------
getwd()
library(haven)
library(dplyr)
library(readr)

#' ISSP: load, keep three waves, lowecase names once

issp_raw <- read_dta("data/raw/issp/cumulation/ZA8790_v1-0-0b.dta") %>%
  filter(year %in% c(1999, 2009, 2019))
names(issp_raw) <- tolower(names(issp_raw))
saveRDS(issp_raw, "data/interim/issp_raw.rds")

#' ISSP:recode

issp <- issp_raw %>%
  transmute(
    country    = recode(sub("^[0-9]+\\. [A-Z]{2}-", "", as.character(as_factor(country))),
                        "United States" = "USA", "Slovak Republic" = "Slovakia"),
    wave       = year,
    sss        = if_else(topbot %in% 1:10, topbot, NA_real_),
    credential = case_when(degree == 5 ~ 1, degree %in% 0:4 ~ 0),   # tertiary = degree 5 only
    credential_alt = case_when(degree >= 4 ~ 1, degree %in% 0:3 ~ 0),   # post-secondary or higher
    ind_belief = if_else(v6 %in% 1:5, 6 - v6, NA_real_),            # 5 = strongest endorsement
    income     = factor(if_else(income %in% 1:3, income, NA_real_)),
    age        = if_else(age > 0, age, NA_real_),
    age_sq     = age^2,
    gender     = case_when(sex == 1 ~ 0, sex == 2 ~ 1),             # 1 = female
    isco88     = isco88
  ) %>%
  zap_labels()

#' ISEI Lookup 
x <- readLines("data/raw/macro/iskoisei.sps")
x <- gsub("recode @isko \\(|\\) into @isei\\.", "", x)
isei_lookup <- read.table(text = gsub("=", " ", x), col.names = c("isco88", "isei"))
issp <- left_join(issp, isei_lookup, by = "isco88")

#' Country-level Variables

strat <- read_csv("data/raw/macro/oecd/age_of_first_selection.csv", show_col_types = FALSE) %>%
  transmute(country, stratification = max(age_first_selection, na.rm = TRUE) - age_first_selection)

wdi <- read_csv("data/raw/macro/worldbank/wdi_gdp_tertiary_1995_2020.csv", show_col_types = FALSE) %>%
  mutate(country = recode(country,
                          "United States" = "USA", "United Kingdom" = "Great Britain",
                          "Czechia" = "Czech Republic", "Slovak Republic" = "Slovakia",
                          "Korea, Rep." = "Korea")) %>%
  select(country, wave = year, gdp_pc, tertiary)

swiid <- read_csv("data/raw/macro/swiid/swiid9_92_summary.csv", show_col_types = FALSE) %>%
  mutate(country = recode(country,
                          "United States" = "USA", "United Kingdom" = "Great Britain",
                          "Turkey" = "Turkiye")) %>%
  select(country, wave = year, gini = gini_disp)

#' tertiary is NA for six country-waves in 2009 (Australia, Germany, Japan, NZ, Portugal, USA);
#' kept as NA by decision
country_level <- strat %>%
  left_join(wdi,   by = "country") %>%
  left_join(swiid, by = c("country", "wave"))

setdiff(unique(issp$country), strat$country)  
setdiff(unique(issp$country), wdi$country)    
setdiff(unique(issp$country), swiid$country) 


saveRDS(country_level, "data/interim/country_level_vars.rds")

merged <- left_join(issp, country_level, by = c("country", "wave"))
stopifnot(nrow(merged) == nrow(issp))  

saveRDS(merged, "data/interim/issp_merged.rds")
