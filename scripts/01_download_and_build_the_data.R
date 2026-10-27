#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 01_download_and_build_the_data
#' ------------------------------
getwd()
library(haven)
library(dplyr)
library(readr)

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
names(issp_ch1) <- tolower(names(issp_ch1))
names(issp_ch1)

issp_clean <- issp_ch1 %>%
  transmute(
    country    = recode(sub("^[0-9]+\\. [A-Z]{2}-", "", as.character(as_factor(country))),
                        "United States" = "USA", "Slovak Republic" = "Slovakia"),
    wave       = year,
    sss        = if_else(topbot %in% 1:10, topbot, NA_real_),
    credential = case_when(degree == 5 ~ 1, degree %in% 0:4 ~ 0),
    ind_belief = if_else(v6 %in% 1:5, 6 - v6, NA_real_),
    income     = factor(if_else(income %in% 1:3, income, NA_real_)),
    age        = if_else(age > 0, age, NA_real_),
    age_sq     = age^2,
    gender     = case_when(sex == 1 ~ 0, sex == 2 ~ 1),   # 1 = female
    isco88     = isco88
  ) %>%
  zap_labels()

attr(issp_ch1$v6, "labels")
attr(issp_ch1$degree, "labels")
attr(issp_ch1$topbot, "labels")

table(issp_ch1$v6, issp_clean$ind_belief, useNA = "ifany")
table(issp_ch1$degree, issp_clean$credential, useNA = "ifany")

tapply(issp_clean$sss, issp_clean$ind_belief, mean, na.rm = TRUE)
tapply(issp_clean$sss, issp_clean$credential, mean, na.rm = TRUE)

iskoisei <- readLines("data/raw/macro/iskoisei.sps")       
iskoisei <- gsub("recode @isko \\(|\\) into @isei\\.", "", iskoisei)           
isei_lookup <- read.table(text = gsub("=", " ", iskoisei), col.names = c("isco88", "isei"))

issp_clean <- issp_clean %>% left_join(isei_lookup, by = "isco88")
mean(is.na(issp_clean$isei)) 
nrow(issp_clean)

#' Country-level Variables

strat <- read_csv("data/raw/macro/oecd/age_of_first_selection.csv") %>%
  transmute(country, stratification = max(age_first_selection, na.rm = TRUE) - age_first_selection)

wdi <- read_csv("data/raw/macro/worldbank/wdi_gdp_tertiary_1995_2020.csv", show_col_types = FALSE) %>%
  mutate(country = recode(country,
                          "United States" = "USA", "United Kingdom" = "Great Britain",
                          "Czechia" = "Czech Republic", "Slovak Republic" = "Slovakia", 
                          "Korea, Rep." = "Korea")) 

swiid <- read_csv("data/raw/macro/swiid/swiid9_92_summary.csv", show_col_types = FALSE) %>%
  mutate(country = recode(country,
                          "United States" = "USA", "United Kingdom" = "Great Britain", "Turkey" = "Turkiye")) %>%
  select(country, wave = year, gini = gini_disp)

wdi   <- wdi   %>% rename(wave = year)

country_level <- wdi %>% 
  left_join(strat, by = "country") %>%
  left_join(swiid, by = c("country", "wave"))

head(country_level)
sum(is.na(country_level$stratification))    

country_level <- country_level %>%
  select(country, wave, gdp_pc, tertiary, gini, stratification)

country_level %>%
  filter(!is.na(stratification)) %>%
  distinct(country) %>%
  nrow()

setdiff(strat$country, wdi$country)     

saveRDS(country_level, "data/interim/country_level_vars.rds")

merged <- issp_clean %>%
  left_join(country_level, by = c("country", "wave"))

saveRDS(merged, "data/interim/issp_merged.rds")

