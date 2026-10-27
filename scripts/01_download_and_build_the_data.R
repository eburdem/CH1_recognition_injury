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
    ind_belief = if_else(v6 %in% 1:5, 6 - v6, NA_real_),
    isco88 = ISCO88
  )

issp_clean <- zap_labels(issp_clean)

attr(issp_ch1$v6, "labels")
attr(issp_ch1$DEGREE, "labels")
attr(issp_ch1$TOPBOT, "labels")

table(issp_ch1$v6, issp_clean$ind_belief, useNA = "ifany")
table(issp_ch1$DEGREE, issp_clean$credential, useNA = "ifany")

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

