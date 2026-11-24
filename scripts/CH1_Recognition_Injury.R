#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 00_setup
#' -------
#'   install.packages("tidyverse") 
#'   install.packages("dplyr")
#'   install.packages("readr")
#'   install.packages("ggplot2")
#'   install.packages("lme4")
#'   install.packages("lmerTest")
#'   install.packages("haven")
#'   install.packages("tidyr")
#'   install.packages("emmeans")
#'   install.packages("table1")

#'  renv::snapshot()

#' ------------------------------
#' 01_download_and_build_the_data
#' ------------------------------
library(haven)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(table1)
library(lme4)
library(lmerTest)
library(emmeans)

dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

#' ISSP: load, keep three waves, lowercase names once
issp_raw <- read_dta("data/raw/issp/cumulation/ZA8790_v1-0-0b.dta") %>%
  filter(year %in% c(1999, 2009, 2019)) # loads the data and filter the years
names(issp_raw) <- tolower(names(issp_raw)) # makes variable names lowercase

#' ISSP:recode
issp <- issp_raw %>%
  transmute(
    iso        = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))), # must stay above country
    country    = recode(sub("^[0-9]+\\. [A-Z]{2}-", "", as.character(as_factor(country))),
                        "United States" = "USA", "Slovak Republic" = "Slovakia"),
    id         = caseid, # respondent ID
    wave       = year, # survey year
    sss        = if_else(topbot %in% 1:10, topbot, NA_real_), # topbot is valid only for 1-10: everything else becomes NA   
    credential = case_when(degree == 5 ~ 1, degree %in% 0:4 ~ 0),   # DEGREE = 5 only; becomes credential_degree5 in script 03
    credential_alt = case_when(degree >= 4 ~ 1, degree %in% 0:3 ~ 0),   # post-secondary or higher
    ind_belief = if_else(v6 %in% 1:5, 6 - v6, NA_real_),            # reverses the hard-work item 5 = strongest endorsement
    income     = factor(if_else(income %in% 1:3, income, NA_real_)), # household income tercile as a factor (low, middle, high), so the model gets two dummies.
    age        = if_else(age > 0, age, NA_real_), # age must be positive
    age_sq     = age^2, # for quadratic term
    gender     = case_when(sex == 1 ~ 0, sex == 2 ~ 1),   # 1 = female
    isco88     = isco88 # occupation code, kept for ISEI lookup
  ) %>%
  zap_labels() # removes haven's value labels so columns are plain numbers

#' ISEI Lookup to turn ISCO*88 occupatıon code into an occupational status score
x <- readLines("data/raw/macro/iskoisei.sps") #reads it as text lines
x <- gsub("recode @isko \\(|\\) into @isei\\.", "", x) # strips the recode isco into isei
isei_lookup <- read.table(text = gsub("=", " ", x), col.names = c("isco88", "isei"))
issp <- left_join(issp, isei_lookup, by = "isco88") # attaches the ISEI score to each respondent.

#' Country-level Variables
strat <- read_csv("data/raw/macro/oecd/age_of_first_selection.csv", show_col_types = FALSE) %>%
  transmute(country, stratification = max(age_first_selection, na.rm = TRUE) - age_first_selection) # reads the OECD age of first selection -> higher = earlier selection = more stratified
name_fix <- c("United States" = "USA", "United Kingdom" = "Great Britain",
              "Czechia" = "Czech Republic", "Slovak Republic" = "Slovakia",
              "Korea, Rep." = "Korea", "Turkey" = "Turkiye")
wdi <- read_csv("data/raw/macro/worldbank/wdi_gdp_tertiary_1995_2020.csv", show_col_types = FALSE) %>%
  mutate(country = recode(country, !!!name_fix)) %>%
  select(country, wave = year, gdp_pc, tertiary) # World Bank 

swiid <- read_csv("data/raw/macro/swiid/swiid9_92_summary.csv", show_col_types = FALSE) %>%
  mutate(country = recode(country, !!!name_fix)) %>%
  select(country, wave = year, gini = gini_disp) # SWIID Gini by country per year

#' tertiary is NA for six country-waves in 2009 (Australia, Germany, Japan, NZ, Portugal, USA);
#' kept as NA by decision
country_level <- strat %>%
  left_join(wdi,   by = "country") %>%
  left_join(swiid, by = c("country", "wave")) #joins the three on country and wave for Gini. Stratification repeats across waves because it does not change.
#' guard checks
setdiff(unique(issp$country), strat$country)  #  lists ISSP countries missing from each country-level file.
setdiff(unique(issp$country), wdi$country)    
setdiff(unique(issp$country), swiid$country) 


merged <- left_join(issp, country_level, by = c("country", "wave")) # keeps every respondent and attaches country values.
stopifnot(nrow(merged) == nrow(issp))  # a join that finds two matching rows would silently duplicate respondents

stopifnot(!anyDuplicated(merged[c("wave", "iso", "id")])) #  (wave, country, id) must identify one respondent only

table(issp_raw$degree, issp$credential, useNA = "ifany")

#' ------------------------------
#' 02_degree_variable_built  (ONE-OFF, switched off)
#' ------------------------------
#' The ISSP `DEGREE` variable is not harmonized over time: 
#' in 2009 it was derived from national categories, and codes differ by country and wave
#' Builds the BLANK crosswalks that I filled in by hand into degree_crosswalk_final.csv.
#' Not needed to reproduce the analysis. Set to TRUE only to regenerate the blank files.
rebuild_blank_crosswalk <- FALSE

#' Wave files: needed by 02 and 03, so load them once here
w09 <- read_dta("data/raw/issp/2009/ZA5400_v4-0-0.dta")
w19 <- read_dta("data/raw/issp/2019/ZA7600_v3-0-0.dta")
names(w09) <- tolower(names(w09))
names(w19) <- tolower(names(w19))

if (rebuild_blank_crosswalk) {
  
  tab_one <- function(d, iso, wave) {
    nat  <- paste0(tolower(iso), "_degr")  # builds the national variable name
    sub  <- d[startsWith(as.character(d$c_alphan), iso), ] # selects the country
    labs <- attr(d[[nat]], "labels")      # value labels of the national variable
    sub %>%
      transmute(nat_code        = as.numeric(.data[[nat]]),
                degree_original = as.numeric(degree),
                educyrs         = as.numeric(educyrs)) %>%
      group_by(nat_code, degree_original) %>%
      summarise(n = n(),
                median_educyrs = median(educyrs[educyrs >= 0 & educyrs <= 30]),
                .groups = "drop") %>%
      mutate(wave = wave, iso = iso,
             nat_label = names(labs)[match(nat_code, labs)])
  }
  
  isos_2009 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","HU","LV","PL","SK","ES")
  isos_2019 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","AU","DE","JP","NZ","US")
  
  cw09 <- bind_rows(lapply(isos_2009, function(i) tab_one(w09, i, 2009)))
  cw19 <- bind_rows(lapply(isos_2019, function(i) tab_one(w19, i, 2019)))
  
  crosswalk <- bind_rows(cw09, cw19) %>%
    mutate(degree_cumulation = if_else(wave == 2019 & degree_original == 6, 5, degree_original),
           harmonized_tertiary = NA_real_,
           note = "")
  write_csv(crosswalk, "data/docs/degree_crosswalk_blank.csv")
  crosswalk %>% group_by(wave) %>% summarise(sum(n))
  
  isos_2009_extra <- c("AU","DE","JP","NZ","US")
  cw09_extra <- bind_rows(lapply(isos_2009_extra, function(i) tab_one(w09, i, 2009)))
  crosswalk_extra <- cw09_extra %>%
    mutate(degree_cumulation = degree_original,
           harmonized_tertiary = NA_real_,
           note = "")
  write_csv(crosswalk_extra, "data/docs/degree_crosswalk_2009_extra_blank.csv")
  crosswalk_extra %>% group_by(iso) %>% summarise(sum(n))
}

#' ------------------------------
#' 03_degree_variable_harmonized
#' ------------------------------
#' Uses the hand-filled crosswalk (1 = completed bachelor-level or higher, 0 = otherwise)
stopifnot(file.exists("data/docs/degree_crosswalk_final.csv"))
cw <- read_csv("data/docs/degree_crosswalk_final.csv", show_col_types = FALSE)

#' Country codes covered in each wave (same as in the crosswalk)
isos_2009 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","HU","LV","PL","SK","ES",
               "AU","DE","JP","NZ","US")
isos_2019 <- c("AT","CL","CZ","DK","FI","FR","GB","IS","IL","IT","LT","NO","SI","SE","CH","AU","DE","JP","NZ","US")

#' Respondent table: id is v3 in 2009 and caseid in 2019
resp_one <- function(d, iso, wave, idvar) { # builds a respondent table from the wave files
  nat <- paste0(tolower(iso), "_degr")
  d[startsWith(as.character(d$c_alphan), iso), ] %>%
    transmute(wave = wave, iso = iso, id = as.numeric(.data[[idvar]]),
              nat_code = as.numeric(.data[[nat]]),
              degree_original = as.numeric(degree))
}
resp <- bind_rows( # results stacked with this
  lapply(isos_2009, function(i) resp_one(w09, i, 2009, "v3")),
  lapply(isos_2019, function(i) resp_one(w19, i, 2019, "caseid"))
)
#' Above keeps each respondent's ID, national degree code and DEGREE.
#' The two lapply calls run it over every country and stack the results (a loop in short form).

#' Join the crosswalk, with a guard that no rows were added or lost
n_before <- nrow(resp)
resp <- resp %>%
  left_join(cw %>% select(wave, iso, nat_code, degree_original, harmonized_tertiary),
            by = c("wave", "iso", "nat_code", "degree_original"))
stopifnot(nrow(resp) == n_before)
resp <- resp %>%
  mutate(harmonized_tertiary = if_else(degree_original %in% 0:6, harmonized_tertiary, NA_real_))
stopifnot(!anyDuplicated(resp[c("wave", "iso", "id")]))

#' Attach to the merged data (many-to-one on wave, iso, id) and rename
harm <- resp %>% select(wave, iso, id, harmonized_tertiary)
analysis <- merged %>%
  left_join(harm, by = c("wave", "iso", "id"), relationship = "many-to-one") 

stopifnot(nrow(analysis) == nrow(merged))
stopifnot(sum(!is.na(analysis$harmonized_tertiary)) == sum(!is.na(harm$harmonized_tertiary)))

analysis <- analysis %>%
  rename(credential_degree5 = credential, credential = harmonized_tertiary)

sum(!is.na(analysis$credential))     # should be a large number (about 6,646 of 6,732 for the five 2009 countries alone)
table(analysis$wave[analysis$wave %in% c(2009, 2019)], is.na(analysis$credential[analysis$wave %in% c(2009, 2019)]))

#' ---------
#' 04_checks
#' ---------

merged <- analysis

#' Old vs new recodes
table(issp_raw$v6, merged$ind_belief, useNA = "ifany")
table(issp_raw$degree, merged$credential, useNA = "ifany")
table(issp_raw$degree, merged$credential_degree5, useNA = "ifany")
table(issp_raw$income, merged$income, useNA = "ifany")
table(issp_raw$sex, merged$gender, useNA = "ifany")
summary(issp_raw$age) 
summary(merged$age)
summary(merged$age_sq)

#' Direction Checks - mean SSS per group - SSS should be higher for the credentialed and for stronger belief.
tapply(merged$sss, merged$ind_belief, mean, na.rm = TRUE) 
tapply(merged$sss, merged$credential, mean, na.rm = TRUE)
tapply(merged$ind_belief, merged$credential, mean, na.rm = TRUE)

#' Structure
nrow(merged) == nrow(issp_raw)                     
table(merged$wave)                              
table(merged$country, merged$wave) # which country-waves exist
table(merged$wave, is.na(merged$ind_belief))    
stopifnot(anyDuplicated(country_level[, c("country", "wave")]) == 0)
stopifnot(!anyDuplicated(merged[c("wave", "iso", "id")]))
sort(unique(merged$country))

#' Missingness
colMeans(is.na(merged)) # share missing per variable
summary(merged$sss)
summary(merged$ind_belief)
summary(merged$isei)
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
  print(n = 35) # missingness of country-level variables by country

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

table(issp_raw$degree, merged$credential_alt, useNA = "ifany")

#' ------------------
#' 05_sample_analysis
#' ------------------
#' d2 and d3 are the data frames that part 05 builds from merged 
#' by filtering step by step. Each one is a smaller version of the one before

#' Sample: filters
model_vars <- c("sss", "credential", "ind_belief", "income", "isei", "age", "gender")

d1 <- merged %>% filter(wave %in% c(2009, 2019))   # Respondents in the two waves I use
nrow(d1) # 1999 is dropped because it has no hard-work belief item.
d2 <- d1 %>% filter(!is.na(stratification)) # keeping only countries that have a stratification value       
nrow(d2) # H2 needs the OECD stratification value, so countries without one (Bulgaria, Croatia, Russia and so on) drop out.
n_distinct(d2$country)  
d3 <- d2 %>% drop_na(all_of(model_vars)) # keeping only respondents with no missing value on any model variable
#' d3 is the analytic sample.
nrow(d3)               # 42714
n_distinct(d3$country) # 25
setdiff(unique(d2$country), unique(d3$country))   # should be "Portugal" because it has no crosswalk entry and so no credential.

#' Attrition table
attrition <- data.frame(
  step = c("All rows",
           "Waves 2009 + 2019",
           "Has stratification value",
           "Complete on model variables"),
  n = c(nrow(merged), nrow(d1), nrow(d2), nrow(d3)),
  countries = c(n_distinct(merged$country), n_distinct(d1$country),
                n_distinct(d2$country), n_distinct(d3$country))
) # The attrition table records n and countries after each step
attrition$lost <- c(NA, -diff(attrition$n))   #  gives rows lost per step. 
attrition

setdiff(unique(d1$country), unique(d2$country))  # shows which countries each step removes.
setdiff(unique(d2$country), unique(d3$country)) 

write_csv(attrition, "output/tables/attrition.csv")

#' Missingness
colMeans(is.na(d2[model_vars])) * 100  # is the percent missing per variable
colSums(is.na(d2[model_vars]))

d2 %>%
  group_by(country) %>%
  summarise(across(c(income, isei, tertiary), ~ mean(is.na(.x)) * 100)) %>%
  print(n = 30)

d2 %>%
  group_by(wave) %>%
  summarise(across(c(income, isei, tertiary), ~ mean(is.na(.x)) * 100))

#' Retention by country
retention <- d2 %>% 
  count(country, name = "n_d2") %>%
  left_join(count(d3, country, name = "n_d3"), by = "country") %>%
  mutate(n_d3 = coalesce(n_d3, 0L), 
         pct_kept = round(100 * n_d3 / n_d2, 1)) %>%
  arrange(pct_kept)
print(retention, n=30) # The retention table is count(country) joined to count(d3, country)

write_csv(retention, "output/tables/country_retention.csv")

#' Sensitivity sample with credential_alt instead of credential
d2 %>% drop_na(all_of(c(setdiff(model_vars, "credential"), "credential_alt"))) %>% nrow()

#' Selection across steps
compare <- function(d) c(n = nrow(d), 
                         sss = mean(d$sss, na.rm = TRUE),
                         share_tertiary = mean(d$credential, na.rm = TRUE),
                         age = mean(d$age, na.rm = TRUE)) # is a small function returning n, mean SSS, share tertiary and mean age for a data frame
rbind(d1 = compare(d1), d2 = compare(d2), d3 = compare(d3))

d1 %>% filter(is.na(stratification)) %>% summarise(mean_sss = mean(sss, na.rm = TRUE))

#' ----------------
#' 06_descriptives
#' ----------------

#' Table 1: categorical variables (% of respondents)

for (v in c("credential", "income", "gender", "wave")) {
  cat("\n", v, "\n")
  print(round(100 * prop.table(table(d3[[v]])), 1))
}

#' Table 1: number of observations per country and wave
table1_obs <- d3 %>%
  count(country, wave) %>%
  pivot_wider(names_from = wave, values_from = n, values_fill = 0) %>%
  mutate(Total = `2009` + `2019`) %>%
  arrange(country)

table1_obs
write_csv(table1_obs, "output/tables/table1_obs_per_country_wave.csv")

#' Group Descriptives

#' Raw credential gap
d3 %>% group_by(credential) %>%
  summarise(mean_sss = mean(sss), n = n())

#' Credential x belief (do the lines fan out?)
d3 %>% group_by(credential, ind_belief) %>%
  summarise(mean_sss = mean(sss), n = n(), .groups = "drop")

#' Correlations among main variables
cor(d3[c("sss", "credential", "ind_belief", "isei", "age")])

d3 %>%
  group_by(country) %>%
  summarise(n = n(),
            mean_sss = mean(sss),
            pct_tertiary = 100 * mean(credential),
            stratification = first(stratification)) %>%
  arrange(desc(stratification)) %>%
  print(n = 30)

#' country-level variables by wave (stratification is constant; the others vary)
d3 %>% distinct(country, wave, stratification, gini, gdp_pc, tertiary) %>%
  arrange(country, wave) %>% print(n = 50)

d3 %>% group_by(country) %>%
  summarise(pct_tert = 100 * mean(credential), pct_alt = 100 * mean(credential_alt)) %>%
  arrange(desc(pct_alt - pct_tert)) %>% print(n = 30)

#' table 2: Descriptives
d3_t1 <- d3     

d3_t1$credential_f <- factor(d3_t1$credential, levels = 0:1, labels = c("No tertiary degree", "Tertiary degree"))
d3_t1$gender_f     <- factor(d3_t1$gender,     levels = 0:1, labels = c("Male", "Female"))
d3_t1$income_f     <- factor(d3_t1$income,     levels = 1:3, labels = c("Low", "Middle", "High"))

label(d3_t1$sss)          <- "Subjective social status (1-10)"
label(d3_t1$ind_belief)   <- "Meritocratic belief (1-5)"
label(d3_t1$isei)         <- "Occupational status (ISEI)"
label(d3_t1$age)          <- "Age"
label(d3_t1$credential_f) <- "Credential"
label(d3_t1$gender_f)     <- "Gender"
label(d3_t1$income_f)     <- "Household income tercile"

t1_descriptives <- table1(~ sss + ind_belief + isei + age + credential_f + gender_f + income_f | factor(wave),
             data = d3_t1,
             render.continuous = c(.= "Mean (SD)", .= "[Min, Max]"))

t1_descriptives                       

write_csv(as.data.frame(t1_descriptives), "output/tables/table2_descriptives.csv")

#' ------------------
#' 07_educyrs_variant
#' ------------------

#' print(attr(issp_raw$educyrs, "labels"))   # check what 95/96 mean

ey <- issp_raw %>%
  transmute(iso  = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))),
            id   = caseid,
            wave = year,
            educyrs = if_else(educyrs >= 0 & educyrs <= 30, educyrs, NA_real_)) %>%
  zap_labels() # builds a small table of (iso, id, wave, educyrs) from the raw data, keeping only years between 0 and 30 

n_before <- nrow(analysis)
analysis <- analysis %>%
  left_join(ey, by = c("iso", "id", "wave"), relationship = "one-to-one")
stopifnot(nrow(analysis) == n_before) # attaches it

analysis <- analysis %>%
  mutate(educyrs_cube = (educyrs / 10)^3)      # scaled so coefficients are readable


#' Checks: is EDUCYRS stable across waves where DEGREE was not?
analysis %>%
  filter(wave %in% c(2009, 2019), !is.na(stratification)) %>%
  group_by(country, wave) %>%
  summarise(mean_years = mean(educyrs, na.rm = TRUE),
            pct_missing = 100 * mean(is.na(educyrs)),
            pct_tertiary = 100 * mean(credential, na.rm = TRUE), .groups = "drop") %>%
  print(n = 60)

#' ------------
#' 08_centering
#' ------------


#' 1. Country means (one value per country)
d3 <- d3 %>%
  group_by(country) %>%
  mutate(cred_cmean = mean(credential),
         bel_cmean  = mean(ind_belief),
         isei_cmean = mean(isei),
         gini_cm    = mean(gini, na.rm = TRUE),
         gdp_cm     = mean(log(gdp_pc), na.rm = TRUE),
         tert_cm    = mean(tertiary, na.rm = TRUE)) %>%
  ungroup()

#' One row per country, used to center on the mean of the country values
cm <- d3 %>%
  distinct(country, cred_cmean, bel_cmean, isei_cmean, gini_cm, gdp_cm, tert_cm, stratification)
stopifnot(nrow(cm) == n_distinct(d3$country))     # fails if a "country-level" value varies within a country

#' 2. Within parts (version WITHIN) and between parts (both versions)
d3 <- d3 %>%
  mutate(credential_w = credential - cred_cmean,
         belief_w     = ind_belief - bel_cmean,
         isei_w       = isei - isei_cmean,
         credential_b = cred_cmean - mean(cm$cred_cmean),
         belief_b     = bel_cmean  - mean(cm$bel_cmean),
         isei_b       = isei_cmean - mean(cm$isei_cmean))

#' 3. Other centered variables
d3 <- d3 %>%
  mutate(stratification_c = stratification - mean(cm$stratification),
         gini_c           = gini_cm - mean(cm$gini_cm),
         gdp_c            = gdp_cm  - mean(cm$gdp_cm),
         tertiary_c       = tert_cm - mean(cm$tert_cm),
         age10            = (age - 50) / 10,
         age10_sq         = age10^2)

#' 4. Version B: within country-wave centering (robustness)
d3 <- d3 %>%
  group_by(country, wave) %>%
  mutate(credential_w_cw = credential - mean(credential),
         belief_w_cw     = ind_belief - mean(ind_belief)) %>%
  ungroup()

#' 5. Checks
stopifnot(max(abs(tapply(d3$credential_w, d3$country, mean))) < 1e-10)   # within parts average 0 in each country
stopifnot(max(abs(tapply(d3$belief_w,     d3$country, mean))) < 1e-10)
stopifnot(max(abs(tapply(d3$isei_w,       d3$country, mean))) < 1e-10)
stopifnot(all(tapply(d3$credential_b, d3$country, function(x) length(unique(x))) == 1))   # between part constant in a country
stopifnot(!anyNA(d3[c("credential_w", "belief_w", "isei_w", "credential_b", "belief_b", "isei_b",
                      "stratification_c", "gini_c", "gdp_c", "tertiary_c", "age10", "age10_sq")]))

cor(d3[c("credential", "credential_w", "credential_b")])    # raw is the sum of the within and between parts
cor(d3[c("ind_belief", "belief_w", "belief_b")])
d3 %>% group_by(country) %>%
  summarise(sd_diff = sd(credential_w - credential_w_cw)) %>% print(n = 30)

# WITHIN version (current): credential_w * belief_w + credential_b + belief_b
# RAW version (Anthony):    credential   * ind_belief + credential_b + belief_b

saveRDS(d3, "data/processed/d3_models.rds")

#' -------------
#' 09_null_model
#' -------------

#' m0: random intercept for country
m0 <- lmer(sss ~ 1 + (1 | country), data = d3)
summary(m0)

vc  <- as.data.frame(VarCorr(m0))
icc <- vc$vcov[vc$grp == "country"] / sum(vc$vcov)
icc    # share of SSS variance between countries (about 0.161)

#' m0b: adds a random intercept for each country-wave
m0b <- lmer(sss ~ 1 + (1 | country) + (1 | country:wave), data = d3)
summary(m0b)

vc2 <- as.data.frame(VarCorr(m0b))
vc2$share <- vc2$vcov / sum(vc2$vcov)
vc2[, c("grp", "vcov", "share")]

anova(m0, m0b)     # does the country-wave level improve the fit?


#' -----------------
#' 10_baseline_model
#' -----------------

#' WITHIN version (main): credential_w + credential_b
#' m1: credential only (within and between), wave as fixed effect
m1 <- lmer(sss ~ credential_w + credential_b + factor(wave) +
             (1 + credential_w | country), data = d3)
summary(m1)


#' m2: m1 + individual controls (ISEI within and between, age in decades, gender, income)
m2 <- lmer(sss ~ credential_w + credential_b + factor(wave) +
             income + isei_w + isei_b + age10 + age10_sq + gender +
             (1 + credential_w | country), data = d3)
summary(m2)

#' RAW version: same models with credential uncentered
m1_raw <- lmer(sss ~ credential + credential_b + factor(wave) +
                 (1 + credential | country), data = d3)

m2_raw <- lmer(sss ~ credential + credential_b + factor(wave) +
                 income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential | country), data = d3)

#' Compare the credential slope in the two versions (should be almost the same)
summary(m2)$coefficients["credential_w", ]
summary(m2_raw)$coefficients["credential", ]

isSingular(m1)
isSingular(m2)     # FALSE means the random slope is estimated properly


#' ---------------
#' 11_interactions
#' ---------------
#' Models WITHOUT country controls 

#' WITHIN version (main)
#' H1: credential x individual belief
m_h1 <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
               factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
               (1 + credential_w | country), data = d3)
summary(m_h1)

#' H2: credential x stratification
m_h2 <- lmer(sss ~ credential_w * stratification_c + credential_b +
               factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
               (1 + credential_w | country), data = d3)
summary(m_h2)

#' H3: three-way interaction
m_h3 <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
               factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
               (1 + credential_w | country), data = d3)
summary(m_h3)

#' RAW version (credential and belief not centered), same models
m_h1_raw <- lmer(sss ~ credential * ind_belief + credential_b + belief_b +
                   factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                   (1 + credential | country), data = d3)

m_h2_raw <- lmer(sss ~ credential * stratification_c + credential_b +
                   factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                   (1 + credential | country), data = d3)

m_h3_raw <- lmer(sss ~ credential * ind_belief * stratification_c + credential_b + belief_b +
                   factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                   (1 + credential | country), data = d3)


#' The three key interaction rows (estimate, SE, df, t, p), both versions
main_rows <- rbind(
  H1     = summary(m_h1)$coefficients["credential_w:belief_w", ],
  H2     = summary(m_h2)$coefficients["credential_w:stratification_c", ],
  H3     = summary(m_h3)$coefficients["credential_w:belief_w:stratification_c", ],
  H1_raw = summary(m_h1_raw)$coefficients["credential:ind_belief", ],
  H2_raw = summary(m_h2_raw)$coefficients["credential:stratification_c", ],
  H3_raw = summary(m_h3_raw)$coefficients["credential:ind_belief:stratification_c", ])
main_rows
write.csv(main_rows, "output/tables/main_interactions.csv")

isSingular(m_h1); isSingular(m_h2); isSingular(m_h3)

#' -------------------
#' 12_country_controls
#' -------------------
#' Same models as in 11_interactions plus the country controls gini_c + gdp_c + tertiary_c
#' (all built in 08_centering). These are the MAIN models.

stopifnot(!anyNA(d3[c("gini_c", "gdp_c", "tertiary_c")]))

#' WITHIN version (main)
m_h1_ctrl <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w | country), data = d3)

m_h2_ctrl <- lmer(sss ~ credential_w * stratification_c + credential_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w | country), data = d3)

m_h3_ctrl <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w | country), data = d3)

#' RAW version (credential and belief not centered)
m_h1_ctrl_raw <- lmer(sss ~ credential * ind_belief + credential_b + belief_b +
                        gini_c + gdp_c + tertiary_c +
                        factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                        (1 + credential | country), data = d3)

m_h2_ctrl_raw <- lmer(sss ~ credential * stratification_c + credential_b +
                        gini_c + gdp_c + tertiary_c +
                        factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                        (1 + credential | country), data = d3)

m_h3_ctrl_raw <- lmer(sss ~ credential * ind_belief * stratification_c + credential_b + belief_b +
                        gini_c + gdp_c + tertiary_c +
                        factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                        (1 + credential | country), data = d3)


ctrl_rows <- rbind(
  H1     = summary(m_h1_ctrl)$coefficients["credential_w:belief_w", ],
  H2     = summary(m_h2_ctrl)$coefficients["credential_w:stratification_c", ],
  H3     = summary(m_h3_ctrl)$coefficients["credential_w:belief_w:stratification_c", ],
  H1_raw = summary(m_h1_ctrl_raw)$coefficients["credential:ind_belief", ],
  H2_raw = summary(m_h2_ctrl_raw)$coefficients["credential:stratification_c", ],
  H3_raw = summary(m_h3_ctrl_raw)$coefficients["credential:ind_belief:stratification_c", ])
ctrl_rows
write.csv(ctrl_rows, "output/tables/controls_interactions.csv")

isSingular(m_h1_ctrl); isSingular(m_h2_ctrl); isSingular(m_h3_ctrl)

#' ------------------------------
#' 13_random_slopes
#' ------------------------------
#' Anthony: add a random slope for belief_w, and one for credential_w x belief_w.
#' Three versions of the random part are compared for H1 and H3 (stratification is country-level,
#' so it cannot have a random slope, and H2 stays as it is).

f_h1 <- sss ~ credential_w * belief_w + credential_b + belief_b +
  gini_c + gdp_c + tertiary_c +
  factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender

f_h3 <- sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
  gini_c + gdp_c + tertiary_c +
  factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender

fit_slopes <- function(fixed, data) {
  list(cred         = lmer(update(fixed, . ~ . + (1 + credential_w | country)), data = data),
       cred_bel     = lmer(update(fixed, . ~ . + (1 + credential_w + belief_w | country)), data = data),
       cred_bel_int = lmer(update(fixed, . ~ . + (1 + credential_w * belief_w | country)), data = data))
} 
# cred is: (1 + credential_w | country) 
# cred_bel adds a random slope for belief
# cred_bel_int adds the credential × belief slope as well

rs_h1 <- fit_slopes(f_h1, d3)
rs_h3 <- fit_slopes(f_h3, d3)

m_h1_ctrl_cred <- m_h1_ctrl            # keep the old versions (credential slope only)
m_h3_ctrl_cred <- m_h3_ctrl

#' New main models: random slopes for credential and belief
m_h1_ctrl <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w + belief_w | country), data = d3)

m_h3_ctrl <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w + belief_w | country), data = d3)

#' Check: these must match the cred_bel versions from the comparison
summary(m_h1_ctrl)$coefficients["credential_w:belief_w", ]       # estimate about 0.0443, p about 0.018
summary(m_h3_ctrl)$coefficients["credential_w:belief_w:stratification_c", ]   # estimate about 0.0091, p about 0.28
isSingular(m_h1_ctrl)
isSingular(m_h3_ctrl)

#' Compare: fit, singular fit, and the key interaction in each version
slope_summary <- function(rs, term) {
  data.frame(random_part = names(rs),
             singular    = sapply(rs, isSingular), # version asks for more than 25 countries can support
             AIC         = sapply(rs, AIC), # BIC penalizes the extra parameters more, so it favours the simpler model sooner.
             BIC         = sapply(rs, BIC),
             estimate    = sapply(rs, function(m) summary(m)$coefficients[term, "Estimate"]),
             SE          = sapply(rs, function(m) summary(m)$coefficients[term, "Std. Error"]),
             p           = sapply(rs, function(m) summary(m)$coefficients[term, "Pr(>|t|)"]),
             row.names = NULL)
}

slopes_h1 <- slope_summary(rs_h1, "credential_w:belief_w")
slopes_h3 <- slope_summary(rs_h3, "credential_w:belief_w:stratification_c")
slopes_h1
slopes_h3

anova(rs_h1$cred, rs_h1$cred_bel, rs_h1$cred_bel_int)    # nested; boundary caveat applies
anova(rs_h3$cred, rs_h3$cred_bel, rs_h3$cred_bel_int)

write_csv(slopes_h1, "output/tables/random_slopes_h1.csv")
write_csv(slopes_h3, "output/tables/random_slopes_h3.csv")

#' ------------------------------
#' 14_marginal_effects
#' ------------------------------

ci_level  <- 0.835    # confidence level of the plotted bands (figures)
tab_level <- 0.95     # confidence level of the tables

#' Values where the effect is read
sd_b   <- sd(d3$belief_w)
b_lim  <- quantile(d3$belief_w, c(0.05, 0.95))               # plot only the 5th-95th percentile of belief
b_grid <- seq(b_lim[1], b_lim[2], length.out = 17)
s_grid <- seq(min(d3$stratification_c), max(d3$stratification_c), length.out = 20)

cm_strat <- d3 %>% distinct(country, stratification_c)       # one value per country
lo <- quantile(cm_strat$stratification_c, 0.10)              # 10th percentile country
hi <- quantile(cm_strat$stratification_c, 0.90)              # 90th percentile country

#' ---- TABLES (95% intervals) ----

#' H1: credential gap at belief -1 SD, country mean, +1 SD (within) and belief 1 to 5 (raw)
h1_table <- as.data.frame(
  confint(emtrends(m_h1_ctrl, ~ belief_w, var = "credential_w",
                   at = list(belief_w = c(-sd_b, 0, sd_b))), level = tab_level))

h1_raw_table <- as.data.frame(
  confint(emtrends(m_h1_ctrl_raw, ~ ind_belief, var = "credential",
                   at = list(ind_belief = 1:5)), level = tab_level))

h1_all <- rbind(
  data.frame(version = "Within (belief centered)",
             belief  = c("-1 SD", "country mean", "+1 SD"),
             gap = h1_table$credential_w.trend, SE = h1_table$SE,
             lower = h1_table$asymp.LCL, upper = h1_table$asymp.UCL),
  data.frame(version = "Raw (belief 1-5)",
             belief  = paste("belief =", 1:5),
             gap = h1_raw_table$credential.trend, SE = h1_raw_table$SE,
             lower = h1_raw_table$asymp.LCL, upper = h1_raw_table$asymp.UCL))
h1_all$ci_level <- tab_level
h1_all
write_csv(h1_all, "output/tables/h1_marginal_effects.csv")

#' H2: credential gap at the lowest, average and highest stratification
h2_table <- as.data.frame(
  confint(emtrends(m_h2_ctrl, ~ stratification_c, var = "credential_w",
                   at = list(stratification_c = c(min(d3$stratification_c), 0, max(d3$stratification_c)))),
          level = tab_level))
h2_table$ci_level <- tab_level
h2_table
write_csv(h2_table, "output/tables/h2_marginal_effects.csv")

#' H3: credential gap at belief -1 SD, mean, +1 SD, in low and high stratification countries
h3_table <- as.data.frame(
  confint(emtrends(m_h3_ctrl, ~ stratification_c, var = "credential_w",
                   at = list(belief_w = c(-sd_b, 0, sd_b), stratification_c = c(lo, hi)),
                   by = "belief_w"), level = tab_level))
h3_table$ci_level <- tab_level
h3_table
write_csv(h3_table, "output/tables/h3_marginal_effects.csv")

#' ---- FIGURES (83.5% bands, same vertical range in all three) ----

#' Figure 1: H1
h1 <- as.data.frame(
  confint(emtrends(m_h1_ctrl, ~ belief_w, var = "credential_w",
                   at = list(belief_w = b_grid)), level = ci_level))

ggplot(h1, aes(belief_w, credential_w.trend)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  geom_line() +
  labs(x = "Meritocratic belief (within country, centered)",
       y = "Credential gap in SSS",
       caption = paste0("Shaded band: ", ci_level * 100, "% CI")) +
  coord_cartesian(ylim = c(0.2, 0.9)) +
  theme_minimal()
ggsave("output/figures/fig1_h1_marginal.png", width = 6, height = 4, dpi = 300)

#' Figure 2: H2
h2 <- as.data.frame(
  confint(emtrends(m_h2_ctrl, ~ stratification_c, var = "credential_w",
                   at = list(stratification_c = s_grid)), level = ci_level))

ggplot(h2, aes(stratification_c, credential_w.trend)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  geom_line() +
  labs(x = "Educational stratification (centered)",
       y = "Credential gap in SSS",
       caption = paste0("Shaded band: ", ci_level * 100, "% CI")) +
  coord_cartesian(ylim = c(0.2, 0.9)) +
  theme_minimal()
ggsave("output/figures/fig2_h2_marginal.png", width = 6, height = 4, dpi = 300)

#' Figure 3: H3 (Figure 1 repeated at low and high stratification)
h3 <- as.data.frame(
  confint(emtrends(m_h3_ctrl, ~ belief_w | stratification_c, var = "credential_w",
                   at = list(belief_w = b_grid, stratification_c = c(lo, hi))), level = ci_level))
h3$stratification <- factor(h3$stratification_c, levels = c(lo, hi),
                            labels = c("Low stratification (10th percentile)",
                                       "High stratification (90th percentile)"))

ggplot(h3, aes(belief_w, credential_w.trend)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ stratification) +
  labs(x = "Meritocratic belief (within country, centered)",
       y = "Credential gap in SSS",
       caption = paste0("Shaded band: ", ci_level * 100, "% CI")) +
  coord_cartesian(ylim = c(0.2, 0.9)) +
  theme_minimal()
ggsave("output/figures/fig3_h3_marginal.png", width = 8, height = 4, dpi = 300)

#' Save the main models (the old models_ctrl.rds from part 12 still holds the credential-only versions)
saveRDS(list(m_h1_ctrl = m_h1_ctrl, m_h2_ctrl = m_h2_ctrl, m_h3_ctrl = m_h3_ctrl,
             m_h1_ctrl_raw = m_h1_ctrl_raw, m_h2_ctrl_raw = m_h2_ctrl_raw, m_h3_ctrl_raw = m_h3_ctrl_raw),
        "data/processed/models_ctrl.rds")


#' ------------------------------
#' 15_stepwise_regression_table
#' ------------------------------
#' Columns: M0 null, M0b + country-wave level, M1 basic, M2 individual controls,
#'          H1, H2, H3 = interaction models with country controls (the main models)

coef_table <- function(models) {
  one <- function(nm) {
    s <- summary(models[[nm]])$coefficients
    stars <- ifelse(s[, "Pr(>|t|)"] < .001, "***",
                    ifelse(s[, "Pr(>|t|)"] < .01,  "**",
                           ifelse(s[, "Pr(>|t|)"] < .05,  "*", "")))
    data.frame(term  = rownames(s),
               model = nm,
               cell  = paste0(sprintf("%.3f", s[, "Estimate"]), stars,
                              " (", sprintf("%.3f", s[, "Std. Error"]), ")"))
  }
  bind_rows(lapply(names(models), one)) %>%
    pivot_wider(names_from = model, values_from = cell)
}

models <- list(M0 = m0, M0b = m0b, M1 = m1, M2 = m2,
               H1 = m_h1_ctrl, H2 = m_h2_ctrl, H3 = m_h3_ctrl)

table3 <- coef_table(models)

#' Row order and readable labels
term_order <- c("(Intercept)", "credential_w", "credential_b", "belief_w", "belief_b",
                "stratification_c", "credential_w:belief_w", "credential_w:stratification_c",
                "belief_w:stratification_c", "credential_w:belief_w:stratification_c",
                "gini_c", "gdp_c", "tertiary_c", "factor(wave)2019", "income2", "income3",
                "isei_w", "isei_b", "age10", "age10_sq", "gender")
stopifnot(all(table3$term %in% term_order))        # fails if a term is missing from the order list
table3 <- table3 %>% arrange(match(term, term_order))

term_labels <- c("(Intercept)" = "Intercept",
                 "credential_w" = "Credential (within country)",
                 "credential_b" = "Credential (country mean)",
                 "belief_w" = "Meritocratic belief (within country)",
                 "belief_b" = "Meritocratic belief (country mean)",
                 "stratification_c" = "Educational stratification",
                 "credential_w:belief_w" = "Credential x belief (H1)",
                 "credential_w:stratification_c" = "Credential x stratification (H2)",
                 "belief_w:stratification_c" = "Belief x stratification",
                 "credential_w:belief_w:stratification_c" = "Credential x belief x stratification (H3)",
                 "gini_c" = "Gini (country)", "gdp_c" = "Log GDP per capita (country)",
                 "tertiary_c" = "Tertiary share (country)", "factor(wave)2019" = "Wave 2019",
                 "income2" = "Income: middle", "income3" = "Income: high",
                 "isei_w" = "ISEI (within country)", "isei_b" = "ISEI (country mean)",
                 "age10" = "Age (decades from 50)", "age10_sq" = "Age squared", "gender" = "Female")
table3$term <- unname(term_labels[table3$term])

#' Bottom rows: random effects and model information
get_sd <- function(m, grp, var) {
  vc <- as.data.frame(VarCorr(m))
  x  <- vc$sdcor[vc$grp == grp & vc$var1 %in% var & is.na(vc$var2)]
  if (length(x) == 0) NA else x
}
fmt <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))

bottom <- data.frame(term = c("SD country intercept", "SD credential slope", "SD belief slope",
                              "SD country-wave intercept", "SD residual",
                              "Observations", "Countries", "AIC"),
                     stringsAsFactors = FALSE)
for (nm in names(models)) {
  m <- models[[nm]]
  bottom[[nm]] <- c(fmt(get_sd(m, "country", "(Intercept)")),
                    fmt(get_sd(m, "country", "credential_w")),
                    fmt(get_sd(m, "country", "belief_w")),
                    fmt(get_sd(m, "country:wave", "(Intercept)")),
                    fmt(sigma(m)),
                    as.character(nobs(m)),
                    as.character(ngrps(m)[["country"]]),
                    as.character(round(AIC(m))))
}

table3_full <- rbind(table3, bottom)
table3_full[is.na(table3_full)] <- ""
table3_full
write.csv(table3_full, "output/tables/table3_regression.csv", row.names = FALSE)

#' Appendix: same three interaction models with raw (uncentered) credential and belief
models_raw <- list(H1_raw = m_h1_ctrl_raw, H2_raw = m_h2_ctrl_raw, H3_raw = m_h3_ctrl_raw)
table_raw <- coef_table(models_raw)
table_raw[is.na(table_raw)] <- ""
write.csv(table_raw, "output/tables/table_raw_appendix.csv", row.names = FALSE)

#' ------------------------
#' 16_leave_one_country_out
#' ------------------------
#' (uses the main models from part 13)

countries <- sort(unique(as.character(d3$country)))

loo <- function(model, term) {
  bind_rows(lapply(countries, function(cn) {
    s <- summary(update(model, data = filter(d3, country != cn)))$coefficients
    data.frame(dropped = cn, estimate = s[term, "Estimate"], p = s[term, "Pr(>|t|)"])
  }))
}

loo_h1 <- loo(m_h1_ctrl, "credential_w:belief_w")
loo_h2 <- loo(m_h2_ctrl, "credential_w:stratification_c")
loo_h3 <- loo(m_h3_ctrl, "credential_w:belief_w:stratification_c")

write_csv(loo_h1, "output/tables/loo_h1.csv")
write_csv(loo_h2, "output/tables/loo_h2.csv")
write_csv(loo_h3, "output/tables/loo_h3.csv")

#' Results
loo_h1 %>% arrange(desc(p));    range(loo_h1$estimate); sum(loo_h1$p < .05)
loo_h2 %>% arrange(estimate);   range(loo_h2$estimate); sum(loo_h2$p < .05)
loo_h3 %>% arrange(p);          range(loo_h3$estimate); sum(loo_h3$p < .05)


#' -------------
#' 17_robustness
#' -------------
#' Every check refits the three main models (country controls, credential and belief slopes)
#' and records the H1, H2, H3 interaction estimates and p-values in one table, `rob`.

#' ---- Helpers (each defined once) ----

#' estimate and p of one coefficient
pick <- function(m, term) {
  s <- summary(m)$coefficients
  c(s[term, "Estimate"], s[term, "Pr(>|t|)"])
}

#' one table row from three fitted models; cr / be = names of the credential and belief terms
rob_row <- function(label, m1, m2, m3, cr = "credential_w", be = "belief_w") {
  a  <- pick(m1, paste0(cr, ":", be))
  b  <- pick(m2, paste0(cr, ":stratification_c"))
  c3 <- pick(m3, paste0(cr, ":", be, ":stratification_c"))
  data.frame(check = label, n = nobs(m1),
             h1_est = a[1], h1_p = a[2], h2_est = b[1], h2_p = b[2], h3_est = c3[1], h3_p = c3[2],
             row.names = NULL)
}

#' same models, different data
rob_data <- function(label, d) {
  rob_row(label, update(m_h1_ctrl, data = d), update(m_h2_ctrl, data = d), update(m_h3_ctrl, data = d))
}

#' rename variables inside a model formula (alternative credential or belief measures)
swap_vars <- function(m, map) {
  f <- paste(deparse(formula(m), width.cutoff = 500), collapse = " ")
  for (v in names(map)) f <- gsub(v, map[[v]], f, fixed = TRUE)
  as.formula(f)
}

#' same models, variables renamed, on data d
rob_swap <- function(label, d, map, cr, be = "belief_w") {
  rob_row(label,
          lmer(swap_vars(m_h1_ctrl, map), data = d),
          lmer(swap_vars(m_h2_ctrl, map), data = d),
          lmer(swap_vars(m_h3_ctrl, map), data = d),
          cr = cr, be = be)
}

#' recompute within/between parts on a subsample (same logic as part 08)
recenter <- function(d) {
  d <- d %>% group_by(country) %>%
    mutate(cred_cmean = mean(credential), bel_cmean = mean(ind_belief), isei_cmean = mean(isei)) %>%
    ungroup()
  cm_s <- d %>% distinct(country, cred_cmean, bel_cmean, isei_cmean)
  d %>% mutate(credential_w = credential - cred_cmean,
               belief_w     = ind_belief - bel_cmean,
               isei_w       = isei - isei_cmean,
               credential_b = cred_cmean - mean(cm_s$cred_cmean),
               belief_b     = bel_cmean  - mean(cm_s$bel_cmean),
               isei_b       = isei_cmean - mean(cm_s$isei_cmean))
}

#' p-values with df = number of countries - 1
conservative_p <- function(m, term) {
  s <- summary(m)$coefficients
  t <- s[term, "t value"]
  c(estimate = s[term, "Estimate"], p_df24 = 2 * pt(-abs(t), df = 24))
}

#' H1 and H3 only (two-item belief models have no H2 row)
h13_row <- function(label, m1, m3, be) {
  a <- pick(m1, paste0("credential_w:", be))
  b <- pick(m3, paste0("credential_w:", be, ":stratification_c"))
  data.frame(check = label, n = nobs(m1), h1_est = a[1], h1_p = a[2], h2_est = NA, h2_p = NA,
             h3_est = b[1], h3_p = b[2], row.names = NULL)
}

#' ---- Build the datasets for the checks ----

d_hp <- d3 %>% filter(!country %in% c("Hungary", "Poland"))
d_ua <- d3 %>% filter(!country %in% c("USA", "Austria"))
d_ad <- recenter(d3 %>% filter(age >= 18))                      # adults only, centering redone
nrow(d3) - nrow(d_ad); n_distinct(d_ad$country)                 # respondents under 18 dropped; countries

#' ISSP weights (plain and capped at 5)
wts <- issp_raw %>%
  transmute(iso  = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))),
            id   = caseid, wave = year, weight = weight) %>%
  zap_labels()
d3w <- d3 %>% left_join(wts, by = c("iso", "id", "wave"), relationship = "one-to-one")
stopifnot(nrow(d3w) == nrow(d3), !anyNA(d3w$weight))
d3w$weight_cap <- pmin(d3w$weight, 5)
summary(d3w$weight); quantile(d3w$weight, c(.01, .5, .99, .999))

#' DEGREE = 5 credential (original coding)
stopifnot(!anyNA(d3$credential_degree5))
d5 <- d3 %>% group_by(country) %>% mutate(cred5_cmean = mean(credential_degree5)) %>% ungroup()
cm5 <- d5 %>% distinct(country, cred5_cmean)
d5 <- d5 %>% mutate(cred5_w = credential_degree5 - cred5_cmean,
                    cred5_b = cred5_cmean - mean(cm5$cred5_cmean))
map5 <- c(credential_w = "cred5_w", credential_b = "cred5_b")

#' Years of education (raw and cubed); `ey` was built in part 07
d_ey <- d3 %>% left_join(ey, by = c("iso", "id", "wave"), relationship = "one-to-one")
sum(is.na(d_ey$educyrs))                                        # respondents without years of education
d_ey <- recenter(d_ey %>% filter(!is.na(educyrs))) %>%
  mutate(yrs = educyrs / 10, yrs3 = (educyrs / 10)^3) %>%
  group_by(country) %>% mutate(yrs_cm = mean(yrs), yrs3_cm = mean(yrs3)) %>% ungroup()
cm_e <- d_ey %>% distinct(country, yrs_cm, yrs3_cm)
d_ey <- d_ey %>% mutate(ey_w  = yrs  - yrs_cm,  ey_b  = yrs_cm  - mean(cm_e$yrs_cm),
                        ey3_w = yrs3 - yrs3_cm, ey3_b = yrs3_cm - mean(cm_e$yrs3_cm))
nrow(d_ey); n_distinct(d_ey$country)
map_e  <- c(credential_w = "ey_w",  credential_b = "ey_b")
map_e3 <- c(credential_w = "ey3_w", credential_b = "ey3_b")

#' ISCO-88 skill-level credential (major groups 1-3 = 1, 4-9 = 0)
d_is <- d3 %>%
  mutate(cred_isco = case_when(floor(isco88 / 1000) %in% 1:3 ~ 1,
                               floor(isco88 / 1000) %in% 4:9 ~ 0)) %>%
  filter(!is.na(cred_isco))
nrow(d3) - nrow(d_is)                                           # respondents lost (should be 0)
mean(d_is$cred_isco); mean(d_is$credential); cor(d_is$cred_isco, d_is$credential)
d_is <- recenter(d_is) %>% group_by(country) %>% mutate(isco_cm = mean(cred_isco)) %>% ungroup()
cm_i <- d_is %>% distinct(country, isco_cm)
d_is <- d_is %>% mutate(isco_w = cred_isco - isco_cm, isco_b = isco_cm - mean(cm_i$isco_cm))
map_i <- c(credential_w = "isco_w", credential_b = "isco_b")

#' Two-item belief (hard work + ambition), 2009 only
amb <- issp_raw %>%
  transmute(iso = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))),
            id = caseid, wave = year, v4 = v4) %>%
  zap_labels()
d09 <- d3 %>% left_join(amb, by = c("iso", "id", "wave"), relationship = "one-to-one") %>%
  filter(wave == 2009, v4 > 0) %>%                              # valid ambition answers only
  mutate(ambition = 6 - v4, belief2 = (ind_belief + ambition) / 2)
nrow(d09); n_distinct(d09$country); cor(d09$ind_belief, d09$ambition)
d09 <- recenter(d09) %>%
  group_by(country) %>% mutate(b1_cm = mean(ind_belief), b2_cm = mean(belief2)) %>% ungroup()
cm_9 <- d09 %>% distinct(country, b1_cm, b2_cm)
d09 <- d09 %>% mutate(b1_w = ind_belief - b1_cm, b1_b = b1_cm - mean(cm_9$b1_cm),
                      b2_w = belief2 - b2_cm,    b2_b = b2_cm - mean(cm_9$b2_cm),
                      b1_wz = b1_w / sd(b1_w),   b2_wz = b2_w / sd(b2_w))     # per SD
map1 <- c(belief_w = "b1_wz", belief_b = "b1_b", "factor(wave) + " = "")      # one wave: no wave term
map2 <- c(belief_w = "b2_wz", belief_b = "b2_b", "factor(wave) + " = "")

#' ---- Run the checks and collect them in one table ----

#' each wave on its own (d3 has 2009 and 2019 only, so the wave term is dropped)
rob_wave <- bind_rows(lapply(sort(unique(d3$wave)), function(wv) {
  dw <- filter(d3, wave == wv)
  rob_row(paste("Wave", wv, "only"),
          update(m_h1_ctrl, . ~ . - factor(wave), data = dw),
          update(m_h2_ctrl, . ~ . - factor(wave), data = dw),
          update(m_h3_ctrl, . ~ . - factor(wave), data = dw))
}))

#' conservative p-values (df = 24), computed from the main models
cons <- rbind(H1 = conservative_p(m_h1_ctrl, "credential_w:belief_w"),
              H2 = conservative_p(m_h2_ctrl, "credential_w:stratification_c"),
              H3 = conservative_p(m_h3_ctrl, "credential_w:belief_w:stratification_c"))
rob_cons <- data.frame(check = "Conservative df (24)", n = nobs(m_h1_ctrl),
                       h1_est = cons["H1", "estimate"], h1_p = cons["H1", "p_df24"],
                       h2_est = cons["H2", "estimate"], h2_p = cons["H2", "p_df24"],
                       h3_est = cons["H3", "estimate"], h3_p = cons["H3", "p_df24"])

rob <- bind_rows(
  rob_data("Main models", d3),
  #' samples
  rob_data("Without Hungary and Poland", d_hp),
  rob_data("Without USA and Austria", d_ua),
  rob_wave,
  rob_data("Adults only (18+)", d_ad),
  #' controls and random part
  rob_row("Without income and ISEI",
          update(m_h1_ctrl, . ~ . - income - isei_w - isei_b),
          update(m_h2_ctrl, . ~ . - income - isei_w - isei_b),
          update(m_h3_ctrl, . ~ . - income - isei_w - isei_b)),
  rob_row("Country-wave random intercept",
          update(m_h1_ctrl, . ~ . + (1 | country:wave)),
          update(m_h2_ctrl, . ~ . + (1 | country:wave)),
          update(m_h3_ctrl, . ~ . + (1 | country:wave))),
  #' centering
  rob_swap("Country-wave centering", d3,
           c(credential_w = "credential_w_cw", belief_w = "belief_w_cw"),
           cr = "credential_w_cw", be = "belief_w_cw"),
  #' inference
  rob_cons,
  rob_row("Weighted",
          update(m_h1_ctrl, data = d3w, weights = weight),
          update(m_h2_ctrl, data = d3w, weights = weight),
          update(m_h3_ctrl, data = d3w, weights = weight)),
  rob_row("Weighted (capped at 5)",
          update(m_h1_ctrl, data = d3w, weights = weight_cap),
          update(m_h2_ctrl, data = d3w, weights = weight_cap),
          update(m_h3_ctrl, data = d3w, weights = weight_cap)),
  #' credential measures
  rob_swap("DEGREE = 5 credential", d5, map5, cr = "cred5_w"),
  rob_data("Main credential, years-of-education sample", d_ey),
  rob_swap("Years of education (per 10 years)", d_ey, map_e,  cr = "ey_w"),
  rob_swap("Years of education, cubed",        d_ey, map_e3, cr = "ey3_w"),
  rob_swap("ISCO-88 skill-level credential",   d_is, map_i,  cr = "isco_w"),
  #' belief measures (2009 only, belief per SD)
  h13_row("One-item belief, 2009 (per SD)",
          lmer(swap_vars(m_h1_ctrl, map1), data = d09),
          lmer(swap_vars(m_h3_ctrl, map1), data = d09), "b1_wz"),
  h13_row("Two-item belief, 2009 (per SD)",
          lmer(swap_vars(m_h1_ctrl, map2), data = d09),
          lmer(swap_vars(m_h3_ctrl, map2), data = d09), "b2_wz")
)

rob
write.csv(rob, "output/tables/robustness.csv", row.names = FALSE)

#' ---- Checks that do not go into the table ----

#' Formal test of wave differences (the three-way row is the 2019 minus 2009 change)
m_h1_wave <- update(m_h1_ctrl, . ~ . + credential_w:factor(wave) + belief_w:factor(wave) +
                      credential_w:belief_w:factor(wave))
m_h2_wave <- update(m_h2_ctrl, . ~ . + credential_w:factor(wave) + stratification_c:factor(wave) +
                      credential_w:stratification_c:factor(wave))
s1 <- summary(m_h1_wave)$coefficients
s2 <- summary(m_h2_wave)$coefficients
s1[grepl("credential_w:belief_w:factor\\(wave\\)", rownames(s1)), , drop = FALSE]
s2[grepl("credential_w:stratification_c:factor\\(wave\\)", rownames(s2)), , drop = FALSE]

wave_test <- rbind(
  H1 = s1[grepl("credential_w:belief_w:factor\\(wave\\)", rownames(s1)), ],
  H2 = s2[grepl("credential_w:stratification_c:factor\\(wave\\)", rownames(s2)), ])
wave_test
write.csv(wave_test, "output/tables/wave_difference_test.csv")

#' Marginal effects at the 25th and 75th percentile country (needs cm_strat, tab_level, b_lim from part 14)
lo25 <- quantile(cm_strat$stratification_c, 0.25)
hi75 <- quantile(cm_strat$stratification_c, 0.75)

e2_q <- emtrends(m_h2_ctrl, ~ stratification_c, var = "credential_w",
                 at = list(stratification_c = c(lo25, hi75)))
h2_q <- confint(e2_q, level = tab_level)
h2_q
contrast(e2_q, "revpairwise")                # high minus low

e3_q <- emtrends(m_h3_ctrl, ~ belief_w * stratification_c, var = "credential_w",
                 at = list(belief_w = as.numeric(b_lim), stratification_c = c(lo25, hi75)))
h3_q <- confint(e3_q, level = tab_level)
h3_q
contrast(e3_q, interaction = "revpairwise")  # difference in the belief gap, high minus low stratification

write.csv(as.data.frame(h2_q), "output/tables/h2_marginal_effects_q25_75.csv", row.names = FALSE)
write.csv(as.data.frame(h3_q), "output/tables/h3_marginal_effects_q25_75.csv", row.names = FALSE)

#' Diagnostics
isSingular(m_h1_ctrl); isSingular(m_h2_ctrl); isSingular(m_h3_ctrl)

cl <- d3 %>% distinct(country, stratification_c, gini_c, gdp_c, tertiary_c)   # one row per country
round(cor(cl[, -1]), 2)
vif_manual <- sapply(names(cl)[-1], function(v) {
  r2 <- summary(lm(reformulate(setdiff(names(cl)[-1], v), response = v), data = cl))$r.squared
  1 / (1 - r2)
})
round(vif_manual, 2)

png("output/figures/diagnostics_residuals.png", width = 1800, height = 900, res = 200)
par(mfrow = c(1, 2))
plot(fitted(m_h3_ctrl), resid(m_h3_ctrl), pch = ".", xlab = "Fitted", ylab = "Residual")
abline(h = 0, col = "red")
qqnorm(resid(m_h3_ctrl), pch = "."); qqline(resid(m_h3_ctrl), col = "red")
dev.off()

png("output/figures/diagnostics_ranef.png", width = 1800, height = 900, res = 200)
re <- ranef(m_h3_ctrl)$country
par(mfrow = c(1, 3))
for (v in names(re)[1:3]) { qqnorm(re[[v]], main = v); qqline(re[[v]], col = "red") }
dev.off()

write.csv(round(cor(cl[, -1]), 2), "output/tables/country_level_correlations.csv")
write.csv(data.frame(variable = names(vif_manual), vif = round(vif_manual, 2)),
          "output/tables/country_level_vif.csv", row.names = FALSE)
write.csv(data.frame(model = c("H1", "H2", "H3"),
                     singular = c(isSingular(m_h1_ctrl), isSingular(m_h2_ctrl), isSingular(m_h3_ctrl))),
          "output/tables/singular_fit_check.csv", row.names = FALSE)

