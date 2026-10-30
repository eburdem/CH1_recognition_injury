#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 05_sample_analysis
#' ------------------

library(dplyr)
library(tidyr)
library(readr)

dir.create("data/processed", showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)


merged <- readRDS("data/interim/issp_analysis.rds")

#' Sample: filters
model_vars <- c("sss", "credential", "ind_belief", "income", "isei",
                "age", "gender", "gini", "gdp_pc", "tertiary")

d1 <- merged %>% filter(wave %in% c(2009, 2019));        nrow(d1)
d2 <- d1 %>% filter(!is.na(stratification));         nrow(d2); n_distinct(d2$country)  
d3 <- d2 %>% drop_na(all_of(model_vars)) 

#' Attrition table
attrition <- data.frame(
  step = c("All rows",
           "Waves 2009 + 2019",
           "Has stratification value",
           "Complete on model variables"),
  n = c(nrow(merged), nrow(d1), nrow(d2), nrow(d3)),
  countries = c(n_distinct(merged$country), n_distinct(d1$country),
                n_distinct(d2$country), n_distinct(d3$country))
)
attrition$lost <- c(NA, -diff(attrition$n))   
attrition

setdiff(unique(d1$country), unique(d2$country))  
setdiff(unique(d2$country), unique(d3$country)) 

write_csv(attrition, "output/tables/attrition.csv")
saveRDS(d3, "data/processed/d3.rds")

#' Missingness
colMeans(is.na(d2[model_vars])) * 100  
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
print(retention, n=30)

write_csv(retention, "output/tables/country_retention.csv")

#' Sensitivity sample without tertiary
d_notert <- d2 %>% drop_na(all_of(setdiff(model_vars, "tertiary")))
nrow(d_notert); n_distinct(d_notert$country)
saveRDS(d_notert, "data/processed/d_notert.rds")

#' Sensitivity sample with credential_alt instead of credential
d2 %>% drop_na(all_of(c(setdiff(model_vars, "credential"), "credential_alt"))) %>% nrow()

#' Selection across steps
compare <- function(d) c(n = nrow(d), 
                         sss = mean(d$sss, na.rm = TRUE),
                         share_tertiary = mean(d$credential, na.rm = TRUE),
                         age = mean(d$age, na.rm = TRUE))
rbind(d1 = compare(d1), d2 = compare(d2), d3 = compare(d3))

d1 %>% filter(is.na(stratification)) %>% summarise(mean_sss = mean(sss, na.rm = TRUE))


