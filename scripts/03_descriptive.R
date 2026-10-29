#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 03_descriptive
#' --------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
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


#' Selection across steps
compare <- function(d) c(n = nrow(d), 
                         sss = mean(d$sss, na.rm = TRUE),
                         share_tertiary = mean(d$credential, na.rm = TRUE),
                         age = mean(d$age, na.rm = TRUE))
rbind(d1 = compare(d1), d2 = compare(d2), d3 = compare(d3))

d1 %>% filter(is.na(stratification)) %>% summarise(mean_sss = mean(sss, na.rm = TRUE))

#' Descriptives
 
#' Table 1: continuous variables
cont <- c("sss", "ind_belief", "age", "isei")
table1 <- data.frame(var  = cont,
                     mean = sapply(d3[cont], mean),
                     sd   = sapply(d3[cont], sd),
                     min  = sapply(d3[cont], min),
                     max  = sapply(d3[cont], max),
                     row.names = NULL)
table1
write_csv(table1, "output/tables/table1_continuous.csv")

#' Table 1: categorical variables (% of respondents)

for (v in c("credential", "income", "gender", "wave")) {
  cat("\n", v, "\n")
  print(round(100 * prop.table(table(d3[[v]])), 1))
}

#' Plots
p_sss <- ggplot(d3, aes(x = sss)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Subjective social status (1 = bottom, 10 = top)", y = "Respondents")
p_sss
ggsave("output/figures/hist_sss.png",   p_sss,    width = 6, height = 4, dpi = 300)

p_ind_belief <- ggplot(d3, aes(x = factor(ind_belief))) +
  geom_bar() +
  labs(x = "Belief: hard work is important to get ahead (1-5)", y = "Respondents")
p_ind_belief
ggsave("output/figures/bar_belief.png", p_ind_belief, width = 6, height = 4, dpi = 300)

p_income <- ggplot(d3, aes(x = income)) +
  geom_bar() +
  scale_x_discrete(labels = c("Low", "Middle", "High")) +
  labs(x = "Household income tercile", y = "Respondents")
p_income
ggsave("output/figures/bar_income.png", p_income, width = 6, height = 4, dpi = 300)

p_isei <- ggplot(d3, aes(x = isei)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Occupational status (ISEI)", y = "Respondents")
p_isei
ggsave("output/figures/hist_isei.png", p_isei, width = 6, height = 4, dpi = 300)

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

# country-level variables by wave (stratification is constant; the others vary)
d3 %>% distinct(country, wave, stratification, gini, gdp_pc, tertiary) %>%
  arrange(country, wave) %>% print(n = 50)


d2 %>% drop_na(all_of(c(setdiff(model_vars, "credential"), "credential_alt"))) %>% nrow()

d3 %>% group_by(country) %>%
  summarise(pct_tert = 100 * mean(credential), pct_alt = 100 * mean(credential_alt)) %>%
  arrange(desc(pct_alt - pct_tert)) %>% print(n = 30)


