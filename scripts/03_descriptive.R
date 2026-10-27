#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 03_descriptive
#' --------------

library(dplyr)
library(tidyr)
library(ggplot2)
merged        <- readRDS("data/interim/issp_merged.rds")
issp_raw      <- readRDS("data/interim/issp_raw.rds")
country_level <- readRDS("data/interim/country_level_vars.rds")

d0 <- merged;                                        nrow(d0)
d1 <- d0 %>% filter(wave %in% c(2009, 2019));        nrow(d1)
d2 <- d1 %>% filter(!is.na(stratification));         nrow(d2); n_distinct(d2$country)  
d3 <- d2 %>% drop_na(sss, credential, ind_belief, income, isei, age, gender,
                     gini, gdp_pc, tertiary);        nrow(d3); n_distinct(d3$country) 

setdiff(unique(d1$country), unique(d2$country))
setdiff(unique(d2$country), unique(d3$country))

#' Attrition table
attrition <- data.frame(
  step = c("All rows",
           "Waves 2009 + 2019",
           "Has stratification value",
           "Complete on model variables"),
  n = c(nrow(d0), nrow(d1), nrow(d2), nrow(d3)),
  countries = c(n_distinct(d0$country), n_distinct(d1$country),
                n_distinct(d2$country), n_distinct(d3$country))
)
attrition$lost <- c(NA, -diff(attrition$n))   
attrition

write_csv(attrition, "output/attrition.csv")
saveRDS(d3, "data/processed/d3.rds")


colMeans(is.na(d2)) * 100  

vars <- c("sss", "credential", "ind_belief", "income", "isei", "age", "gender",
          "gini", "gdp_pc", "tertiary")
for (v in vars) {
  d_tmp <- d2[!is.na(d2[[v]]), ]
  cat(v, "alone loses", nrow(d2) - nrow(d_tmp), "\n")
}

d2 %>%
  group_by(country) %>%
  summarise(pct_income = mean(is.na(income)) * 100,
            pct_isei   = mean(is.na(isei)) * 100,
            pct_tert   = mean(is.na(tertiary)) * 100) %>%
  print(n = 30)

d2 %>%
  group_by(wave) %>%
  summarise(pct_income = mean(is.na(income)) * 100,
            pct_isei   = mean(is.na(isei)) * 100)

d2 %>% count(country, name = "n_d2") %>%
  left_join(d3 %>% count(country, name = "n_d3"), by = "country") %>%
  mutate(n_d3 = coalesce(n_d3, 0L),
         pct_kept = round(100 * n_d3 / n_d2, 1)) %>%
  arrange(pct_kept) %>%
  print(n = 30)


tmp <- data.frame(country = merged$country, wave = merged$wave,
                  income_raw = as.numeric(issp_raw$income))
table(tmp$country[tmp$country %in% c("Spain", "Portugal", "Slovenia", "Czech Republic") &
                    tmp$wave %in% c(2009, 2019)],
      tmp$income_raw[tmp$country %in% c("Spain", "Portugal", "Slovenia", "Czech Republic") &
                       tmp$wave %in% c(2009, 2019)])

d_notert <- d2 %>% drop_na(sss, credential, ind_belief, income, isei, age, gender, gini, gdp_pc)
nrow(d_notert); n_distinct(d_notert$country)

retention <- d2 %>% count(country, name = "n_d2") %>%
  left_join(d3 %>% count(country, name = "n_d3"), by = "country") %>%
  mutate(n_d3 = coalesce(n_d3, 0L), pct_kept = round(100 * n_d3 / n_d2, 1))
write_csv(retention, "output/country_retention.csv")

summary(d3$sss);  sd(d3$sss)
summary(d3$ind_belief);  sd(d3$ind_belief)
summary(d3$age);  sd(d3$age)
summary(d3$isei); sd(d3$isei)

table(d3$credential);  prop.table(table(d3$credential))
table(d3$income);      prop.table(table(d3$income))
table(d3$gender);      prop.table(table(d3$gender))
table(d3$wave);        prop.table(table(d3$wave))

compare <- function(d) c(n = nrow(d), sss = mean(d$sss, na.rm = TRUE),
                         tertiary = mean(d$credential, na.rm = TRUE),
                         age = mean(d$age, na.rm = TRUE))
rbind(d1 = compare(d1), d2 = compare(d2), d3 = compare(d3))

write_csv(data.frame(var = c("sss","ind_belief","age","isei"),
                     mean = c(mean(d3$sss), mean(d3$ind_belief), mean(d3$age), mean(d3$isei)),
                     sd   = c(sd(d3$sss), sd(d3$ind_belief), sd(d3$age), sd(d3$isei))),
          "output/table1_continuous.csv")

d1 %>% filter(is.na(stratification)) %>% summarise(mean_sss = mean(sss, na.rm = TRUE))



p_sss <- ggplot(d3, aes(x = sss)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Subjective social status (1 = bottom, 10 = top)", y = "Respondents")
ggsave("output/figures/hist_sss.png", p_sss, width = 6, height = 4, dpi = 300)

p_ind_belief <- ggplot(d3, aes(x = factor(ind_belief))) + 
  geom_bar() +
  labs(x = "Belief: hard work is important to get ahead (1–5)", y = "Respondents")
ggsave("output/figures/hist_ind_belief.png", p_ind_belief, width = 6, height = 4, dpi = 300)

p_income <- ggplot(d3, aes(x = income)) + geom_bar() +
  labs(x = "Household income tercile", y = "Respondents")
ggsave("output/figures/hist_income.png", p_income, width = 6, height = 4, dpi = 300)

p_ISEI <- ggplot(d3, aes(x = isei)) + geom_histogram(binwidth = 5) +
  labs(x = "Occupational status (ISEI)", y = "Respondents")
ggsave("output/figures/hist_isei.png", p_ISEI, width = 6, height = 4, dpi = 300)





