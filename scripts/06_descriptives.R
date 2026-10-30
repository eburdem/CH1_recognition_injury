#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 06_descriptives
#' ----------------

library(dplyr)
library(readr)
library(ggplot2)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
d3 <- readRDS("data/processed/d3.rds")

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

#' country-level variables by wave (stratification is constant; the others vary)
d3 %>% distinct(country, wave, stratification, gini, gdp_pc, tertiary) %>%
  arrange(country, wave) %>% print(n = 50)

d3 %>% group_by(country) %>%
  summarise(pct_tert = 100 * mean(credential), pct_alt = 100 * mean(credential_alt)) %>%
  arrange(desc(pct_alt - pct_tert)) %>% print(n = 30)


