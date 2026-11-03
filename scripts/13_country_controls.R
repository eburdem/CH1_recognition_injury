#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 13_country_controls
#' -------------------

library(dplyr)
library(lme4)
library(lmerTest)

d3 <- readRDS("data/processed/d3_models.rds")

#' Country means, then centered on the mean of the 25 country means
d3 <- d3 %>%
  group_by(country) %>%
  mutate(gini_cm = mean(gini, na.rm = TRUE),
         gdp_cm  = mean(log(gdp_pc), na.rm = TRUE),
         tert_cm = mean(tertiary, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gini_c     = gini_cm - mean(unique(gini_cm)),
         gdp_c      = gdp_cm  - mean(unique(gdp_cm)),
         tertiary_c = tert_cm - mean(unique(tert_cm)))

stopifnot(!anyNA(d3$gini_c), !anyNA(d3$gdp_c), !anyNA(d3$tertiary_c))
stopifnot(n_distinct(d3$tert_cm) == n_distinct(d3$country))

d3 %>% distinct(country, credential_b, tertiary_c) %>%
  summarise(sd_cred_b = sd(credential_b), r = cor(credential_b, tertiary_c))

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

summary(m_h1_ctrl)$coefficients["credential_w:belief_w", ]
summary(m_h2_ctrl)$coefficients["credential_w:stratification_c", ]
summary(m_h3_ctrl)$coefficients["credential_w:belief_w:stratification_c", ]
