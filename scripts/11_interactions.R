#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 11_interactions
#' -----------------

library(dplyr)
library(lme4)
library(lmerTest)

d3 <- readRDS("data/processed/d3_centered.rds")

d3 <- d3 %>%
  group_by(country) %>%
  mutate(isei_cmean = mean(isei)) %>%
  ungroup() %>%
  mutate(isei_w = isei - isei_cmean,
         isei_b = isei_cmean - mean(unique(isei_cmean)),
         age10 = (age -50) /10,
         age10_sq = age10^2)

#' H1: credential x individual belief

m_h1 <- lmer(sss~ credential_w * belief_w + credential_b + belief_b +
               factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
               (1 + credential_w | country), data = d3)

summary(m_h1)          

#' H2: credential x stratification

m_h2 <- lmer(sss~ credential_w * stratification_c + credential_b + 
               factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
               (1 + credential_w | country), data = d3)

summary(m_h2)          

#' H3: threeway interaction

m_h3 <- lmer(sss~ credential_w * belief_w* stratification_c + credential_b + belief_b +
               factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
               (1 + credential_w | country), data = d3)

summary(m_h3)    

saveRDS(d3, "data/processed/d3_models.rds")
saveRDS(list(m_h1 = m_h1, m_h2 = m_h2, m_h3 = m_h3), "data/processed/models_h123.rds")
