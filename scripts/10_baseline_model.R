#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 10_baseline_model
#' -----------------

library(dplyr)
library(lme4)
library(lmerTest)

d3 <- readRDS("data/processed/d3_centered.rds")

#' ISEI within and between country (same logic as credential)

d3 <- d3 %>%
  group_by(country) %>%
  mutate(isei_cmean = mean(isei)) %>%
  ungroup() %>%
  mutate(isei_w = isei - isei_cmean,
         isei_b = isei_cmean - mean(unique(isei_cmean)))

#' Model1: credential only (within and between), wave as fixed effect

m1 <- lmer (sss ~ credential_w + credential_b + factor(wave) +
              (1 + credential_w | country), data = d3) 
summary(m1)

m2 <- lmer (sss ~ credential_w + credential_b + factor(wave) +
              income + isei_w +isei_b + age + age_sq+ gender +
              (1 + credential_w | country), data = d3) 
summary(m2)