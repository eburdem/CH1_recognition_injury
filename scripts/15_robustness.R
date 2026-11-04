#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 15_robustness
#' -------------

library(dplyr)
library(lme4)
library(lmerTest)
library(readr)

d3   <- readRDS("data/processed/d3_models.rds")
mods <- readRDS("data/processed/models_h123.rds")
m_h1 <- mods$m_h1
m_h2 <- mods$m_h2
m_h3 <- mods$m_h3

#' estimate, SE and p of one coefficient
get_term <- function(m, term) {
  summary(m)$coefficients[term, c("Estimate", "Std. Error", "Pr(>|t|)")]
}

#' Country-wave random intercept added 
m_h1_a <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country) + (1 | country:wave), data = d3)

m_h2_a <- lmer(sss ~ credential_w * stratification_c + credential_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country) + (1 | country:wave), data = d3)

m_h3_a <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country) + (1 | country:wave), data = d3)

#' Within country-wave centering

m_h1_b <- lmer(sss ~ credential_w_cw * belief_w_cw + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w_cw | country), data = d3)

m_h2_b <- lmer(sss ~ credential_w_cw * stratification_c + credential_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w_cw | country), data = d3)

m_h3_b <- lmer(sss ~ credential_w_cw * belief_w_cw * stratification_c + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w_cw | country), data = d3)


#' Original DEGREE = 5 credential instead of the harmonized one

stopifnot(!anyNA(d3$credential_degree5))

d3 <- d3 %>%
  group_by(country) %>%
  mutate(cred5_cmean = mean(credential_degree5)) %>%
  ungroup() %>%
  mutate(cred5_w = credential_degree5 - cred5_cmean,
         cred5_b = cred5_cmean - mean(unique(cred5_cmean)))

m_h1_c <- lmer(sss ~ cred5_w * belief_w + cred5_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + cred5_w | country), data = d3)

m_h2_c <- lmer(sss ~ cred5_w * stratification_c + cred5_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + cred5_w | country), data = d3)

m_h3_c <- lmer(sss ~ cred5_w * belief_w * stratification_c + cred5_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + cred5_w | country), data = d3)


#' Comparison table

robust <- rbind(
  data.frame(model = "Main", hyp = "H1", t(get_term(m_h1, "credential_w:belief_w"))),
  data.frame(model = "Main", hyp = "H2", t(get_term(m_h2, "credential_w:stratification_c"))),
  data.frame(model = "Main", hyp = "H3", t(get_term(m_h3, "credential_w:belief_w:stratification_c"))),
  data.frame(model = "A country-wave intercept", hyp = "H1", t(get_term(m_h1_a, "credential_w:belief_w"))),
  data.frame(model = "A country-wave intercept", hyp = "H2", t(get_term(m_h2_a, "credential_w:stratification_c"))),
  data.frame(model = "A country-wave intercept", hyp = "H3", t(get_term(m_h3_a, "credential_w:belief_w:stratification_c"))),
  data.frame(model = "B within country-wave", hyp = "H1", t(get_term(m_h1_b, "credential_w_cw:belief_w_cw"))),
  data.frame(model = "B within country-wave", hyp = "H2", t(get_term(m_h2_b, "credential_w_cw:stratification_c"))),
  data.frame(model = "B within country-wave", hyp = "H3", t(get_term(m_h3_b, "credential_w_cw:belief_w_cw:stratification_c"))),
  data.frame(model = "C DEGREE = 5",  hyp = "H1", t(get_term(m_h1_c, "cred5_w:belief_w"))),
  data.frame(model = "C DEGREE = 5",  hyp = "H2", t(get_term(m_h2_c, "cred5_w:stratification_c"))),
  data.frame(model = "C DEGREE = 5",  hyp = "H3", t(get_term(m_h3_c, "cred5_w:belief_w:stratification_c")))
)
names(robust) <- c("model", "hyp", "estimate", "se", "p")
robust

write_csv(robust, "output/tables/robustness_interactions.csv")









