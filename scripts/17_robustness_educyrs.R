#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 17_robustness_educyrs
#' ---------------------

library(dplyr)
library(lme4)
library(lmerTest)
library(readr)

d3 <- readRDS("data/processed/d3_models.rds")
ey <- readRDS("data/interim/issp_educyrs.rds") %>%
  select(iso, id, wave, educyrs)           # educyrs already cleaned (0-30, else NA) in script 07

get_term <- function(m, term) {
  summary(m)$coefficients[term, c("Estimate", "Std. Error", "Pr(>|t|)")]
}

#' Attach EDUCYRS and build a common sample 
n_before <- nrow(d3)
d3 <- d3 %>% left_join(ey, by = c("iso", "id", "wave"), relationship = "one-to-one")
stopifnot(nrow(d3) == n_before)

sum(is.na(d3$educyrs))                      # respondents without EDUCYRS
d_ey <- d3 %>% filter(!is.na(educyrs))      # common sample for all models below
nrow(d_ey); n_distinct(d_ey$country)       

#' Build the variables (years in decades; cube) 
d_ey <- d_ey %>%
  mutate(ey    = educyrs / 10,
         ey3   = (educyrs / 10)^3) %>%
  group_by(country) %>%
  mutate(ey_cmean  = mean(ey),
         ey3_cmean = mean(ey3),
         cred_cmean2 = mean(credential),     
         bel_cmean2  = mean(ind_belief)) %>%
  ungroup()

cm <- d_ey %>% distinct(country, ey_cmean, ey3_cmean, cred_cmean2, bel_cmean2)

d_ey <- d_ey %>%
  mutate(ey_w   = ey  - ey_cmean,
         ey_b   = ey_cmean  - mean(cm$ey_cmean),
         ey3_w  = ey3 - ey3_cmean,
         ey3_b  = ey3_cmean - mean(cm$ey3_cmean),
         credential_w = credential - cred_cmean2,     # main credential, recentered on this sample
         credential_b = cred_cmean2 - mean(cm$cred_cmean2),
         belief_w     = ind_belief - bel_cmean2,
         belief_b     = bel_cmean2 - mean(cm$bel_cmean2))

stopifnot(max(abs(tapply(d_ey$ey_w,  d_ey$country, mean))) < 1e-10)
stopifnot(max(abs(tapply(d_ey$ey3_w, d_ey$country, mean))) < 1e-10)

cor(d_ey[c("credential_w", "ey_w", "ey3_w")])    # how close are the three measures?

#' Main credential on the common sample (reference) 
m_h1_0 <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d_ey)
m_h2_0 <- lmer(sss ~ credential_w * stratification_c + credential_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d_ey)
m_h3_0 <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d_ey)

#' Raw years of education (per 10 years) 
m_h1_f <- lmer(sss ~ ey_w * belief_w + ey_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + ey_w | country), data = d_ey)
m_h2_f <- lmer(sss ~ ey_w * stratification_c + ey_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + ey_w | country), data = d_ey)
m_h3_f <- lmer(sss ~ ey_w * belief_w * stratification_c + ey_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + ey_w | country), data = d_ey)

#' Cubed years of education ((years/10)^3) 
m_h1_g <- lmer(sss ~ ey3_w * belief_w + ey3_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + ey3_w | country), data = d_ey)
m_h2_g <- lmer(sss ~ ey3_w * stratification_c + ey3_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + ey3_w | country), data = d_ey)
m_h3_g <- lmer(sss ~ ey3_w * belief_w * stratification_c + ey3_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + ey3_w | country), data = d_ey)

#' Comparison table
robust3 <- rbind(
  data.frame(model = "Main, common sample", hyp = "H1", t(get_term(m_h1_0, "credential_w:belief_w"))),
  data.frame(model = "Main, common sample", hyp = "H2", t(get_term(m_h2_0, "credential_w:stratification_c"))),
  data.frame(model = "Main, common sample", hyp = "H3", t(get_term(m_h3_0, "credential_w:belief_w:stratification_c"))),
  data.frame(model = "F raw years", hyp = "H1", t(get_term(m_h1_f, "ey_w:belief_w"))),
  data.frame(model = "F raw years", hyp = "H2", t(get_term(m_h2_f, "ey_w:stratification_c"))),
  data.frame(model = "F raw years", hyp = "H3", t(get_term(m_h3_f, "ey_w:belief_w:stratification_c"))),
  data.frame(model = "G cubed years", hyp = "H1", t(get_term(m_h1_g, "ey3_w:belief_w"))),
  data.frame(model = "G cubed years", hyp = "H2", t(get_term(m_h2_g, "ey3_w:stratification_c"))),
  data.frame(model = "G cubed years", hyp = "H3", t(get_term(m_h3_g, "ey3_w:belief_w:stratification_c")))
)
names(robust3) <- c("model", "hyp", "estimate", "se", "p")
robust3

write_csv(robust3, "output/tables/robustness3_educyrs.csv")














