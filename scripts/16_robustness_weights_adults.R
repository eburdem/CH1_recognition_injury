#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 16_robustness_weights_adults
#' ----------------------------

library(dplyr)
library(haven)
library(lme4)
library(lmerTest)
library(readr)

d3       <- readRDS("data/processed/d3_models.rds")
issp_raw <- readRDS("data/interim/issp_raw.rds")

get_term <- function(m, term) {
  summary(m)$coefficients[term, c("Estimate", "Std. Error", "Pr(>|t|)")]
}

#' Attach the ISSP weight (same as script 07) 
w <- issp_raw %>%
  transmute(iso  = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))),
            id   = caseid,
            wave = year,
            weight = weight) %>%
  zap_labels()

n_before <- nrow(d3)
d3 <- d3 %>% left_join(w, by = c("iso", "id", "wave"), relationship = "one-to-one")
stopifnot(nrow(d3) == n_before)
stopifnot(!anyNA(d3$weight))

summary(d3$weight)                      
quantile(d3$weight, c(.01, .5, .99, .999))
d3 %>% group_by(country) %>% summarise(mean_w = mean(weight), max_w = max(weight)) %>%
  arrange(desc(max_w)) %>% print(n = 5)

#' Weighted models 

m_h1_d <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d3, weights = weight)

d3$weight_cap <- pmin(d3$weight, 5)     # cap at 5 (affects about 0.1% of respondents)

m_h1_d_cap <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                     factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                     (1 + credential_w | country), data = d3, weights = weight_cap)
get_term(m_h1_d_cap, "credential_w:belief_w")

m_h2_d <- lmer(sss ~ credential_w * stratification_c + credential_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d3, weights = weight)

m_h3_d <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d3, weights = weight)

#' Adults only (age 18+), centering redone on the adult sample 
d_adult <- d3 %>%
  filter(age >= 18) %>%
  group_by(country) %>%
  mutate(cred_cmean = mean(credential),
         bel_cmean  = mean(ind_belief)) %>%
  ungroup()

cm_adult <- d_adult %>% distinct(country, cred_cmean, bel_cmean)

d_adult <- d_adult %>%
  mutate(credential_w = credential - cred_cmean,
         belief_w     = ind_belief - bel_cmean,
         credential_b = cred_cmean - mean(cm_adult$cred_cmean),
         belief_b     = bel_cmean  - mean(cm_adult$bel_cmean))

nrow(d3) - nrow(d_adult)                # respondents aged 15-17 dropped
n_distinct(d_adult$country)             # should still be 25

m_h1_e <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d_adult)

m_h2_e <- lmer(sss ~ credential_w * stratification_c + credential_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d_adult)

m_h3_e <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
                 factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d_adult)

#' Comparison table
robust2 <- rbind(
  data.frame(model = "D weighted", hyp = "H1", t(get_term(m_h1_d, "credential_w:belief_w"))),
  data.frame(model = "D weighted", hyp = "H2", t(get_term(m_h2_d, "credential_w:stratification_c"))),
  data.frame(model = "D weighted", hyp = "H3", t(get_term(m_h3_d, "credential_w:belief_w:stratification_c"))),
  data.frame(model = "E adults only", hyp = "H1", t(get_term(m_h1_e, "credential_w:belief_w"))),
  data.frame(model = "E adults only", hyp = "H2", t(get_term(m_h2_e, "credential_w:stratification_c"))),
  data.frame(model = "E adults only", hyp = "H3", t(get_term(m_h3_e, "credential_w:belief_w:stratification_c")))
)
names(robust2) <- c("model", "hyp", "estimate", "se", "p")
robust2

robust2 <- rbind(robust2,
                 data.frame(model = "D weighted (capped at 5)", hyp = "H1", t(get_term(m_h1_d_cap, "credential_w:belief_w")),
                            row.names = NULL) %>% setNames(names(robust2)))
write_csv(robust2, "output/tables/robustness2_interactions.csv")




