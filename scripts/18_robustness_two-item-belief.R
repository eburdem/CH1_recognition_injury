#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY 
#' ------------------------------
#' 18_robustness_two-item-belief
#' -----------------------------

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

#' Attach the ambition item (v4) 
amb <- issp_raw %>%
  transmute(iso  = sub("^[0-9]+\\. ([A-Z]{2})-.*", "\\1", as.character(as_factor(country))),
            id   = caseid,
            wave = year,
            v4   = v4) %>%
  zap_labels()

n_before <- nrow(d3)
d3 <- d3 %>% left_join(amb, by = c("iso", "id", "wave"), relationship = "one-to-one")
stopifnot(nrow(d3) == n_before)

#' 2009 only; ambition recoded like hard work (5 = strongest endorsement) 
d09 <- d3 %>%
  filter(wave == 2009, v4 > 0) %>%    # valid answers only (drops -9, -8, -3, -1)
  mutate(ambition = 6 - v4,
         belief2  = (ind_belief + ambition) / 2)   # two-item scale, 1 to 5

nrow(d09); n_distinct(d09$country)   # sample size and countries
cor(d09$ind_belief, d09$ambition)    # correlation between the two items
sd(d09$ind_belief); sd(d09$belief2)


#' Within/between split on the 2009 sample (credential and both belief measures)
d09 <- d09 %>%
  group_by(country) %>%
  mutate(cred_cm = mean(credential),
         b1_cm   = mean(ind_belief),
         b2_cm   = mean(belief2)) %>%
  ungroup()

cm09 <- d09 %>% distinct(country, cred_cm, b1_cm, b2_cm)

d09 <- d09 %>%
  mutate(credential_w = credential - cred_cm,
         credential_b = cred_cm - mean(cm09$cred_cm),
         b1_w = ind_belief - b1_cm,
         b1_b = b1_cm - mean(cm09$b1_cm),
         b2_w = belief2 - b2_cm,
         b2_b = b2_cm - mean(cm09$b2_cm),
         b1_wz = b1_w / sd(b1_w),    # per SD, so the two measures are comparable
         b2_wz = b2_w / sd(b2_w))

stopifnot(max(abs(tapply(d09$b2_w, d09$country, mean))) < 1e-10)

#' Reference: one-item belief on the same 2009 respondents 
m_h1_1 <- lmer(sss ~ credential_w * b1_wz + credential_b + b1_b +
                 income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d09)

m_h3_1 <- lmer(sss ~ credential_w * b1_wz * stratification_c + credential_b + b1_b +
                 income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d09)

#' two-item belief (hard work + ambition) 
m_h1_2 <- lmer(sss ~ credential_w * b2_wz + credential_b + b2_b +
                 income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d09)

m_h3_2 <- lmer(sss ~ credential_w * b2_wz * stratification_c + credential_b + b2_b +
                 income + isei_w + isei_b + age10 + age10_sq + gender +
                 (1 + credential_w | country), data = d09)

#' Comparison table (belief per SD)
robust4 <- rbind(
  data.frame(model = "One item, 2009 only", hyp = "H1", t(get_term(m_h1_1, "credential_w:b1_wz"))),
  data.frame(model = "One item, 2009 only", hyp = "H3", t(get_term(m_h3_1, "credential_w:b1_wz:stratification_c"))),
  data.frame(model = "Two items, 2009 only", hyp = "H1", t(get_term(m_h1_2, "credential_w:b2_wz"))),
  data.frame(model = "Two items, 2009 only", hyp = "H3", t(get_term(m_h3_2, "credential_w:b2_wz:stratification_c")))
)
names(robust4) <- c("model", "hyp", "estimate", "se", "p")
robust4

write_csv(robust4, "output/tables/robustness4_two_item_belief.csv")









