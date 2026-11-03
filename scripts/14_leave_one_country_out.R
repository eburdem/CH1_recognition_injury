#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 14_leave_one_country_out
#' ------------------------

library(dplyr)
library(lme4)
library(lmerTest)

d3   <- readRDS("data/processed/d3_models.rds")
mods <- readRDS("data/processed/models_h123.rds")
m_h1 <- mods$m_h1; m_h2 <- mods$m_h2; m_h3 <- mods$m_h3

countries <- sort(unique(as.character(d3$country)))

#' H1: credential_w:belief_w
res_h1 <- list()
for (cn in countries) {
  fit <- update(m_h1, data = d3 %>% filter(country != cn))
  s <- summary(fit)$coefficients
  res_h1[[cn]] <- data.frame(dropped = cn,
                             estimate = s["credential_w:belief_w", "Estimate"],
                             p = s["credential_w:belief_w", "Pr(>|t|)"])
  print(cn)
}
loo_h1 <- bind_rows(res_h1)
saveRDS(loo_h1, "data/processed/loo_h1.rds")

#' H2: credential_w:stratification_c
res_h2 <- list()
for (cn in countries) {
  fit <- update(m_h2, data = d3 %>% filter(country != cn))
  s <- summary(fit)$coefficients
  res_h2[[cn]] <- data.frame(dropped = cn,
                             estimate = s["credential_w:stratification_c", "Estimate"],
                             p = s["credential_w:stratification_c", "Pr(>|t|)"])
  print(cn)
}
loo_h2 <- bind_rows(res_h2)
saveRDS(loo_h2, "data/processed/loo_h2.rds")

#' Results
loo_h1 %>% arrange(desc(p))
loo_h2 %>% arrange(estimate)

range(loo_h1$estimate); sum(loo_h1$p < .05)
range(loo_h2$estimate); sum(loo_h2$p < .05)

#' H3: credential_w:belief_w:stratification_c
res_h3 <- list()
for (cn in countries) {
  fit <- update(m_h3, data = d3 %>% filter(country != cn))
  s <- summary(fit)$coefficients
  res_h3[[cn]] <- data.frame(dropped = cn,
                             estimate = s["credential_w:belief_w:stratification_c", "Estimate"],
                             p = s["credential_w:belief_w:stratification_c", "Pr(>|t|)"])
  print(cn)
}
loo_h3 <- bind_rows(res_h3)
saveRDS(loo_h3, "data/processed/loo_h3.rds")
