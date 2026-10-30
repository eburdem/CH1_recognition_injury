#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 08_centering
#' ------------

library(dplyr)

d3 <- readRDS("data/processed/d3.rds")

#' Version A: within-country centering

d3 <- d3 %>%
  group_by(country) %>%
  mutate(cred_cmean = mean(credential),
        bel_cmean = mean(ind_belief)) %>%
  ungroup() %>%
  mutate(credential_w = credential - cred_cmean,
         belief_w = ind_belief - bel_cmean)

#' Between part: country mean minus the avarage of the 25 country means

cm <- d3 %>%
  group_by(country) %>%
  summarise(cred_cmean = first(cred_cmean),
            bel_cmean = first(bel_cmean),
            strat = first(stratification))
d3 <- d3 %>%
  mutate(credential_b = cred_cmean - mean(cm$cred_cmean),
         belief_b = bel_cmean - mean(cm$bel_cmean),
         stratification_c = stratification - mean(cm$strat))

#' Version B: within country-wave centering (same between parts)
d3 <- d3 %>%
  group_by(country, wave) %>%
  mutate(credential_w_cw = credential - mean(credential),
         belief_w_cw = ind_belief - mean(ind_belief)) %>%
  ungroup()

#' Checks

stopifnot(max(abs(tapply(d3$credential_w, d3$country, mean))) < 1e-10)
stopifnot(max(abs(tapply(d3$belief_w,     d3$country, mean))) < 1e-10)
stopifnot(all(tapply(d3$credential_b, d3$country, function(x) length(unique(x))) == 1))
stopifnot(abs(mean(cm$cred_cmean - mean(cm$cred_cmean))) < 1e-10)

cor(d3[c("credential_w", "credential_b", "credential_w_cw")])
d3 %>% group_by(country) %>%
  summarize(sd_diff = sd(credential_w - credential_w_cw)) %>% print(n = 30)

saveRDS(d3, "data/processed/d3_centered.rds")


