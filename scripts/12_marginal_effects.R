#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 12_marginal_effects
#' -------------------

library(emmeans)

d3   <- readRDS("data/processed/d3_models.rds")
mods <- readRDS("data/processed/models_h123.rds")
m_h1 <- mods$m_h1; m_h2 <- mods$m_h2; m_h3 <- mods$m_h3

#' H1: credential gap at belief -1 SD, 0, +1 SD (within country)

sd_b <- sd(d3$belief_w)
emtrends(m_h1, ~ belief_w, var = "credential_w",
         at = list(belief_w = c(-sd_b, 0, sd_b)))

#' H2: credential gap at the lowest, average and highest stratification
emtrends(m_h2, ~ stratification_c, var = "credential_w",
         at = list(stratification_c = c(min(d3$stratification_c), 0, max(d3$stratification_c))))

library(ggplot2)

#' H1 plot: credential gap across the range of belief
b_grid <- seq(-2, 2, by = 0.25)
h1 <- as.data.frame(
  confint(emtrends(m_h1, ~ belief_w, var = "credential_w",
                   at = list(belief_w = b_grid)), level = 0.835))

ggplot(h1, aes(belief_w, credential_w.trend)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  geom_line() +
  labs(x = "Meritocratic belief (within country, centered)",
       y = "Credential gap in SSS",
       caption = "Shaded band: 83.5% CI") +
  theme_minimal()

#' H2 plot: credential gap across the range of stratification
s_grid <- seq(min(d3$stratification_c), max(d3$stratification_c), length.out = 20)
h2 <- as.data.frame(
  confint(emtrends(m_h2, ~ stratification_c, var = "credential_w",
                   at = list(stratification_c = s_grid)), level = 0.835))

ggplot(h2, aes(stratification_c, credential_w.trend)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  geom_line() +
  labs(x = "Educational stratification (centered)",
       y = "Credential gap in SSS",
       caption = "Shaded band: 83.5% CI") +
  theme_minimal()

