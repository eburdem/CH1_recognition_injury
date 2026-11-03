#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 13_country_controls
#' -------------------

names(d3)

d3 %>% group_by(country) %>%
  summarise(across(c(gini, gdp_pc, tertiary), ~ n_distinct(.x))) %>%
  summarise(across(-country, max))

#' Country means, then centered on the mean of the 25 country means
d3 <- d3 %>%
  group_by(country) %>%
  mutate(gini_cm = mean(gini),
         gdp_cm  = mean(log(gdp_pc)),
         tert_cm = mean(tertiary)) %>%
  ungroup() %>%
  mutate(gini_c     = gini_cm - mean(unique(gini_cm)),
         gdp_c      = gdp_cm  - mean(unique(gdp_cm)),
         tertiary_c = tert_cm - mean(unique(tert_cm)))

stopifnot(abs(mean(unique(d3$gini_c))) < 1e-8)
stopifnot(n_distinct(d3$gini_cm) == n_distinct(d3$country))


d3 %>% distinct(country, credential_b, tertiary_c) %>%
  summarise(sd_cred_b = sd(credential_b), r = cor(credential_b, tertiary_c))


m_h2_ctrl <- lmer(sss ~ credential_w * stratification_c + credential_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w | country), data = d3)
summary(m_h2_ctrl)


m_h1_ctrl <- lmer(sss ~ credential_w * belief_w + credential_b + belief_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w | country), data = d3)

m_h3_ctrl <- lmer(sss ~ credential_w * belief_w * stratification_c + credential_b + belief_b +
                    gini_c + gdp_c + tertiary_c +
                    factor(wave) + income + isei_w + isei_b + age10 + age10_sq + gender +
                    (1 + credential_w | country), data = d3)

summary(m_h1_ctrl)
summary(m_h3_ctrl)


