#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 14_leave_one_country_out
#' ------------------------

countries <- sort(unique(as.character(d3$country)))

loo <- function(model, term) {
  out <- lapply(countries, function(cn) {
    fit <- update(model, data = d3 %>% filter(country != cn))
    s <- summary(fit)$coefficients
    data.frame(dropped = cn,
               estimate = s[term, "Estimate"],
               p = s[term, "Pr(>|t|)"])
  })
  bind_rows(out)
}

loo_h1 <- loo(m_h1, "credential_w:belief_w")
loo_h1 %>% arrange(desc(p))

loo_h2 <- loo(m_h2, "credential_w:stratification_c")
loo_h2 %>% arrange(estimate)

range(loo_h1$estimate); sum(loo_h1$p < .05)
range(loo_h2$estimate); sum(loo_h2$p < .05)

