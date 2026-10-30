#' ------------------------------
#' CHAPTER 1 - RECOGNITION INJURY
#' ------------------------------
#' 09_null_model
#' -------------

library(dplyr)
library(lme4)
library(lmerTest)

d3 <- readRDS("data/processed/d3_centered.rds")

m0 <- lmer(sss ~ 1 + (1 | country), data = d3)
summary(m0)

vc  <- as.data.frame(VarCorr(m0))
icc <- vc$vcov[vc$grp == "country"] / sum(vc$vcov)
icc

m0b <- lmer(sss ~ 1 + (1 | country) + (1 | country:wave), data = d3)
summary(m0b)

vc2 <- as.data.frame(VarCorr(m0b))
vc2$share <- vc2$vcov / sum(vc2$vcov)
vc2[, c("grp", "vcov", "share")]

anova(m0, m0b)