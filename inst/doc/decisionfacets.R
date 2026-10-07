## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(decisionfacets)
sim <- df_simulate(n_persons = 400, n_items = 4, n_raters = 12,
                   raters_per_person = 2, severity_sd = 0.6, seed = 2026)
head(sim$data)
round(sim$par$lambda, 2)   # true rater severities (positive = harsher)

## -----------------------------------------------------------------------------
fit <- df_fit(sim$data, engine = "jmle")
df_rater_effects(fit)

## -----------------------------------------------------------------------------
raw <- df_cut(2 * 4 * 2, "raw_total")
fa  <- df_cut(2, "fair_average")

## -----------------------------------------------------------------------------
cf_raw <- df_counterfactual(fit, raw)
cf_raw
summary(cf_raw)
summary(df_counterfactual(fit, fa))

## -----------------------------------------------------------------------------
df_attribute(cf_raw)

## -----------------------------------------------------------------------------
truth <- df_counterfactual(sim, raw)
cor(truth$advantage, cf_raw$advantage)
table(truth = truth$rater_dependent, estimated = cf_raw$rater_dependent)

