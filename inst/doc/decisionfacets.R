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

## -----------------------------------------------------------------------------
head(oral_exam)
d <- df_data(oral_exam, person = "candidate", item = "case",
             rater = "examiner", scale = "continuous")

## -----------------------------------------------------------------------------
ofit <- df_fit(d)
round(ofit$par$sigma, 1)                # residual SD (score points)
head(df_rater_effects(ofit))
range(round(ofit$par$lambda, 1))        # most lenient and harshest examiners

## -----------------------------------------------------------------------------
cut70 <- df_cut(70, "raw_mean")
cf_oral <- df_counterfactual(ofit, cut70, max_panels = 500, seed = 1)
summary(cf_oral)
df_attribute(cf_oral)

## -----------------------------------------------------------------------------
summary(df_counterfactual(ofit, df_cut(70, "measure"), max_panels = 500, seed = 1))

