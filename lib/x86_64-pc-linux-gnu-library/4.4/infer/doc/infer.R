## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4.5) 
options(digits = 4)

## ----load-packages, echo = FALSE, message = FALSE, warning = FALSE------------
library(dplyr)
library(infer)

## ----load-gss, warning = FALSE, message = FALSE-------------------------------
# load in the dataset
data(gss)

# take a look at its structure
dplyr::glimpse(gss)

## ----specify-example, warning = FALSE, message = FALSE------------------------
gss %>%
  specify(response = age)

## ----specify-one, warning = FALSE, message = FALSE----------------------------
gss %>%
  specify(response = age) %>%
  class()

## ----specify-two, warning = FALSE, message = FALSE----------------------------
# as a formula
gss %>%
  specify(age ~ partyid)

# with the named arguments
gss %>%
  specify(response = age, explanatory = partyid)

## ----specify-success, warning = FALSE, message = FALSE------------------------
# specifying for inference on proportions
gss %>%
  specify(response = college, success = "degree")

## ----hypothesize-independence, warning = FALSE, message = FALSE---------------
gss %>%
  specify(college ~ partyid, success = "degree") %>%
  hypothesize(null = "independence")

## ----hypothesize-40-hr-week, warning = FALSE, message = FALSE-----------------
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40)

## ----generate-point, warning = FALSE, message = FALSE-------------------------
set.seed(1)

gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap")

## ----generate-permute, warning = FALSE, message = FALSE-----------------------
gss %>%
  specify(partyid ~ age) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")

## ----calculate-point, warning = FALSE, message = FALSE------------------------
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

## ----specify-diff-in-means, warning = FALSE, message = FALSE------------------
gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate("diff in means", order = c("degree", "no degree"))

## ----utilities-examples-------------------------------------------------------
# find the point estimate
obs_mean <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")

# generate a null distribution
null_dist <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

## ----visualize, warning = FALSE, message = FALSE------------------------------
null_dist %>%
  visualize()

## ----visualize2, warning = FALSE, message = FALSE-----------------------------
null_dist %>%
  visualize() +
  shade_p_value(obs_stat = obs_mean, direction = "two-sided")

## ----get_p_value, warning = FALSE, message = FALSE----------------------------
# get a two-tailed p-value
p_value <- null_dist %>%
  get_p_value(obs_stat = obs_mean, direction = "two-sided")

p_value

## ----get_conf, message = FALSE, warning = FALSE-------------------------------
# generate a distribution like the null distribution, 
# though exclude the null hypothesis from the pipeline
boot_dist <- gss %>%
  specify(response = hours) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

# start with the bootstrap distribution
ci <- boot_dist %>%
  # calculate the confidence interval around the point estimate
  get_confidence_interval(
    point_estimate = obs_mean,
    # at the 95% confidence level
    level = .95,
    # using the standard error
    type = "se"
  )

ci

## ----visualize-ci, warning = FALSE, message = FALSE---------------------------
boot_dist %>%
  visualize() +
  shade_confidence_interval(endpoints = ci)

## ----message = FALSE, warning = FALSE-----------------------------------------
# calculate an observed t statistic
obs_t <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")

## ----message = FALSE, warning = FALSE-----------------------------------------
# switch out calculate with assume to define a distribution
t_dist <- gss %>%
  specify(response = hours) %>%
  assume(distribution = "t")

## ----message = FALSE, warning = FALSE-----------------------------------------
# visualize the theoretical null distribution
visualize(t_dist) +
  shade_p_value(obs_stat = obs_t, direction = "greater")

# more exactly, calculate the p-value
get_p_value(t_dist, obs_t, "greater")

## ----message = FALSE, warning = FALSE-----------------------------------------
# find the theory-based confidence interval
theor_ci <- 
  get_confidence_interval(
    x = t_dist,
    level = .95,
    point_estimate = obs_mean
  )

theor_ci

## -----------------------------------------------------------------------------
# visualize the theoretical sampling distribution
visualize(t_dist) +
  shade_confidence_interval(theor_ci)

## -----------------------------------------------------------------------------
observed_fit <- gss %>%
  specify(hours ~ age + college) %>%
  fit()

## -----------------------------------------------------------------------------
null_fits <- gss %>%
  specify(hours ~ age + college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  fit()

null_fits

## -----------------------------------------------------------------------------
get_confidence_interval(
  null_fits, 
  point_estimate = observed_fit, 
  level = .95
)

## -----------------------------------------------------------------------------
visualize(null_fits) + 
  shade_p_value(observed_fit, direction = "both")

