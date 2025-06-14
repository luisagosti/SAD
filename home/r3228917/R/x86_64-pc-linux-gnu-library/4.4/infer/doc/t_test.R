## ----settings, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4.5) 
options(digits = 4)

## ----load-packages, echo = FALSE, message = FALSE, warning = FALSE------------
library(ggplot2)
library(dplyr)
library(infer)

## ----glimpse-gss-actual, warning = FALSE, message = FALSE---------------------
dplyr::glimpse(gss)

## ----plot-1-sample, echo = FALSE----------------------------------------------
gss %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = hours) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::labs(
    x = "hours: Number of Hours Worked",
    y = "Number of Responses"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, 90, 10))

## ----calc-obs-stat-1-sample, warning = FALSE, message = FALSE-----------------
# calculate the observed statistic
observed_statistic <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")

## ----generate-null-1-sample, warning = FALSE, message = FALSE-----------------
# generate the null distribution
null_dist_1_sample <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

## ----visualize-1-sample, warning = FALSE, message = FALSE---------------------
# visualize the null distribution and test statistic!
null_dist_1_sample %>%
  visualize() +
  shade_p_value(observed_statistic,
    direction = "two-sided"
  )

## ----p-value-1-sample, warning = FALSE, message = FALSE-----------------------
# calculate the p value from the test statistic and null distribution
p_value_1_sample <- null_dist_1_sample %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

p_value_1_sample

## ----t-test-wrapper, message = FALSE, warning = FALSE-------------------------
t_test(gss, response = hours, mu = 40)

## -----------------------------------------------------------------------------
# calculate the observed statistic
observed_statistic <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t") %>%
  dplyr::pull()

## -----------------------------------------------------------------------------
pt(unname(observed_statistic), df = nrow(gss) - 1, lower.tail = FALSE)*2

## ----plot-2-sample, echo = FALSE----------------------------------------------
gss %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = college, y = hours) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "college: Whether the Respondent has a College Degree",
                y = "hours: Number of Hours Worked")

## ----calc-obs-stat-2-sample, warning = FALSE, message = FALSE-----------------
# calculate the observed statistic
observed_statistic <- gss %>%
  specify(hours ~ college) %>%
  calculate(stat = "diff in means", order = c("degree", "no degree"))

observed_statistic

## ----generate-null-2-sample, warning = FALSE, message = FALSE-----------------
# generate the null distribution with randomization
null_dist_2_sample <- gss %>%
  specify(hours ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("degree", "no degree"))

## ----visualize-2-sample, warning = FALSE, message = FALSE---------------------
# visualize the randomization-based null distribution and test statistic!
null_dist_2_sample %>%
  visualize() + 
  shade_p_value(observed_statistic,
                direction = "two-sided")

## ----p-value-2-sample, warning = FALSE, message = FALSE-----------------------
# calculate the p value from the randomization-based null 
# distribution and the observed statistic
p_value_2_sample <- null_dist_2_sample %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

p_value_2_sample

## ----2-sample-t-test-wrapper, message = FALSE, warning = FALSE----------------
t_test(x = gss, 
       formula = hours ~ college, 
       order = c("degree", "no degree"),
       alternative = "two-sided")

## -----------------------------------------------------------------------------
# calculate the observed statistic
observed_statistic <- gss %>%
  specify(hours ~ college) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t", order = c("degree", "no degree")) %>%
  dplyr::pull()

observed_statistic

## -----------------------------------------------------------------------------
pt(unname(observed_statistic), df = nrow(gss) - 2, lower.tail = FALSE)*2

