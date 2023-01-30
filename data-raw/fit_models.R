# Author: Kevin See
# Purpose: Fit redd observer net error models
# Created: 1/27/23
# Last Modified: 1/27/23
# Notes: one observer data comes from the Wenatchee, two observer from the Methow

#-----------------------------------------------------------------
# load needed libraries
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(janitor)

#-----------------------------------------------------------------
# One observer model
#-----------------------------------------------------------------
# read in data
one_obs_df <- read_csv("data-raw/one_obs_redd_data.csv") |>
  select(year:reach_length_m) |>
  mutate(
    across(
      stream,
      str_to_title
    ),
    across(
      stream,
      recode,
      "Pes" = "Peshastin",
      "Wen" = "Wenatchee"
    )
  ) |>
  mutate(reach_length_km = reach_length_m / 1000,
         naive_density_km = total_features / reach_length_km,
         exp_sp_total_log = log(exp_sp_total),
         net_error = (total_features / visible_redds) - 1) |>
  select(-reach_length_m)

one_obs_covar_center <- one_obs_df |>
  select(exp_sp_total:exp_sp_total_log) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "values"
  ) |>
  group_by(metric) |>
  summarize(
    across(
      values,
      list(mean = mean,
           sd = sd),
      na.rm = TRUE,
      .names = "{.fn}"
    ),
    .groups = "drop")

one_obs_mod_df = one_obs_df |>
  mutate(id = 1:n()) %>%
  select(id,
         net_error,
         mean_depth,
         mean_thalweg_cv,
         naive_density_km) |>
  pivot_longer(cols = -c(id:net_error),
               names_to = "metric",
               values_to = "value") |>
  left_join(one_obs_covar_center) |>
  mutate(across(value,
                ~ (. - mean) / sd)) |>
  select(-c(mean:sd)) |>
  pivot_wider(names_from = metric,
              values_from = value)

one_obs_net_mod <- glm(net_error ~ mean_depth + mean_thalweg_cv + naive_density_km,
      data = one_obs_mod_df,
      family = gaussian)

#-----------------------------------------------------------------
# Two observer model
#-----------------------------------------------------------------
# read in data
two_obs_df <- read_csv("data-raw/two_obs_redd_data.csv") |>
  mutate(naive_density_km = total_features / reach_length_km,
         exp_sp_total_log = log(exp_sp_total),
         net_error = (total_features / visible_redds) - 1) |>
  select(any_of(names(one_obs_df)),
         everything())

two_obs_covar_center <- two_obs_df |>
  select(exp_sp_total:exp_sp_total_log) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "values"
  ) |>
  group_by(metric) |>
  summarize(
    across(
      values,
      list(mean = mean,
           sd = sd),
      na.rm = TRUE,
      .names = "{.fn}"
    ),
    .groups = "drop")

two_obs_mod_df = two_obs_df |>
  mutate(id = 1:n()) %>%
  select(id,
         net_error,
         starts_with('exp_sp_total'),
         mean_discharge,
         mean_thalweg_cv,
         naive_density_km) |>
  pivot_longer(cols = -c(id:net_error),
               names_to = "metric",
               values_to = "value") |>
  left_join(two_obs_covar_center) |>
  mutate(across(value,
                ~ (. - mean) / sd)) |>
  select(-c(mean:sd)) |>
  pivot_wider(names_from = metric,
              values_from = value)

two_obs_net_mod <- glm(net_error ~ exp_sp_total_log + mean_discharge + mean_thalweg_cv + naive_density_km,
                       data = two_obs_mod_df,
                       family = gaussian)

#-----------------------------------------------------------------
# save to be available in package (e.g. to compare range of covariates)
usethis::use_data(one_obs_df)
usethis::use_data(two_obs_df)

# save for use in package functions
usethis::use_data(one_obs_covar_center,
                  one_obs_net_mod,
                  two_obs_covar_center,
                  two_obs_net_mod,
                  internal = T,
                  overwrite = T,
                  version = 2)
