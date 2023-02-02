#' @title Predict Net Error
#'
#' @description Predict the net error of a steelhead or Spring Chinook redd survey, based on published model
#'
#' @author Kevin See
#'
#' @param redd_df dataframe containing redd data including columns with covariates in selected observer error model
#' @param species which species is being analyzed? This function chooses the appropriate error model for this species. Choices are \code{Steelhead} or \code{Chinook}, with \code{Steelhead} being the default.
#' @param num_obs if species is \code{Steelhead}, which error model to use, for one or two observers? Default is \code{two}.
#'
#' @import dplyr tidyr tibble
#' @return tibble
#' @export

predict_neterr <- function(redd_df = NULL,
                           species = c("Steelhead", "Spring Chinook"),
                           num_obs = c("two", "one")) {

  if(is.null(redd_df)) {
    stop("Redd data not supplied.")
  }

  species <- match.arg(species)
  num_obs <- match.arg(num_obs)

  if(species == "Steelhead") {
    if (num_obs == "two") {
      net_err_mod <- two_obs_net_mod
      covar_center <- two_obs_covar_center
    }

    if (num_obs == "one") {
      net_err_mod <- one_obs_net_mod
      covar_center <- one_obs_covar_center
    }
  }

  if(species == "Spring Chinook") {
    net_err_mod <- chnk_net_mod
    covar_center <- chnk_covar_center
  }

  if (sum(!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)) > 0) {
    miss_covar <- attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)]

    if("exp_sp_total_log" %in% miss_covar & "exp_sp_total" %in% names(redd_df)) {
      redd_df <- redd_df |>
        mutate(exp_sp_total_log = log(exp_sp_total))
      miss_covar <- attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)]
    }

    if("naive_density_km" %in% miss_covar & sum(c("visible_redds", "reach_length_km") %in% names(redd_df)) == 2) {
      redd_df <- redd_df |>
        mutate(naive_density_km = visible_redds / reach_length_km)
      miss_covar <- attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(redd_df)]
    }

    if(length(miss_covar) > 0) {
      stop(paste("Missing covariates in dataset:", paste(miss_covar, collapse = ", ")))
    }
  }

  pred_df <- redd_df %>%
    mutate(data_id = 1:n()) %>%
    tidyr::pivot_longer(any_of(covar_center$metric),
                        names_to = "metric",
                        values_to = "value"
    ) %>%
    left_join(covar_center,
              by = "metric"
    ) %>%
    mutate(value = (value - mean) / sd) %>%
    select(-mean, -sd) %>%
    tidyr::pivot_wider(
      names_from = "metric",
      values_from = "value"
    ) %>%
    bind_cols(predict(net_err_mod,
                      newdata = .,
                      backtransform = T,
                      type = "link",
                      se.fit = T
    ) %>%
      tibble::as_tibble() %>%
      select(
        net_error = fit,
        net_error_se = se.fit
      ))

  pred_df <- pred_df %>%
    select(-any_of(covar_center$metric)) %>%
    left_join(redd_df %>%
                mutate(data_id = 1:n())) %>%
    select(-data_id) %>%
    select(any_of(names(redd_df)), everything())

  return(pred_df)
}
