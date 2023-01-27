#' @title Predict Net Error
#'
#' @description Predict the net error of a steelhead redd survey, based on published model
#'
#' @author Kevin See
#'
#' @param data dataframe containing columns with covariates in observer error model
#' @param num_obs which error model to use, for one or two observers? Default is \code{two}.
#'
#' @import dplyr tidyr tibble
#' @return tibble
#' @export

predict_neterr = function(data,
                          num_obs = c('two', 'one')) {

  num_obs = match.arg(num_obs)

  if(num_obs == 'two') {
    net_err_mod = two_obs_net_mod
    covar_center = two_obs_covar_center
  }

  if(num_obs == 'one') {
    net_err_mod = one_obs_net_mod
    covar_center = one_obs_covar_center
  }

  if(sum(!attr(net_err_mod$terms, "term.labels") %in% names(data)) > 0) {
    miss_covar = attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(data)]
    stop(paste("Missing covariates in dataset:", paste(miss_covar, collapse = ', ')))
  }

  pred_df = data %>%
    mutate(data_id = 1:n()) %>%
    tidyr::pivot_longer(any_of(covar_center$metric),
                        names_to = "metric",
                        values_to = "value") %>%
    left_join(covar_center,
              by = "metric") %>%
    mutate(value = (value - mu) / stddev ) %>%
    select(-mu, -stddev) %>%
    tidyr::pivot_wider(names_from = "metric",
                       values_from = "value") %>%
    bind_cols(predict(net_err_mod,
                      newdata = .,
                      backtransform = T,
                      type = 'link',
                      se.fit = T) %>%
                tibble::as_tibble() %>%
                select(NetError = fit,
                       NetErrorSE = se.fit))

  pred_df = pred_df %>%
    select(-any_of(covar_center$metric)) %>%
    left_join(data %>%
                mutate(data_id = 1:n())) %>%
    select(-data_id) %>%
    select(any_of(names(data)), everything())

  return(pred_df)

}
