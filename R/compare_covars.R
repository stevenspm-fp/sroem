#' @title Covariate Comparison
#'
#' @description Compares covariates of model dataset with predictive dataset
#'
#' @author Kevin See
#'
#' @inheritParams predict_neterr
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @return dataframe
#' @export

compare_covars <- function(redd_df = NULL,
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

  redd_df |>
    dplyr::select(dplyr::any_of(attr(net_err_mod$terms, "term.labels"))) |>
    sroem::predict_neterr(species = species,
                          num_obs = num_obs) |>
    dplyr::select(-net_error_se) |>
    tidyr::pivot_longer(names_to = "covariate",
                        values_to = "value") |>
    dplyr::mutate(source = "Predictive Data") |>
    dplyr::bind_rows(net_err_mod$data |>
                       dplyr::select(dplyr::any_of(attr(net_err_mod$terms, "term.labels")),
                                     net_error) |>
                       dplyr::mutate(source = "Model Data"))


}
