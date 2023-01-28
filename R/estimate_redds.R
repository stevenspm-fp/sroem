#' @title Estimate Redds
#'
#' @description Estimates true number of redds across a series of reaches, using a Gaussian area-under-the-curve model where possible and incorporating estimates of observer error if possible.
#'
#' @author Kevin See
#'
#' @param redd_df dataframe containing redd data
#' @param group_var vector of column names from {redd_df} to group results by
#' @param new_redd_nm quoted name of column in {redd_df} listing number of new redds found during that survey
#' @param vis_redd_nm quoted name of column in {redd_df} listing number of visible redds present during that survey
#' @param net_err_nm quoted name of column in {redd_df} listing estimate of net error for that survey
#' @param net_se_nm quoted name of column in {redd_df} listing standard error of net error estimate for that survey
#' @param min_non0_wks minimum number of weeks with at least one new redd observed
#' @param min_redds minimum number of total redds observed
#' @param add_zeros should leading and trailing zero counts be added if the first and last counts aren't already zero? Default is {FALSE}.
#'
#'
#' @import dplyr msm
#' @return dataframe
#' @export

estimate_redds <- function(redd_df = NULL,
                           group_vars = c(River, Reach, Index, SurveyType),
                           new_redd_nm = "NewRedds",
                           vis_redd_nm = "VisibleRedds",
                           net_err_nm = "NetError",
                           net_se_nm = "NetErrorSE",
                           min_non0_wks = 3,
                           min_redds = 2,
                           add_zeros = F,
                           ...) {
  if (is.null(redd_df)) {
    stop("redd data must be supplied")
  }

  redd_results <- redd_df %>%
    select(all_of({{ group_vars }}),
      new_redds = {{ new_redd_nm }},
      vis_redds = {{ vis_redd_nm }},
      net_err = {{ net_err_nm }},
      net_se = {{ net_se_nm }}
    ) %>%
    group_by(across({{ group_vars }})) %>%
    summarise(
      n_weeks = n(),
      n_non0_wks = sum(new_redds > 0, na.rm = T),
      tot_feat = sum(new_redds, na.rm = T),
      err_est = mean(net_err[vis_redds > 0], na.rm = T),
      err_se = mean(net_se[vis_redds > 0], na.rm = T),
      .groups = "drop"
    ) %>%
    mutate(
      err_se = if_else(is.na(err_est), 0, err_se),
      err_est = if_else(is.na(err_est), 1, err_est)
    ) %>%
    full_join(
      redd_df %>%
        group_by(across({{ group_vars }})) %>%
        nest(),
      by = {{ group_vars }}
    ) %>%
    mutate(gauc_list = map(data,
      .f = function(x) {
        mod_df <- x %>%
          select(redds = {{ new_redd_nm }}) %>%
          mutate(day = 1:n())

        if (add_zeros) {
          if (mod_df$redds[mod_df$day == min(mod_df$day)] != 0) {
            mod_df <- mod_df %>%
              bind_rows(tibble(
                redds = 0,
                day = min(mod_df$day) - 1
              )) %>%
              arrange(day) %>%
              mutate(day = 1:n())
          }
          if (mod_df$redds[mod_df$day == max(mod_df$day)] != 0) {
            mod_df <- mod_df %>%
              bind_rows(tibble(
                redds = 0,
                day = max(mod_df$day) + 1
              )) %>%
              arrange(day)
          }
        }

        v_vec <- x %>%
          rename(
            vis_redds = {{ vis_redd_nm }},
            net_err = {{ net_err_nm }},
            net_se = {{ net_se_nm }}
          ) %>%
          filter(vis_redds > 0) %>%
          summarise_at(vars(net_err, net_se),
            list(mean),
            na.rm = T
          ) %>%
          mutate(
            net_se = if_else(is.na(net_err),
              0,
              net_se
            ),
            net_err = if_else(is.na(net_err),
              1,
              net_err
            )
          )

        # # if you want to assume no observer error
        # v_vec = tibble(net_err = 1,
        #                net_se = 0)

        res_list <- fit_gauc(
          data = mod_df,
          v = v_vec$net_err,
          v_se = v_vec$net_se,
          ...
        )

        res_list <- c(
          res_list,
          list("mod_data" = mod_df)
        )

        return(res_list)
      }
    )) %>%
    mutate(converged = map_lgl(gauc_list,
      .f = function(x) {
        x$model$converged
      }
    )) %>%
    mutate(
      redd_est = map_dbl(gauc_list,
        .f = "E"
      ),
      redd_se = map_dbl(gauc_list,
        .f = "E_se"
      )
    ) %>%
    mutate(GAUC = if_else(!converged | n_non0_wks < min_non0_wks | tot_feat < min_redds,
      F, T
    )) %>%
    rowwise() %>%
    mutate(
      redd_est = if_else(!GAUC,
        tot_feat / err_est,
        redd_est
      ),
      redd_se = if_else(!GAUC,
        msm::deltamethod(~ x1 / x2,
          mean = c(tot_feat, err_est),
          cov = diag(c(0, err_se)^2)
        ),
        redd_se
      )
    ) %>%
    ungroup() %>%
    mutate_at(
      vars(redd_est),
      list(round_half_up)
    )

  return(redd_results)
}
