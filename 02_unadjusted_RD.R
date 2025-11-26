library(dplyr)
library(tidyr)


# ---- Unadjusted Officer vs Enlisted by month (Any & Exclusive) ----
compare_oe <- function(df, outcome_var) {
  # Summarize counts by month × rank
  agg <- df %>%
    filter(officer_enlisted %in% c("Officer","Enlisted")) %>%
    group_by(month, officer_enlisted) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      s = sum(.data[[outcome_var]] == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # ensure all months and both ranks exist; fill missing with 0
    complete(
      month = factor(c("2mo","4mo","6mo"), levels = c("2mo","4mo","6mo")),
      officer_enlisted = c("Officer","Enlisted"),
      fill = list(n = 0L, s = 0L)
    )
  
  # Wide form: separate Officer vs Enlisted columns
  out <- agg %>%
    pivot_wider(
      names_from = officer_enlisted, values_from = c(n, s),
      names_sep = "_"
    ) %>%
    arrange(match(month, c("2mo","4mo","6mo"))) %>%
    mutate(
      p_off = ifelse(n_Officer   > 0, s_Officer   / n_Officer,   NA_real_),
      p_enl = ifelse(n_Enlisted  > 0, s_Enlisted  / n_Enlisted,  NA_real_)
    )
  
  # Row-wise RD CI (Wilson/score) + Fisher p
  out <- out %>%
    rowwise() %>%
    mutate(
      rd_ci = list(
        if (n_Officer > 0 && n_Enlisted > 0)
          DescTools::BinomDiffCI(s_Officer, n_Officer, s_Enlisted, n_Enlisted, method = "score")
        else
          matrix(c(NA, NA, NA), nrow = 1,
                 dimnames = list(NULL, c("est","lwr.ci","upr.ci")))
      ),
      rd_est = rd_ci[, "est"],
      rd_lwr = rd_ci[, "lwr.ci"],
      rd_upr = rd_ci[, "upr.ci"],
      p_fisher = if (n_Officer > 0 && n_Enlisted > 0)
        fisher.test(matrix(c(s_Officer, n_Officer - s_Officer,
                             s_Enlisted, n_Enlisted - s_Enlisted),
                           nrow = 2, byrow = TRUE))$p.value
      else NA_real_
    ) %>%
    ungroup() %>%
    select(-rd_ci)
  
  # Return a plain data.frame with safe column names
  out %>%
    transmute(
      month,
      officer_pct          = round(100 * p_off, 1),
      enlisted_pct         = round(100 * p_enl, 1),
      rd_pp                = ifelse(is.na(rd_est), "NA",
                                    sprintf("%.1f (%.1f to %.1f)",
                                            100 * rd_est, 100 * rd_lwr, 100 * rd_upr)),
      p_fisher             = signif(p_fisher, 3),
      officer_s_over_n     = sprintf("%d/%d", s_Officer, n_Officer),
      enlisted_s_over_n    = sprintf("%d/%d", s_Enlisted, n_Enlisted)
    ) %>%
    as.data.frame()
}

step0_any  <- compare_oe(dat, "any_bf");  step0_any$outcome  <- "Any_BF"
step0_excl <- compare_oe(dat, "excl_bf"); step0_excl$outcome <- "Exclusive_BF"

results_step0 <- bind_rows(step0_any, step0_excl) |>
  relocate(outcome, .before = 1) |>
  arrange(outcome, match(month, c("2mo","4mo","6mo"))) |>
  as.data.frame()
print(results_step0)


