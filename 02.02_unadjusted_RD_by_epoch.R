library(dplyr)
library(tidyr)
library(DescTools)

# epoch-specific O–E RD (percentage points) for one outcome
oe_gap_by_epoch <- function(df, outcome_var){
  df %>%
    group_by(epoch, month, officer_enlisted) %>%
    summarise(
      n = sum(!is.na(.data[[outcome_var]])),
      s = sum(.data[[outcome_var]] == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = officer_enlisted, values_from = c(n, s)) %>%
    arrange(epoch, match(month, c("2mo","4mo","6mo"))) %>%
    rowwise() %>%
    mutate(
      #RD+CI
      rd_ci = list(
        if (n_Officer > 0 && n_Enlisted > 0)
          DescTools::BinomDiffCI(s_Officer, n_Officer, s_Enlisted, n_Enlisted, method = "score")
        else matrix(c(NA, NA, NA), nrow = 1, dimnames = list(NULL, c("est","lwr.ci","upr.ci")))
      ),
      rd_pp   = 100 * rd_ci[, "est"],
      rd_lcl  = 100 * rd_ci[, "lwr.ci"],
      rd_ucl  = 100 * rd_ci[, "upr.ci"],
      # Fisher’s exact test: Officer vs Enlisted within epoch/month
      fisher_p = if (n_Officer > 0 && n_Enlisted > 0) {
        mat <- matrix(
          c(
            s_Officer,               n_Officer - s_Officer,
            s_Enlisted,              n_Enlisted - s_Enlisted
          ),
          nrow = 2, byrow = TRUE
        )
        fisher.test(mat)$p.value
      } else {
        NA_real_
      }
    ) %>%
    ungroup() %>%
    transmute(
      epoch, month,
      officer_pct   = round(100 * s_Officer / pmax(n_Officer, 1), 1),
      enlisted_pct  = round(100 * s_Enlisted / pmax(n_Enlisted, 1), 1),
      `O−E RD (pp)` = ifelse(is.na(rd_pp), "NA",
                             sprintf("%.1f (%.1f to %.1f)", rd_pp, rd_lcl, rd_ucl)),
      `Officer n/N`  = sprintf("%d/%d", s_Officer, n_Officer),
      `Enlisted n/N` = sprintf("%d/%d", s_Enlisted, n_Enlisted),
      p_fisher       = fisher_p
    )
}

# Run for Any and Exclusive
oe_any_by_epoch  <- oe_gap_by_epoch(dat, "any_bf")  %>% mutate(outcome = "Any BF", .before = 1)
oe_excl_by_epoch <- oe_gap_by_epoch(dat, "excl_bf") %>% mutate(outcome = "Exclusive BF", .before = 1)

epoch_RD<-bind_rows(oe_any_by_epoch, oe_excl_by_epoch) %>% arrange(outcome, epoch, month)
write.csv(epoch_RD, "o_e_unadj_by_epoch.csv", row.names = FALSE)