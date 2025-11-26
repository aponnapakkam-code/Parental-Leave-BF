library(dplyr)

baseline_covars0 <- c(
  "maternal_age","mothers_marrital_status","branch","parity",
  "delivery_type","term_preterm","nicu_admission","infant_gender2"
)

keep_vars_with_variation <- function(df, vars){
  vars[vapply(vars, function(v){
    x <- df[[v]]
    if (is.factor(x)) x <- droplevels(x)
    length(unique(x[!is.na(x)])) >= 2
  }, logical(1))]
}

# Bias-reduced logistic via brglm2 if available; falls back to plain glm otherwise
fit_binom <- function(formula, data){
  if (requireNamespace("brglm2", quietly = TRUE)) {
    glm(formula, family = binomial, data = data, method = brglm2::brglmFit)
  } else {
    glm(formula, family = binomial, data = data)
  }
}

# Bootstrap adjusted RD (Officer − Enlisted) in percentage points
rd_bootstrap <- function(formula, d, B = 200){
  # ensure consistent factor levels for officer_enlisted
  d$officer_enlisted <- factor(d$officer_enlisted, levels = c("Officer", "Enlisted"))
  
  # Fit on full data for point estimate
  fit <- fit_binom(formula, d)
  
  d_off <- d
  d_enl <- d
  d_off$officer_enlisted <- factor("Officer",  levels = c("Officer","Enlisted"))
  d_enl$officer_enlisted <- factor("Enlisted", levels = c("Officer","Enlisted"))
  
  p_off_hat <- mean(predict(fit, newdata = d_off, type = "response"))
  p_enl_hat <- mean(predict(fit, newdata = d_enl, type = "response"))
  RD_hat    <- 100 * (p_off_hat - p_enl_hat)
  
  # Bootstrap distribution of RD
  boot_rds <- replicate(B, {
    idx <- sample(seq_len(nrow(d)), replace = TRUE)
    d_b <- d[idx, , drop = FALSE]
    
    # need both ranks in the bootstrap sample
    if (dplyr::n_distinct(d_b$officer_enlisted) < 2L) return(NA_real_)
    d_b$officer_enlisted <- factor(d_b$officer_enlisted,
                                   levels = c("Officer","Enlisted"))
    
    fit_b <- try(fit_binom(formula, d_b), silent = TRUE)
    if (inherits(fit_b, "try-error")) return(NA_real_)
    
    d_off_b <- d_b
    d_enl_b <- d_b
    d_off_b$officer_enlisted <- factor("Officer",  levels = c("Officer","Enlisted"))
    d_enl_b$officer_enlisted <- factor("Enlisted", levels = c("Officer","Enlisted"))
    
    p_off_b <- mean(predict(fit_b, newdata = d_off_b, type = "response"))
    p_enl_b <- mean(predict(fit_b, newdata = d_enl_b, type = "response"))
    
    100 * (p_off_b - p_enl_b)
  })
  
  boot_rds <- boot_rds[!is.na(boot_rds)]
  
  if (length(boot_rds) < 10) {
    # not enough valid resamples; return point estimate only
    c(est = RD_hat, lwr = NA_real_, upr = NA_real_)
  } else {
    qs <- quantile(boot_rds, c(0.025, 0.975))
    c(est = RD_hat, lwr = unname(qs[1]), upr = unname(qs[2]))
  }
}

# Adjusted RD via marginal standardization; optionally stratify by epoch
run_models_std <- function(mo, outcome, strat_epoch = NULL){
  d <- dat %>%
    filter(month == mo, officer_enlisted %in% c("Officer","Enlisted")) %>%
    { if (is.null(strat_epoch)) . else filter(., epoch == strat_epoch) } %>%
    droplevels()
  
  # guard: need rows, both ranks, and outcome present
  if (nrow(d) == 0L ||
      dplyr::n_distinct(d$officer_enlisted) < 2L ||
      all(is.na(d[[outcome]]))) {
    
    return(tibble(
      month   = if (is.null(strat_epoch)) mo else paste0(mo, " (", strat_epoch, ")"),
      outcome = if (outcome == "any_bf") "Any BF" else "Exclusive BF",
      RD_total_pp   = NA_character_,
      RD_total_lcl  = NA_real_,
      RD_total_ucl  = NA_real_,
      RD_direct_pp  = NA_character_,
      RD_direct_lcl = NA_real_,
      RD_direct_ucl = NA_real_,
      p_total  = NA_real_,
      p_direct = NA_real_
    ))
  }
  
  # ensure consistent factor levels
  d$officer_enlisted <- factor(d$officer_enlisted,
                               levels = c("Officer","Enlisted"))
  
  # choose covariates that vary in this slice
  varsA <- keep_vars_with_variation(d, baseline_covars0)
  
  # build birth_any, used only for the direct-effect model
  d <- d %>%
    mutate(
      birth_any = case_when(
        birth_admission_feeding_type %in% c("Breast milk","Combo") ~ "Any",
        birth_admission_feeding_type %in% c("Formula")             ~ "Formula",
        TRUE ~ "Missing"
      ),
      birth_any = factor(birth_any, levels = c("Any","Formula","Missing"))
    )
  
  has_birth_var <- dplyr::n_distinct(d$birth_any[d$birth_any != "Missing"]) >= 2
  
  # ---- Model A (total disparity): officer_enlisted + baseline covars (no birth_any)
  fA <- as.formula(
    paste(
      outcome, "~ officer_enlisted",
      if (length(varsA)) paste("+", paste(varsA, collapse = " + ")) else ""
    )
  )
  rdA <- rd_bootstrap(fA, d)  # c(est, lwr, upr)
  
  # ---- Model B (direct disparity): add birth_any if it varies
  varsB <- c(varsA, if (has_birth_var) "birth_any")
  fB <- as.formula(
    paste(
      outcome, "~ officer_enlisted",
      if (length(varsB)) paste("+", paste(varsB, collapse = " + ")) else ""
    )
  )
  rdB <- rd_bootstrap(fB, d)
  
  tibble(
    month   = if (is.null(strat_epoch)) mo else paste0(mo, " (", strat_epoch, ")"),
    outcome = if (outcome == "any_bf") "Any BF" else "Exclusive BF",
    
    # formatted point estimates
    RD_total_pp   = sprintf("%.1f", rdA["est"]),
    RD_direct_pp  = sprintf("%.1f", rdB["est"]),
    
    # numeric CIs (pp)
    RD_total_lcl  = round(rdA["lwr"], 1),
    RD_total_ucl  = round(rdA["upr"], 1),
    RD_direct_lcl = round(rdB["lwr"], 1),
    RD_direct_ucl = round(rdB["upr"], 1),
    
    # p-values left as NA; CIs carry the inferential info
    p_total  = NA_real_,
    p_direct = NA_real_
  )
}

# --- run pooled per-month models
months <- dat %>%
  distinct(month) %>%
  pull() %>%
  as.character() %>%
  (\(x) x[!is.na(x)])()

res_adj_pooled <- dplyr::bind_rows(
  lapply(months, run_models_std, outcome = "any_bf"),
  lapply(months, run_models_std, outcome = "excl_bf")
) %>%
  arrange(outcome, match(month, c("2mo","4mo","6mo")))

# --- and 4-month stratified by epoch (since we saw interaction there)
res_adj_4mo_by_epoch <- dplyr::bind_rows(
  run_models_std("4mo", "any_bf",  strat_epoch = "Pre"),
  run_models_std("4mo", "any_bf",  strat_epoch = "Post"),
  run_models_std("4mo", "excl_bf", strat_epoch = "Pre"),
  run_models_std("4mo", "excl_bf", strat_epoch = "Post")
)

res_adj_pooled
res_adj_4mo_by_epoch

write.csv(res_adj_pooled, "brglm_pooled.csv", row.names = FALSE)
write.csv(res_adj_4mo_by_epoch, "brglm_4_mo.csv", row.names = FALSE)
