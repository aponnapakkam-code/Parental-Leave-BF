library(dplyr)

test_rank_epoch <- function(df, outcome) {
  levels_month <- levels(df$month)
  out <- lapply(levels_month, function(mo){
    d <- filter(df, month == mo)
    fit0 <- glm(reformulate(c("officer_enlisted","epoch"), outcome),
                family = binomial, data = d)
    fit1 <- glm(reformulate(c("officer_enlisted","epoch","officer_enlisted:epoch"), outcome),
                family = binomial, data = d)
    tibble(
      month = mo,
      p_interaction = as.numeric(anova(fit0, fit1, test = "LRT")[2,"Pr(>Chi)"])
    )
  })
  bind_rows(out)
}

interact_any<-test_rank_epoch(dat, "any_bf")
interact_excl<-test_rank_epoch(dat, "excl_bf")
