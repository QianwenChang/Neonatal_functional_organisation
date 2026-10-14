run_lm_PMA <- function(df, var) {
  
  formula_str <- paste(var, "~ nscan_ga_at_scan_weeks + fd_outliers + gender")
  lm_fit <- lm(as.formula(formula_str), data = df)
  
  pcor_res <- ppcor::pcor(
    df %>%
      dplyr::select(dplyr::all_of(c(var, "nscan_ga_at_scan_weeks", "fd_outliers", "baby_gender"))) %>%
      na.omit(),
    method = "pearson"
  )
  
  coef <- summary(lm_fit)$coefficients
  
  data.frame(
    var = var,
    t = coef["nscan_ga_at_scan_weeks", "t value"],
    p = coef["nscan_ga_at_scan_weeks", "Pr(>|t|)"],
    r = pcor_res$estimate[var, "nscan_ga_at_scan_weeks"],
    stringsAsFactors = FALSE
  )
}