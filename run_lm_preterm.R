library(parameters)
run_lm_preterm <- function(df, var) {
  
  formula_str <- paste(var, "~ nscan_ga_at_scan_weeks + fd_outliers + factor(baby_gender) + term_status")
  lm_fit <- lm(as.formula(formula_str), data = df)
  std_coef <- standardize_parameters(
    lm_fit,
    method = "basic"
  )
  
  pcor_res <- pcor(
    df %>%
      dplyr::select(all_of(c(var, "nscan_ga_at_scan_weeks", "fd_outliers", "baby_gender", "term_status_numeric"))) %>%
      na.omit(),
    method = "pearson"
  )
  
  coef <- summary(lm_fit)$coefficients
  term_row <- grep("term_status", rownames(coef), value = TRUE)
  
  data.frame(
    var = var,
    t = coef[term_row, "t value"],
    p = coef[term_row, "Pr(>|t|)"],
    r = pcor_res$estimate[var, "term_status_numeric"],
    beta = unname(std_coef$Std_Coefficient[grepl("term_status", std_coef$Parameter)]),
    stringsAsFactors = FALSE
  )
}