library(dplyr)
library(ggplot2)
library(ggtext)
library(parameters)
boxplot_preterm<- function(df, var, y_label) {

  formula_str <- paste(var, "~ nscan_ga_at_scan_weeks + term_status + fd_outliers + gender")
  lm_fit <- lm(as.formula(formula_str), data = df)
  std_coef <- standardize_parameters(
    lm_fit,
    method = "basic"
  )
  
  coef <- summary(lm_fit)$coefficients
  term_status_term <- which(grepl("^term_status", rownames(coef)))[1]
  p   <- coef[term_status_term, "Pr(>|t|)"]
  beta <- round(std_coef$Std_Coefficient[
    grepl("term_status", std_coef$Parameter)
  ], 3)
  
  sig   <- dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
  p_text <- ifelse(p < 0.001, " < 0.001", sprintf(" = %.3f", p))
  
  label <-paste0(
    "*β* = ", beta, "<br>",
    "<sup>", sig, "</sup> *p* ", p_text
  )
  
  p <- ggplot(df, 
              aes(x = term_status, y = .data[[var]], 
                  fill = term_status, color = term_status)) +
    geom_point(position = position_jitter(w = .15), 
               size = 1,alpha = 0.5,show.legend = F) + 
    geom_boxplot(width = .25, color = "black", 
                 outlier.shape = NA,fill = NA, alpha = 0.5) + 
    scale_x_discrete(limits = c("Preterm", "Term")) +
    ggtext::geom_richtext(
      x = I(0.99), y = I(0.90),label = label,
      color = "black", size = 4.5, fill = NA, label.color = NA) +
    labs(x = NULL, y = NULL) + 
    scale_fill_manual(values = c("Preterm" = "#FF4136", "Term" = "#2980b9")) +
    scale_color_manual(values = c("Preterm" = "#FF4136", "Term" = "#2980b9")) +
    theme_minimal() + 
    theme(
      panel.border      = element_rect(color = "black", fill = NA, size = 1),
      panel.background  = element_rect(fill = NA, color = NA),
      plot.background   = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      legend.position   = "bottom",                                      
      legend.text       = element_text(size = 16, color = "black"), 
      aspect.ratio      = 1.15,
      axis.text.y       = element_text(size = 16, color = "black", margin = margin(r = 5)),
      axis.text.x       = element_text(size = 16, color = "black"),   
      panel.grid        = element_blank(),
      axis.ticks        = element_line(color = "black", size = 0.5),     
      axis.ticks.length = unit(0.1, "cm")
    )
}