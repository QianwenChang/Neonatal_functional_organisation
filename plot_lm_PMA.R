library(ppcor)
library(marginaleffects)
plot_lm_PMA <- function(df,var,y_title) {
  
  formula_str <- paste(var, "~ nscan_ga_at_scan_weeks + fd_outliers + gender")
  lm_fit <- lm(as.formula(formula_str), data = df)
  
  pcor_res <- pcor(
    df %>%
      dplyr::select(all_of(c(var, "nscan_ga_at_scan_weeks", "fd_outliers", "baby_gender"))) %>%
      na.omit(),
    method = "pearson"
  )
  
  coef <- summary(lm_fit)$coefficients
  p     <- coef["nscan_ga_at_scan_weeks", "Pr(>|t|)"]
  r     <- round(pcor_res$estimate[var, "nscan_ga_at_scan_weeks"], 3)
  sig   <- dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
  p_text <- if (p < 0.001) " < 0.001" else sprintf(" = %.3f", p)

  label <- paste0("*r* = ", r, "<br>",
                 "<sup>", sig, "</sup> *p* ", p_text)
  
  p <- plot_predictions(lm_fit,
                        condition = c("nscan_ga_at_scan_weeks"),
                        newdata = datagrid(
                          model = lm_fit,
                          nscan_ga_at_scan_weeks = seq(min(df$nscan_ga_at_scan_weeks), 
                                                       max(df$nscan_ga_at_scan_weeks), 
                                                       length.out = 100),
                          fd_outliers = mean(df$fd_outliers),
                          gender = levels(df$gender)[1]
                        )) +
    geom_point(data = df, 
               aes(x = nscan_ga_at_scan_weeks, y = .data[[var]]),
               color = "#2980b9",alpha = 0.5, size = 1) +
    labs(x = "PMA at scan (weeks)", y = NULL) +  
    scale_x_continuous(breaks = seq(floor(min(df$nscan_ga_at_scan_weeks)), 
                                    ceiling(max(df$nscan_ga_at_scan_weeks)), 
                                    by = 1)) + 
    ggtext::geom_richtext(
      x = I(0.99),y = I(0.90),label = label,
      size = 4.5,fill = NA,hjust = 1,label.color = NA
    ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),  
      panel.background = element_rect(fill = NA, color = NA),
      plot.background = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      axis.text = element_text(size = 16, color = "black"),  
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 16, color = "black"),
      legend.position = "none",
      aspect.ratio = 0.75,
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "black", size = 0.5),    
      axis.ticks.length = unit(0.1, "cm")
    )
}
