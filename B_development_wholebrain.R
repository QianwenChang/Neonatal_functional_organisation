# 01 load packages
library(dplyr)
library(ggplot2)
library(ggtext)
library(marginaleffects)
library(patchwork)
source("plot_lm_PMA.R")
source("boxplot_preterm.R")
# 02 load data
data <- read.csv("data/network_features_wholebrain.csv") 
SubInfo <- read.csv("data/dHCP_R3_info_394sub.csv")
  
data <- data %>%
  left_join(SubInfo, by = "Subject_ID") %>%
  mutate(
    gender = factor(
      baby_gender - 1,
      levels = c(0, 1),
      labels = c("Male", "Female")
    ),
    corr_age_at_assess = corr_age_at_assess_m +
      corr_age_at_assess_d / 30,
    term_status = factor(
      ifelse(baby_ga_at_birth_weeks < 37, "Preterm", "Term"),
      levels = c("Term", "Preterm")
    ),
    GEnorm_scaled = GEnorm * 1e5,
    LEnorm_scaled = LEnorm * 1e5
  )
  
# 03 Developmental effects
measures <- c("meanFCS","GEnorm_scaled","LEnorm_scaled")
y_labels <- c("GEnorm_scaled"  = "Normalised global efficiency", 
              "LEnorm_scaled"  = "Normalised local efficiency", 
              "meanFCS" = "Mean FCS")

data_term <- data %>% filter(term_status =="Term")

for (var in measures) {
  y_title <- y_labels[var]
  
  y_lim <- range(data[[var]], na.rm = TRUE)
  
  p_PMA <- plot_lm_PMA(data_term, var, y_title) +
    scale_y_continuous(limits = y_lim, breaks = pretty(data[[var]], n = 5))
  
  p_preterm <- boxplot_preterm(data, var, y_title) +
    scale_y_continuous(limits = y_lim, breaks = pretty(data[[var]], n = 5))
  
  combined_plot <- p_PMA + p_preterm + 
    plot_layout(ncol = 2, widths = c(3, 2)) &
    theme(plot.background = element_rect(fill = NA, color = NA))
  
  # ggsave(
  #   filename = paste0(var,".pdf"),
  #   plot = combined_plot,
  #   width = 8, height = 4,
  #   device = "pdf"
  # )
}
