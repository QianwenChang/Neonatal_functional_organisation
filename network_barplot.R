network_colors <- c(
  "SMN"    = "#4682B4",
  "VN"     = "#781286",
  "SN_VAN" = "#C43AFA",
  "DAN"    = "#00760E",
  "FPN"    = "#E69422",
  "DMN"    = "#CD3E4E",
  "Limbic" = "#DCF8A4",
  "SCN"    = "#800000",
  "CBN"    = "#FFD700"
)

network_barplot <- function(df, y_var, title = NULL, y_title) {
  
  df$network_ord <- factor(
    df$network,
    levels = df$network[order(df[[y_var]])]
  )
  
  p <- ggplot(df, aes(x = network_ord, y = .data[[y_var]], fill = network)) +
    geom_col(width = 0.7, color = "black") +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray30", size = 0.8) + 
    scale_fill_manual(values = network_colors, guide = "none") +
    labs(
      title = title,
      y = y_title,
      x = NULL
    ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA,linewidth = 1),
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", size = 0.3),
      panel.grid.minor.y = element_line(color = "grey90", size = 0.3),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(0.1, "cm")
    )
  
  return(p)
}