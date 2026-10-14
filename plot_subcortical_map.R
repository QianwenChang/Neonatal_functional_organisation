plot_subcortical_map <- function(df, var, title, vmin = -0.3, vmax = 0.3,
                                 onlysig = FALSE) {
  subcortical_atlas <- aseg
  
  if (onlysig) {
    df[[var]][df$q >= 0.05] <- NA
  }
  
  df %>%
    dplyr::filter(structure == "subcortical") %>% 
    dplyr::rename(label = region) %>%
    brain_join(subcortical_atlas, by = "label") %>%
    ggplot() +
    geom_sf(aes(fill = .data[[var]]), colour="black", size=.7) +
    scale_fill_distiller(
      palette = "RdBu", 
      direction = -1,
      limits = c(vmin, vmax),
      breaks = c(vmin, 0, vmax),
      labels = c(vmin, 0, vmax),
      na.value = "grey75",
      trans = "identity",
      oob = scales::squish
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "left"
    )
}