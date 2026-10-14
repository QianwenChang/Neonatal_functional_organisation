source("network_barplot.R")
source("plot_cortical_map.R")
source("plot_subcortical_map.R")
source("run_lm_PMA.R")
source("run_lm_preterm.R")
library(parameters)
library(R.matlab)
library(readxl)
library(dplyr)
library(ppcor)
library(tidyr)
library(scales)
library(ggplot2)

library(ggseg)
library(ggsegExtra)
library(ggsegSchaefer)
library(ggsegYeo2011)
# 01 load data
rootdir <- "data"
## 01-1 atlas info
atlas_info <- read.csv("data/atlas_info.csv") %>%
  dplyr::mutate(
    network = dplyr::recode(network,
                            "SomMot" = "SMN",
                            "Visual" = "VN",
                            "SalVentAttn" = "SN_VAN",
                            "DorsAttn" = "DAN",
                            "Control" = "FPN",
                            "Default" = "DMN",
                            "Limbic" = "Limbic",
                            "Subcortical" = "SCN",
                            "Cerebellum" = "CBN"
    ))

## 01-2 demographic info
SubInfo <- read.csv("data/dHCP_R3_info_394sub.csv")
SubInfo <- SubInfo %>%
  dplyr::mutate(
    gender = factor(
      baby_gender - 1,
      levels = c(0, 1),
      labels = c("Male", "Female")
    ),
    term_status = factor(
      ifelse(baby_ga_at_birth_weeks < 37, "Preterm", "Term"),
      levels = c("Term", "Preterm")
    ),
    term_status_numeric = ifelse(term_status == "Term", 0, 1),
  ) 

## 01-3 network properties
measures <- c("GE_norm",
              "LE_norm",
              "meanFCS")

measure_list <- setNames(
  lapply(measures, function(m) {
    read.csv(file.path(rootdir, paste0("ind_", m, ".csv")))
  }),
  measures
)

# 02 Developmental changes at the parcel level
# Linear regression: y ~ PMA at scan + Sex + head motion
parcel_res_PMA <- lapply(names(measure_list), function(m) {
  
  df <- SubInfo %>%
    full_join(measure_list[[m]], by = c("Subject_ID" = "subjID")) %>%
    filter(term_status == "Term")
  
  node_list <- paste0("node", 1:433)
  node_list <- node_list[colSums(df[, node_list] != 0, na.rm = TRUE) > 0]
  
  lapply(node_list, function(node) {
    run_lm_PMA(df = df, var = node)
  }) %>% 
    bind_rows() %>%
    mutate(measure = m,
           q = p.adjust(p, method = "BH")) %>%
    dplyr::rename("node" = "var")
}) %>% setNames(names(measure_list))

parcel_res_preterm <- lapply(names(measure_list), function(m) {
  
  df <- SubInfo %>%
    full_join(measure_list[[m]], by = c("Subject_ID" = "subjID")) 
  
  node_list <- paste0("node", 1:433)
  node_list <- node_list[colSums(df[, node_list] != 0, na.rm = TRUE) > 0]
  
  # developmental effects for each node
  lapply(node_list, function(node) {
    run_lm_preterm(df = df, var = node)
  }) %>% 
    bind_rows() %>%
    mutate(measure = m,
           q = p.adjust(p, method = "BH")) %>%
    dplyr::rename("node" = "var")
}) %>% setNames(names(measure_list))


network_colors <- c(
  SMN = "#4682B4",
  VN = "#781286",
  SN_VAN = "#C43AFA",
  DAN = "#00760E",
  FPN = "#E69422",
  DMN = "#CD3E4E",
  Limbic = "#DCF8A4",
  SCN = "#800000",
  CBN = "#FFD700"
)

network_mappings <- c(
  "SMN" = "SMN",
  "VN" = "VN",
  "SN_VAN" = "SN/VAN",
  "DAN" = "DAN",
  "FPN" = "FPN",
  "DMN" = "DMN",
  "Limbic" = "Limbic",
  "SCN" = "SCN",
  "CBN" = "CBN"
)

# 03 Visualize
process_matrix <- function(result_df,var) {
  
  df <- atlas_info %>%
    mutate(node = paste0("node", dplyr::row_number())) %>%
    left_join(
      result_df %>% dplyr::select(node, var, q),
      by = "node"
    )
  
  left_lentiform  <- df %>% filter(ROI == "Left-Lentiform")
  right_lentiform <- df %>% filter(ROI == "Right-Lentiform")
  
  df <- df %>%
    filter(!ROI %in% c("Left-Lentiform", "Right-Lentiform",
                       "Left-Subthalamic", "Right-Subthalamic"))
  
  new_rows <- bind_rows(
    left_lentiform  %>% mutate(ROI = "Left-Putamen"),
    left_lentiform  %>% mutate(ROI = "Left-Pallidum"),
    right_lentiform %>% mutate(ROI = "Right-Putamen"),
    right_lentiform %>% mutate(ROI = "Right-Pallidum")
  )
  
  df <- rbind(df, new_rows) %>% 
    rename(region = ROI)
  
  return(df)
}

tmp_PMA <- setNames(
  lapply(names(parcel_res_PMA), function(m) {
    process_matrix(parcel_res_PMA[[m]],"r")
  }),
  names(parcel_res_PMA)
)

tmp_preterm <- setNames(
  lapply(names(parcel_res_preterm), function(m) {
    process_matrix(parcel_res_preterm[[m]],"beta")
  }),
  names(parcel_res_preterm)
)

titles <- c(
  "GE_norm" = "Normalised global efficiency",
  "LE_norm" = "Normalised local efficiency",
  "meanFCS" = "Mean FCS"
)

outputdir <- "data/01_development"
for (m in names(tmp_PMA)) {
  df_PMA <- tmp_PMA[[m]]
  
  p1 <- plot_cortical_map(df_PMA, "r", title = titles[[m]], onlysig = TRUE)
  ggsave(file.path(outputdir, paste0(m, "_PMA_thres.pdf")), p1, width = 4, height = 2)

  p2 <- plot_subcortical_map(df_PMA, "r", title = titles[[m]], onlysig = TRUE)
  ggsave(file.path(outputdir, paste0("sub_", m, "_PMA_thres.pdf")), p2, width = 4, height = 2)
  
  df_preterm <- tmp_preterm[[m]]
  p1 <- plot_cortical_map(df_preterm, "beta", title = titles[[m]], onlysig = TRUE)
  ggsave(file.path(outputdir, paste0(m, "_preterm_beta_thres.pdf")), p1, width = 4, height = 2)

  p2 <- plot_subcortical_map(df_preterm, "beta", title = titles[[m]], onlysig = TRUE)
  ggsave(file.path(outputdir, paste0("sub_", m, "_preterm_beta_thres.pdf")), p2, width = 4, height = 2)
}

# 04 Developmental changes at the network level
network_labels <- atlas_info$network
networks <- unique(network_labels)
network_idxs <- split(seq_along(network_labels), network_labels)

compute_network_means <- function(measure_df) {
  mat <- as.matrix(dplyr::select(measure_df, -subjID))
  
  lapply(network_idxs, function(idx) {
    rowMeans(mat[, idx, drop = FALSE], na.rm = TRUE)
  }) %>%
    as.data.frame(check.names = FALSE) %>%
    dplyr::mutate(subjID = measure_df$subjID, .before = 1)

}

net_list <- lapply(measure_list, compute_network_means)

network_res_PMA <- lapply(names(net_list), function(m) {
  
  df <- net_list[[m]] %>%
    dplyr::left_join(SubInfo, by = c("subjID" = "Subject_ID")) %>%
    filter(term_status == "Term")
  
  lapply(networks, function(net) {
    run_lm_PMA(df = df, var = net)
  }) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(measure = m,
                  q = p.adjust(p, method = "BH")) %>%
    rename("network" = "var")
}) %>% setNames(names(net_list))

network_res_preterm <- lapply(names(net_list), function(m) {
  
  df <- net_list[[m]] %>%
    dplyr::left_join(SubInfo, by = c("subjID" = "Subject_ID")) 
  
  lapply(networks, function(net) {
    run_lm_preterm(df = df, var = net)
  }) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(measure = m,
                  q = p.adjust(p, method = "BH")) %>%
    rename("network" = "var")
}) %>% setNames(names(net_list))

# 05 Visualisation
for (m in names(network_res_PMA)) {
  # PMA effect
  signif_df <- network_res_PMA[[m]] %>%
    mutate(
      network_ord = factor(network, levels = network[order(r)]),
      signif = cut(q, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                   labels = c("***", "**", "*", ""))
    )
  
  p_PMA <- network_barplot(df = network_res_PMA[[m]],
                           y_var = "r",
                           title = titles[[m]],
                           y_title = "Effect size (PMA at scan)"
                           )
  p_PMA <- p_PMA +
    # scale_y_continuous(
    #   limits = c(-0.12, 0.35),
    #   breaks = seq(-0.15, 0.15, by = 0.05),
    #   labels = scales::label_number(accuracy = 0.01),
    #   expand = c(0, 0)
    # ) +
    geom_text(data = signif_df,
              aes(x = network_ord, y = r, label = signif,
                  vjust = ifelse(r >= 0, -0.3, 1.3)),
              size = 5, inherit.aes = FALSE) +
    theme(
      panel.grid.minor.y = element_blank()
    )
  
  # ggsave(
  #   filename = file.path(outputdir, paste0("barplot_", m, "_PMA.pdf")),
  #   plot = p_PMA,
  #   width = 6, height = 5,
  #   device = "pdf"
  # )
  
  # preterm birth effect
  signif_df <- network_res_preterm[[m]] %>%
    mutate(
      network_ord = factor(network, levels = network[order(beta)]),
      signif = cut(q, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                   labels = c("***", "**", "*", ""))
    )
  
  p_preterm <- network_barplot(df = network_res_preterm[[m]],
                               y_var = "beta",
                               title = titles[[m]],
                               y_title = "Effect size (Preterm birth)"
  )
  p_preterm <- p_preterm +
    geom_text(data = signif_df,
              aes(x = network_ord, y = beta, label = signif,
                  vjust = ifelse(beta >= 0, -0.3, 1.3)),
              size = 5, inherit.aes = FALSE) +
    theme(panel.grid.minor.y = element_blank())
  
  # ggsave(
  #   filename = file.path(outputdir, paste0("barplot_", m, "_preterm_beta.pdf")),
  #   plot = p_preterm,
  #   width = 6, height = 5,
  #   device = "pdf"
  # )
}
