library(dplyr)
library(ggplot2)
library(ppcor)
library(readxl)
library(stringr)
# 01 Load data
##  Load 4-year-old neurodevelopmental outcomes

### The 4-6 year follow-up data from the CHiP project 
### cannot be made publicly available due to ethical restrictions.
### Data will be shared upon reasonable request. 
outcome_yr4 <- read_excel("data/dHCP_4yr_data.xlsx", "dHCP_394sub") 
outcome_yr4 <- outcome_yr4 %>%
  mutate(
    ADHD_RS_total = Inattention_score + HI_score, 
    SDQ_total_difficulty = emotional_problems_raw + conduct_problems_raw + hyperactivity_raw + peer_problems_raw
  ) 

n_distinct(outcome_yr4$ID)

##  Load brain network features
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
    term_status = factor(
      ifelse(baby_ga_at_birth_weeks < 37, "Preterm", "Term"),
      levels = c("Term", "Preterm")
    ),
    term_status_numeric = ifelse(term_status == "Term", 0, 1),
    corr_age_at_assess = corr_age_at_assess_m + corr_age_at_assess_d / 30, # corrected age at 18-month assessments
    ID = str_extract(Subject_ID, "CC[0-9]{5}[A-Z]{2}")
  )

# 02  Parcorr(Dev outcomes vs network features|Gender + Age at assessment + IMD + PMA at scan + FD outliers) 
outcome_yr4 <- outcome_yr4 %>%
  left_join(data,by = c("ID"))
  
y_vars <- c(
  "Surgency", "Negative_Affect", "Effortful_Control",
  "ADHD_RS_total",
  "SCQ_Total_Score",
  "Emotion_Contagion", "Attention_to_Others_Feelings", "Prosocial_Actions",
  "SDQ_total_difficulty"
  # "Inhibit_Raw", "Shift_Raw", "WM_Raw","PO_Raw", "EC_Raw"
) 

y_labels <- c("Surgency" = "CBQ Surgency",
                "Negative_Affect" = "CBQ Negative Affectivity",
                "Effortful_Control" = "CBQ Effortful control",
                
                "Inhibit_Raw" = "Inhibition",
                "Shift_Raw" = "Shifting",
                "EC_Raw" = "Emotional control",
                "WM_Raw" = "Working memory",
                "PO_Raw" = "Plan organization",
                
                "Inattention_score" = "Inattention", 
                "HI_score" = "Hyperactivity-impulsivity",
                "ADHD_RS_total" = "ADHD total score",
                
                "SCQ_Total_Score" = "SCQ total score",
                
                "Emotion_Contagion" = "EmQue Emotional Contagion", 
                "Attention_to_Others_Feelings" = "EmQue Attention to Others’ Feelings",
                "Prosocial_Actions" = "EmQue Prosocial Actions",
                
                "emotional_problems_raw" = "Emotional symptoms",
                "conduct_problems_raw" = "Conduct problems",
                "hyperactivity_raw" = "Hyperactivity-Inattention",
                "peer_problems_raw" = "Peer problems",
                "prosocial_raw" = "Prosocial behaviour",
                "SDQ_total_difficulty" = "SDQ total difficulties score"
                )

x_vars <- c(
  "GEnorm", 
  "LEnorm"
)

x_labels <- c("LEnorm" = "Normalised LE",
              "GEnorm" = "Normalised GE")

res <- data.frame(
  Outcome = character(),
  Brain = character(),
  r = numeric(),
  p = numeric(),
  t = numeric(),
  stringsAsFactors = FALSE
)

for (dv in y_vars) {
  for (brain in x_vars) {
    pcor_res <- ppcor::pcor(outcome_yr4[, c(dv, brain,
                              "AgeYrs", # age at 4-6-year assessments
                              "baby_gender",
                              "IMD_anon",
                              "nscan_ga_at_scan_weeks",
                              'fd_outliers')],
                     method = "spearman")
    
    r <- pcor_res$estimate[dv, brain]
    p <- pcor_res$p.value[dv, brain]
    t <- pcor_res$statistic[dv, brain]
    
    res <- rbind(
      res,
      data.frame(Outcome = dv, 
                 Brain = brain, 
                 r = r,
                 p = p,
                 t = t)
    )
  }
}
#
res$p_unc <- ifelse(res$p < 0.001, "<0.001", sprintf("%.3f", res$p))
res$r_label <- sprintf("%.3f", res$r)
res$p_FDR <- p.adjust(res$p, method = "BH")
res$Significance <- cut(res$p_FDR,
                        breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                        labels = c("***", "**", "*", ""),
                        right = FALSE)

# heat map
heatmap <- ggplot(res, aes(y = Outcome, x = Brain, fill = r)) + 
  geom_tile(color = "white") +  
  scale_fill_gradient2(low = "#2980b9", mid = "white", high = "#FF4136", midpoint = 0,
                       breaks=c(-0.35,0,0.35),
                       labels = c("-0.35", "0", "0.35"),
                       limits = c(-0.35, 0.35),
                       guide = guide_colorbar(direction = "vertical", 
                                              title.position = "left", 
                                              title.vjust = 1)) +  
  geom_text(aes(label = paste0(r_label, Significance)),
            color = "black",
            size = 3) +
  labs(title = NULL, 
       fill = expression(paste("Partial Spearman's correlation ", rho))) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, color = "black", size = 11, face = "bold"),  
    axis.text.y = element_text(color = "black", size = 11, face = "bold"), 
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    legend.position = "right",  
    legend.title = element_text(angle = 90, hjust = 0.5),  
    plot.title = element_text(hjust = 0.5),
    axis.ticks.y = element_blank(),    
    axis.line.y = element_blank(),        
    panel.border = element_blank()        
  ) +
  scale_y_discrete(labels = y_labels) + 
  scale_x_discrete(labels = x_labels)

# ggsave("heatmap_Outcome_4yr_NetMea.png",
#        plot = heatmap,
#        width = 5,
#        height = 6,
#        dpi = 600)

plot_A <- ggplot(outcome_yr4, aes(x = GEnorm, y = SDQ_total_difficulty)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(x = "Normalised global efficiency", y = "SDQ total difficulties score") +
  theme_minimal() +
  theme(
        panel.border = element_rect(fill = NA, color = "black"),
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black", size = 0.5), 
        axis.ticks.length = unit(0.1, "cm"))

plot_B <- ggplot(outcome_yr4, aes(x = LEnorm, y = SDQ_total_difficulty)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(x = "Normalised local efficiency", y = "SDQ total difficulties score") +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "black"),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.5), 
    axis.ticks.length = unit(0.1, "cm")
  )

# ggsave(file.path("SDQ_total_GE.png"),
#        plot = plot_A,
#        width = 6,
#        height = 5,
#        dpi = 600)
# 
# ggsave(file.path("SDQ_total_LE.png"),
#        plot = plot_B,
#        width = 6,
#        height = 5,
#        dpi = 600)
