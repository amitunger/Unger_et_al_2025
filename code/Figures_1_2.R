###stress experiment juveniles - FIGURES
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)

#Figure 1:
#change path to adjust file location if needed
surv <- read.csv("~/Library/CloudStorage/OneDrive-mail.tau.ac.il/Papers/Unger et al. 2025/data/survival_ percentage.csv")
surv %>%
  ggplot(aes(x = Day, y = Survival, group = Origin, linetype = Origin)) +
  geom_line(size = 1) + 
  scale_linetype_manual(values = c("Red" = "solid", "Med" = "dashed", "Singapore" = "dotdash")) +
  facet_grid(Salinity ~ Temperature, scales = "free") +
  theme_minimal() +
  labs(x = "Time [Days]", y = "Survival [%]", title = "Survival Rate by Treatment") +
  scale_x_continuous(breaks = seq(0, 30, by = 5), labels = seq(0, 30, by = 5)) +
  theme(text=element_text(family="Times New Roman"),
        strip.text.y = element_text(angle = 0),
        panel.grid = element_blank(), # Remove grid lines from the background
        panel.border = element_rect(color = "black", fill = NA, size = 0.2),
        legend.position = "bottom",
        legend.justification = "right") +
  guides(color = guide_legend(ncol = 1)) 


#Figure 2:
#change path to adjust file location if needed
res <- read.csv("~/Library/CloudStorage/OneDrive-mail.tau.ac.il/Papers/Unger et al. 2025/data/blood_flow.csv")
# Convert temperature and salinity to factors for better control over shapes and colors
res$temperature <- factor(res$temperature)
res$salinity <- factor(res$salinity)

#scatterplot:
# Define custom labels for the facet_wrap titles
custom_labels <- c("med" = "Mediterranean Sea", "red" = "Red Sea", "Singapore" = "Singapore")

ggplot(res, aes(x = factor(day), y = time, shape = salinity, color = temperature)) +
  geom_point(size = 2) +  # Increase point size for better visibility
  facet_wrap(~origin, scales = "free") +
  facet_wrap(~origin, scales = "free", labeller = labeller(origin = custom_labels)) +
  labs(title = "Change in Blood Flow Direction Over Time",
       x = "Day of Experiment",
       y = "Time to Change Direction (Seconds)",
       shape = "Salinity",
       color = "Temperature") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, by = 20)) +
  scale_color_manual(values = c("#66ccff", "#66cc66", "#ff6666"), 
                     labels = c("16° C", "25° C", "31° C")) +  
  scale_shape_manual(values = c(0, 1, 2, 3, 4), 
                     labels = c("28 PSU", "31 PSU", "35 PSU", "40 PSU", "43 PSU"))  

#box plot SINGAPORE:
res$treatment <- paste(res$temperature, res$salinity, sep = "-")

# Filter the data to include only singapore
res_singapore <- res %>% 
  filter(origin %in% c("Singapore"))

temperature_colors <- c("16" = "#66ccff", "25" = "#66cc66", "31" = "#ff6666")
salinity_lines <- c("28" = "blank", "31" = "dotted", "35" = "dashed", "40" = "solid")

ggplot(res_singapore, aes(x = as.factor(day), y = time, fill = factor(temperature), linetype = factor(salinity))) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7, color = "black") +
  labs(x = "Days in experiment", y = "Time of direction change (seconds)", title = "Blood flow direction change time by treatment") +
  scale_fill_manual(values = temperature_colors, name = "Temperature (°C)") +
  scale_linetype_manual(values = salinity_lines, name = "Salinity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  facet_wrap(~ origin, scales = "free_x", ncol = 3) +
  scale_x_discrete(breaks = c("0", "7", "14", "21", "28")) +
  scale_y_continuous(breaks = seq(0, 280, by = 20))
