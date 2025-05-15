###stress experiment reproductive products - FIGURE
library(dplyr)
library(ggplot2)

#Figure 3:
#change path to adjust file location if needed
res <- read.csv("~/Library/CloudStorage/OneDrive-mail.tau.ac.il/Papers/Unger et al. 2025/data/larvae_figure.csv")
data_long <- res %>%
  pivot_longer(cols = c(larvae, settled), names_to = "Category", values_to = "Count")

# stacked bar graph
data_long <- res %>%
  pivot_longer(cols = c(larvae, settled), names_to = "Category", values_to = "Count")

res <- na.omit(res)

# Define custom colors for the populations
custom_colors_1 <- c("Red" = "lightcoral", "Med" = "lightblue", "Singapore" = "#ffcc33")

library(scales)

# Calculate success percentage
res <- res %>%
  mutate(success_percentage = (larvae + settled) / total * 100)

# Plotting code
ggplot(res, aes(x = origin, y = success_percentage, fill = origin)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_grid(salinity ~ temperature, scales = "free") +
  scale_fill_manual(values = custom_colors_1) +
  theme_minimal(base_family = "Times New Roman") +
  labs(x = "Population", y = "Success (%)", title = "Success Percentage by Treatment") +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid = element_blank(),  # Remove grid lines from the background
        panel.border = element_rect(color = "black", fill = NA, size = 0.2),
        legend.position = "bottom",
        legend.justification = "right") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  guides(fill = guide_legend(ncol = 1))

