#### Unger et al. 2025, Ecology Letters
## Figures (1-3): 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)

#Fig. 1:
#change path to adjust file location if needed
surv <- read.csv("D:/Amit/M. Sc Zoology/2- stress experiments on cultured babies/survival_csv.csv")
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

#######################################
#Fig. 2:
#change path to adjust file location if needed
res <- read.csv("~/Documents/M. Sc Zoology/2- stress experiments on cultured babies/results_blood_total_csv.csv")
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

# BOX PLOT SINGAPORE:
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

#######################################
#Fig. 3:
#change path to adjust file location if needed
res <- read.csv("~/Documents/M. Sc Zoology/3- stress experiment on reproductive and early life stages/larvae_results_csv.csv")
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

#######################################
## Stress experiment on juvenile P. nigra
#Kaplan-Meier analysis
library(survival)
library(readxl)
#Mediterranean population
#change path to adjust file location if needed
results_med<- read_excel("D:/Amit/M. Sc Zoology/2- stress experiments on cultured babies/results_med.xlsx")
attach(results_med)

# Define variables 
time <- time
event <- event
X <- cbind(salinity, temperature)
group <- temperature

# Descriptive statistics
summary(time)
summary(event)
summary(X)
summary(group)

#KM by groups of salinity
km.model <- survfit(Surv(time, event) ~ salinity, 
                    type = "kaplan-meier")
summary(km.model)

#plotting the model
plot(km.model, conf.int=F, xlab="Time [days]", ylab="Survival [%]",
     main="KM-Model by salinity in Med. Sea", col=c("#CC9933", "#CC3399", "#33CC99"), las=1)
legend(21, 1, legend=c("35[PSU]", "40[PSU]", "43[PSU]"), lty=1,
       col=c("#CC9933", "#CC3399", "#33CC99"), bty="", cex=0.6)

#LOG-RANK test
#H0: survival is the same
#H1: survival not the same
survdiff(Surv(time, event)~salinity)

#KM by groups of temperature
km.model <- survfit(Surv(time, event) ~ temperature, 
                    type = "kaplan-meier")
summary(km.model)

#plotting the model
plot(km.model, conf.int=F, xlab="Time [days]", ylab="Survival [%]",
     main="KM-Model by temperature in Med. Sea", col=c("#3399CC", "#FF0066", "#666000"), las=1)
legend(22, 1, legend=c("16[°C]", "25[°C]", "31[°C]"), lty=1,
       col=c("#3399CC", "#FF0066", "#666000"), bty="", cex=0.6)

#LOG-RANK test
#H0: survival is the same
#H1: survival not the same
survdiff(Surv(time, event)~temperature)

#Red Sea population
#change path to adjust file location if needed
results_red <- read_excel("D:/Amit/M. Sc Zoology/2- stress experiments on cultured babies/results_red.xlsx")
attach(results_red)

# Define variables 
time <- time
event <- event
X <- cbind(salinity, temperature)

# Descriptive statistics
summary(time)
summary(event)
summary(X)

#KM by groups of salinity
km.model <- survfit(Surv(time, event) ~ salinity, 
                    type = "kaplan-meier")
summary(km.model)

#plotting the model
plot(km.model, conf.int=F, xlab="Time [days]", ylab="Survival [%]",
     main="KM-Model by salinity in Red. Sea", col=c("#CC9933", "#CC3399", "#33CC99"), las=1)
legend(21, 1, legend=c("35[PSU]", "40[PSU]", "43[PSU]"), lty=1,
       col=c("#CC9933", "#CC3399", "#33CC99"), bty="", cex=0.6)

#LOG-RANK test
#H0: survival is the same
#H1: survival not the same
survdiff(Surv(time, event)~salinity)

#KM by groups of temperature
km.model <- survfit(Surv(time, event) ~ temperature, 
                    type = "kaplan-meier")
summary(km.model)

#plotting the model
plot(km.model, conf.int=F, xlab="Time [days]", ylab="Survival [%]",
     main="KM-Model by temperature in Red. Sea", col=c("#3399CC", "#FF0066", "#666000"), las=1)
legend(22, 1, legend=c("16[°C]", "25[°C]", "31[°C]"), lty=1,
       col=c("#3399CC", "#FF0066", "#666000"), bty="", cex=0.6)

#LOG-RANK test
#H0: survival is the same
#H1: survival not the same
survdiff(Surv(time, event)~temperature)

#Singapore population
#change path to adjust file location if needed
results_sin <- read_excel("D:/Amit/M. Sc Zoology/2- stress experiments on cultured babies/Kaplan-Meir/results_sin.xlsx")
attach(results_sin)

# Define variables 
time <- time
event <- event
X <- cbind(salinity, temperature)

# Descriptive statistics
summary(time)
summary(event)
summary(X)

#KM by groups of salinity
km.model <- survfit(Surv(time, event) ~ salinity, 
                    type = "kaplan-meier")
summary(km.model)

#plotting the model
plot(km.model, conf.int=F, xlab="Time [days]", ylab="Survival [%]",
     main="KM-Model by salinity in Singapore", col=c("#CC9933", "#CC3399", "#33CC99"), las=1)
legend(22, 0.2, legend=c("35[PSU]", "40[PSU]", "43[PSU]"), lty=1,
       col=c("#CC9933", "#CC3399", "#33CC99"), bty="", cex=0.6)

#LOG-RANK test
#H0: survival is the same
#H1: survival not the same
survdiff(Surv(time, event)~salinity)

#KM by groups of temperature
km.model <- survfit(Surv(time, event) ~ temperature, 
                    type = "kaplan-meier")
summary(km.model)

#plotting the model
plot(km.model, conf.int=F, xlab="Time [days]", ylab="Survival [%]",
     main="KM-Model by temperature in Singapore", col=c("#3399CC", "#FF0066", "#666000"), las=1)
legend(22, 0.18, legend=c("16[°C]", "25[°C]", "31[°C]"), lty=1,
       col=c("#3399CC", "#FF0066", "#666000"), bty="", cex=0.6)

#LOG-RANK test
#H0: survival is the same
#H1: survival not the same
survdiff(Surv(time, event)~temperature)

#statistics:
#change path to adjust file location if needed
results <- read.csv("~/Documents/M. Sc Zoology/2- stress experiments on cultured babies/results_all_csv.csv")
View(results)

# changing salinity and temperature to be FACTORS and not numeric
temperature <- as.factor(results$temperature)
salinity <- as.factor(results$salinity)
origin <- as.factor(results$origin)

class(temperature)
class(salinity)
class(origin)

#setting straight the levels:
results$temperature <- factor(results$temperature, levels = c("16", "25", "31"))
results$salinity <- factor(results$salinity, levels = c("28", "31", "35", "40", "43"))

#######################################
# GLM with Negative Binomial regression model:
library(MASS)

nb_model <- glm.nb(day ~ temperature + salinity + origin, data = results, na.action = na.fail)
summary(nb_model)

# Validate the model: check residuals
plot(residuals(nb_model, type = "deviance"))

# Calculate dispersion parameter
dispersion_param <- sum(residuals(nb_model, type = "pearson")^2) / df.residual(nb_model)
dispersion_param

# Residual Normality Analysis
qqnorm(residuals(nb_model))
qqline(residuals(nb_model))

# Homoscedasticity
plot(nb_model, which = 1)  # 1 corresponds to residuals vs. fitted values

#checking assumptions:
# Deviance residuals vs. fitted values
plot(fitted(nb_model), residuals(nb_model, type = "deviance"),
     xlab = "Fitted values", ylab = "Deviance residuals",
     main = "Deviance Residuals vs. Fitted")

# QQ plot of deviance residuals
qqnorm(residuals(nb_model, type = "deviance"))
qqline(residuals(nb_model, type = "deviance"))

# Cook's distance plot
plot(cooks.distance(nb_model),
     ylab = "Cook's distance", main = "Cook's Distance Plot")

# Scale-location plot (sqrt(|residuals|) vs. fitted values)
plot(fitted(nb_model), sqrt(abs(residuals(nb_model))),
     xlab = "Fitted values", ylab = "sqrt(|Residuals|)",
     main = "Scale-Location Plot")

#Check for Multicollinearity
vif_values <- vif(nb_model)
print(vif_values)

# r^2 of the glm model:
library(performance)

r2_value <- r2(nb_model)
print(r2_value)

#######################################
#blood flow statistics
#change path to adjust file location if needed
res <- read.csv("~/Documents/M. Sc Zoology/2- stress experiments on cultured babies/results_blood_total_csv.csv")

# Poisson GLM for all populations
library(lme4)
library(dplyr)

#making temp25 and salinity40 the control for comparison:
res$temperature <- relevel(res$temperature, ref = "25")
res$salinity <- relevel(res$salinity, ref = "40")

glm_new_all <- glm(time ~ temperature + salinity + day + origin,
                   family = poisson(link = "log"),
                   data = res)
summary(glm_new_all)

#glm assumptions:
# Residual Normality Analysis
qqnorm(residuals(glm_new_all))
qqline(residuals(glm_new_all))

# Homoscedasticity
plot(glm_new_all, which = 1)  # 1 corresponds to residuals vs. fitted values

# Residuals vs. Fitted values plot
plot(glm_new_all, which = 3)

# Cook's distance plot
plot(glm_new_all, which = 4)

library(performance)

r2_value <- r2(glm_new_all)
print(r2_value)

#######################################
# SINGAPORE Linear Mixed-Effects Model (LMM)  
library(lme4)
library(emmeans)
library(lmerTest) 

# Create a new identifier column
singapore_data$identifier <- paste("plate", singapore_data$plate, "number", singapore_data$subject_number, sep = "_")

#making temp25 and salinity31 the control for comparison:
singapore_data$temperature <- relevel(singapore_data$temperature, ref = "25")

singapore_data$salinity <- relevel(singapore_data$salinity, ref = "31")

# Fit the mixed-effects model
lmer_model <- lmer(time ~ temperature * salinity * day + (1|identifier), data = singapore_data)
summary(lmer_model)

# Check residuals
par(mfrow=c(1,1))
plot(residuals(lmer_model))

#mixed effects assumptions:
#Residuals vs Fitted plot (Check for linearity)
plot(fitted(lmer_model), residuals(lmer_model))
abline(h = 0, col = "red", lty = 2)
title("Residuals vs Fitted")

#Q-Q plot of residuals (Check for normality)
qqnorm(residuals(lmer_model))
qqline(residuals(lmer_model))

#Scale-Location plot (Check for homoscedasticity)
plot(sqrt(abs(residuals(lmer_model))) ~ fitted(lmer_model))
abline(h = 0, col = "red", lty = 2)
title("Scale-Location Plot")

#Cook's distance plot (Check for influential points)
plot(cooks.distance(lmer_model), pch = 20, main = "Cook's distance plot")

#Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(lmer_model))

# Variance inflation factors (VIF) for collinearity
vif(lmer_model)

r2_value <- r2(lmer_model)
print(r2_value)

#######################################
## Stress experiment on P. nigra reproduction products
#change path to adjust file location if needed
#load file with binomial data 
larvae_binomial <- read.csv("~/Documents/M. Sc Zoology/3- stress experiment on reproductive and early life stages/larvae_binomial.csv")

library(dplyr) 
library(car) # for normality tests
library(lme4)

# changing salinity, temperature and origin to be FACTORS and not numeric/character
Temperature <- as.factor(larvae_binomial$Temperature)
Salinity <- as.factor(larvae_binomial$Salinity)
Origin <- as.factor(larvae_binomial$Origin)

class(Temperature)
class(Salinity)
class(Origin)

# GLM per population: 
library(dplyr, ggplot2, stats)

### singapore ###
singapore_larvae <- subset(larvae_binomial, Origin == "Singapore")

# making temp. and salinity factors here as well:
singapore_larvae$Temperature <- as.factor(singapore_larvae$Temperature)
singapore_larvae$Salinity <- as.factor(singapore_larvae$Salinity)

# making temp25 and salinity31 the control for comparison:
singapore_larvae$Temperature <- relevel(singapore_larvae$Temperature, ref = "25")
singapore_larvae$Salinity <- relevel(singapore_larvae$Salinity, ref = "31")

#glm model
glm_larvae_singapore <- glm(Success ~ Temperature * Salinity, data = singapore_larvae, family = binomial)
summary(glm_larvae_singapore)

### red sea ###
red_larvae <- subset(larvae_binomial, Origin == "Red")
red_larvae$Temperature <- as.factor(red_larvae$Temperature)
red_larvae$Salinity <- as.factor(red_larvae$Salinity)

# making temp25 and salinity40 the control for comparison:
red_larvae$Temperature <- relevel(red_larvae$Temperature, ref = "25")
red_larvae$Salinity <- relevel(red_larvae$Salinity, ref = "40")

glm_larvae_red <- glm(Success ~ Temperature * Salinity, data = red_larvae, family = binomial)
summary(glm_larvae_red)

### med sea ###
med_larvae <- subset(larvae_binomial, Origin == "Med")
med_larvae$Temperature <- as.factor(med_larvae$Temperature)
med_larvae$Salinity <- as.factor(med_larvae$Salinity)

# making temp25 and salinity40 the control for comparison:
med_larvae$Temperature <- relevel(med_larvae$Temperature, ref = "25")
med_larvae$Salinity <- relevel(med_larvae$Salinity, ref = "40")

glm_larvae_med <- glm(Success ~ Temperature * Salinity, data = med_larvae, family = binomial)
summary(glm_larvae_med)

##assumptions check per population: 
library(ResourceSelection)

# Residual Normality Analysis
qqnorm(residuals(glm_larvae_singapore))
qqline(residuals(glm_larvae_singapore))
# Homoscedasticity
plot(glm_larvae_singapore, which = 1)  # 1 corresponds to residuals vs. fitted values
# Residuals vs. Fitted values plot
plot(glm_larvae_singapore, which = 3)
# Cook's distance plot
plot(glm_larvae_singapore, which = 4)
# Multicollinearity
vif(glm_larvae_singapore)
# Plot Leverage
plot(glm_larvae_singapore, which = 5)
# Extract leverage values
leverage <- hatvalues(glm_larvae_singapore)
high_leverage <- which(leverage > (2 * mean(leverage)))
print("Observations with high leverage:")
print(high_leverage)
# Perform Hosmer-Lemeshow test
hoslem_test <- hoslem.test(glm_larvae_singapore$y, fitted(glm_larvae_singapore))
print("Hosmer-Lemeshow test results:")
print(hoslem_test)

# Residual Normality Analysis
qqnorm(residuals(glm_larvae_red))
qqline(residuals(glm_larvae_red))
# Homoscedasticity
plot(glm_larvae_red, which = 1)  # 1 corresponds to residuals vs. fitted values
# Residuals vs. Fitted values plot
plot(glm_larvae_red, which = 3)
# Cook's distance plot
plot(glm_larvae_red, which = 4)
# Multicollinearity
vif(glm_larvae_red)
# Plot Leverage
plot(glm_larvae_red, which = 5)
# Extract leverage values
leverage <- hatvalues(glm_larvae_red)
high_leverage <- which(leverage > (2 * mean(leverage)))
print("Observations with high leverage:")
print(high_leverage)
# Perform Hosmer-Lemeshow test
hoslem_test <- hoslem.test(glm_larvae_red$y, fitted(glm_larvae_red))
print("Hosmer-Lemeshow test results:")
print(hoslem_test)

# Residual Normality Analysis
qqnorm(residuals(glm_larvae_med))
qqline(residuals(glm_larvae_med))
# Homoscedasticity
plot(glm_larvae_med, which = 1)  # 1 corresponds to residuals vs. fitted values
# Residuals vs. Fitted values plot
plot(glm_larvae_med, which = 3)
# Cook's distance plot
plot(glm_larvae_med, which = 4)
# Multicollinearity
vif(glm_larvae_med)
# Plot Leverage
plot(glm_larvae_med, which = 5)
# Extract leverage values
leverage <- hatvalues(glm_larvae_med)
high_leverage <- which(leverage > (2 * mean(leverage)))
print("Observations with high leverage:")
print(high_leverage)
# Perform Hosmer-Lemeshow test
hoslem_test <- hoslem.test(glm_larvae_med$y, fitted(glm_larvae_med))
print("Hosmer-Lemeshow test results:")
print(hoslem_test)

library(performance)
r2_value_med <- r2(glm_larvae_med)
r2_value_red <- r2(glm_larvae_red)
r2_value_singapore <- r2(glm_larvae_singapore)

print(r2_value_med)
print(r2_value_red)
print(r2_value_singapore)

#######################################
## Potential distribution of P. nigra under a global change scenario
#includes Fig. 4-6
#change path to adjust file location if needed

plot(output_matrix)
sum(output_matrix, na.rm = T)
nc_shallow = output_matrix

# # two large tos and sos files were created using:
# # ncrcat sos_*.nc -o merged_sos.nc
# # ncrcat tos_*.nc -o merged_tos.nc
# 

nc_tos = nc_open("~/Documents/Papers/Unger et al. 2025/distribution_maps/merged_tos_time.nc")
nc_sos = nc_open("~/Documents/Papers/Unger et al. 2025/distribution_maps/merged_sos_time.nc")
tos_times = ncvar_get(nc_tos, "time")
sos_times = ncvar_get(nc_sos, "time")
#check if the exact same order
sum(abs(tos_times - sos_times))

#We got zero so we can use only one of them
nc_date = tos_times
library(lubridate)
# Convert to Date
nc_date <- as.Date(nc_date, origin = "1850-01-01")
# Extract the month
month_numeric <- month(nc_date)
year = year(nc_date)
all_coords = list()
for (i in 1:dim(nc_shallow)[1]){
  for (j in 1:dim(nc_shallow)[2]){
    if (!is.na(nc_shallow[i,j]) & nc_shallow[i,j]==1) {# this is a beach note that the indexes are opposite here
      #read the data from tos
      #browser()
      print(paste(i, j))
      tos_data = ncvar_get(nc_tos, "tos", start = c(i, j, 1), count = c(1, 1, -1))
      sos_data = ncvar_get(nc_sos, "sos", start = c(i, j, 1), count = c(1, 1, -1))
      lon = ncvar_get(nc_tos, "longitude", start = c(i, j), count = c(1, 1))
      lat = ncvar_get(nc_tos, "latitude", start = c(i, j), count = c(1, 1))
      #create data frame
      data = data.frame(date=nc_date, lat=lat, lon=lon, tos = tos_data, sos=sos_data)
      #add to list using append
      all_coords = append(all_coords, list(data))
    }
  }
}

#combine all dataframes in list using bind_rows
all_data = bind_rows(all_coords)

#add columns of concurrent and future climates
library(data.table)
all_data = as.data.table(all_data)
all_data$year = year(all_data$date)
all_data$period = ifelse(all_data$year < 2075, "current", "future")

#add month column
all_data$month = month(all_data$date)

#use groupby to calculate average per month for concurrent and future climates
summary_stats <- all_data[, .(
  max_tos = max(tos),
  min_tos = min(tos),
  max_sos = max(sos),
  min_sos = min(sos)
), by = .(lat, lon, month, period)]

write.csv(summary_stats, file="min_max_beached_conditions.csv", row.names = F)


#survival statistical models 
#survival of grown by day 7 of the experiment.
data_grown = read.csv("results_survival.csv")
data_larvae = read.csv("results_larvae.csv")

library(dplyr)
library(mgcv)

# Define a function for modeling and predictions
process_population <- function(data_grown, data_larvae, origin_name, temp_range = 0:40, salinity_range = 0:45) {
  
  # Filter data for the specified population
  grown_data <- data_grown %>% filter(origin == tolower(origin_name))
  larvae_data <- data_larvae %>% filter(origin == origin_name)
  
  # Results list to store outputs
  results <- list()
  
  # Process grown data
  if (nrow(grown_data) > 0) {
    #fit a gam model
    model_gam_grown <- gam(survived ~ s(temperature, salinity, k = 3), 
                           data = grown_data, family = "binomial")
    #predictions based on data
    grown_predictions <- predict(model_gam_grown, type = "response")
    grown_data$prediction <- round(grown_predictions * 100)
    #predictions for out-range (=new) data
    new_data_grown <- expand.grid(temperature = temp_range, salinity = salinity_range)
    new_data_grown$predictions <- round(predict(model_gam_grown, 
                                                newdata = new_data_grown, 
                                                type = "response") * 100)
    
    results$grown <- list(
      model_gam = model_gam_grown,
      predictions = grown_data,
      new_data_predictions = new_data_grown
    )
  }
  
  # Process larvae data
  if (nrow(larvae_data) > 0) {
    model_gam_larvae <- gam(success ~ s(temperature, salinity, k = 3), 
                            data = larvae_data, family = "binomial")
    larvae_predictions <- predict(model_gam_larvae, type = "response")
    larvae_data$prediction <- round(larvae_predictions * 100)
    
    new_data_larvae <- expand.grid(temperature = temp_range, salinity = salinity_range)
    new_data_larvae$predictions <- round(predict(model_gam_larvae, 
                                                 newdata = new_data_larvae, 
                                                 type = "response") * 100)
    
    results$larvae <- list(
      model_gam = model_gam_larvae,
      predictions = larvae_data,
      new_data_predictions = new_data_larvae
    )
  }
  
  return(results)
}

# Apply the function for each population
results_med <- process_population(data_grown, data_larvae, "Med")
results_red <- process_population(data_grown, data_larvae, "Red")
results_singapore <- process_population(data_grown, data_larvae, "Singapore")

#save files with predictions
write.csv(results_med$grown$new_data_predictions, file="med_grown_predictions.csv", row.names = F)
write.csv(results_med$larvae$new_data_predictions, file="med_larvae_predictions.csv", row.names = F)
write.csv(results_red$grown$new_data_predictions, file="red_grown_predictions.csv", row.names = F)
write.csv(results_red$larvae$new_data_predictions, file="red_larvae_predictions.csv", row.names = F)
write.csv(results_singapore$grown$new_data_predictions, file="singapore_grown_predictions.csv", row.names = F)
write.csv(results_singapore$larvae$new_data_predictions, file="singapore_larvae_predictions.csv", row.names = F)


#get the table of all data
beach_table = read.csv("min_max_beached_conditions.csv")

survival_pred_files = dir(pattern="*_predictions.csv")


calculate_survival <- function(survival_pred_data, beach_data) {
  # Add columns for survival statistics in the summary_data
  beach_data$mean_survival <- 0
  beach_data$min_survival <- 0
  beach_data$max_survival <- 0
  
  # Initialize the progress bar
  pb <- txtProgressBar(min = 0, max = nrow(beach_data), style = 3)
  
  # Loop through each row in the summary_data
  for (i in seq_len(nrow(beach_data))) {
    # Get the current ranges for tos and sos
    tos_min <- round(beach_data$min_tos[i])
    tos_max <- round(beach_data$max_tos[i])
    sos_min <- round(beach_data$min_sos[i])
    sos_max <- round(beach_data$max_sos[i])
    
    # Subset the data for matching tos and sos values
    matching_rows <- survival_pred_data[survival_pred_data$temperature >= tos_min & survival_pred_data$temperature <= tos_max &
                                          survival_pred_data$salinity >= sos_min & survival_pred_data$salinity <= sos_max, ]
    # print(matching_rows)
    # Calculate survival statistics if there are matches
    if (nrow(matching_rows) > 0) {
      beach_data$mean_survival[i] <- mean(matching_rows$predictions, na.rm = TRUE)
      beach_data$min_survival[i] <- min(matching_rows$predictions, na.rm = TRUE)
      beach_data$max_survival[i] <- max(matching_rows$predictions, na.rm = TRUE)
    } 
    # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(beach_data)
}

for (file_name in survival_pred_files[1:6]){
  new_data_predictions = read.csv(file_name)
  print(file_name)
  beaches_table_with_predictions = calculate_survival(new_data_predictions, beach_table)
  # Split the file name by underscores
  split_name <- unlist(strsplit(file_name, "_"))
  # Save the first two strings into variables
  population <- split_name[1]
  life_stage <- split_name[2]
  write.csv(beaches_table_with_predictions, paste(population, life_stage, "beaches_survival.csv", sep = "_" ), row.names = F)
}

#now we have a table per origin and life stage of survival predictions in "beaches"

#add code to concat all tables together after adding pop and life stage columns
# List all survival tables generated from "beaches_table_with_predictions"
beaches_survival_files <- dir(pattern = "*_beaches_survival.csv")

# Initialize an empty list to store the data frames
all_beaches_survival <- list()

# Loop through each file
for (file_name in beaches_survival_files) {
  # Read the data
  survival_data <- read.csv(file_name)
  
  # Extract 'origin' and 'life_stage' from the file name
  split_name <- unlist(strsplit(file_name, "_"))
  population <- split_name[1]  # e.g., "med"
  life_stage <- split_name[2]  # e.g., "grown"
  
  # Add 'origin' and 'life_stage' columns
  survival_data$origin <- population
  survival_data$life_stage <- life_stage
  
  # Append the data frame to the list
  all_beaches_survival <- append(all_beaches_survival, list(survival_data))
}

# Combine all data frames into one big table
combined_beaches_data <- bind_rows(all_beaches_survival)

# Save the combined data
write.csv(combined_beaches_data, "all_beaches_survival_data.csv", row.names = FALSE)
all_beaches_survival_data = read.csv("all_beaches_survival_data.csv")

#######################################

#group for each lat/lon yearly survival per life stage
#filter out only the grown data
juvenile_data <- all_beaches_survival_data %>%
  filter(life_stage == "grown") 
unique(juvenile_data$life_stage) #make sure i have only grown now

#group by months
yearly_mean_juvenile <- juvenile_data %>%
  group_by(lat, lon, period, origin) %>%
  summarise(mean_survival = mean(mean_survival, na.rm = TRUE), .groups = "drop")
write.csv(yearly_mean_juvenile, "all_yearly_mean_juvenile.csv", row.names = FALSE)


#filter out only the larvae data
larva_data <- all_beaches_survival_data %>%
  filter(life_stage == "larvae", min_tos >= 25) 
unique(larva_data$life_stage) #make sure i have only larvae now

#group by months over 25C
yearly_mean_larvae <- larva_data %>%
  group_by(lat, lon, period, origin) %>%
  summarise(mean_survival = mean(mean_survival, na.rm = TRUE), .groups = "drop")
write.csv(yearly_mean_larvae, "all_yearly_mean_larvae.csv", row.names = FALSE)


##### ##### ##### bug check ##### ##### ##### 
b = all_beaches_survival_data %>%
  filter(lat== 26.7600548429654, lon== 35.895107351487)
View(b)

dot <- read.csv("min_max_beached_conditions.csv")
b = dot %>%
  filter(lat== 26.7600548429654, lon== 35.895107351487)
View(b)
red_p <- read.csv("red_grown_predictions.csv")

new <- calculate_survival(red_p, b)
View(new)

#######################################

### map plots per life stage and population: 
library(ggplot2)
library(dplyr)
library(maps)  

all_yearly_mean_juvenile = read.csv("all_yearly_mean_juvenile.csv")
all_yearly_mean_larvae = read.csv("all_yearly_mean_larvae.csv")

#function for juvenile maps
map_plot_juvenile <- function(population) {
  map_data <- all_yearly_mean_juvenile %>%
    filter(period == "current", origin == population)
  map_data$lon <- ifelse(map_data$lon > 180, map_data$lon - 360, map_data$lon)
  world_map <- map_data("world")
  plot <- ggplot() +
    geom_polygon(
      data = world_map,
      aes(x = long, y = lat, group = group),
      fill = "gray90", color = "gray50", size = 0.3
    ) +
    geom_point(
      data = map_data,
      aes(x = lon, y = lat, color = mean_survival),
      size = 0.5
    ) +
    scale_color_viridis_c(option = "plasma", limits = c(0, 100)) +
    labs(
      title = paste("Survival for current grown", population), 
      x = "Longitude",
      y = "Latitude",
      color = "Survival"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(), 
      panel.border = element_rect(color = "black", fill = NA)  
    )
  print(plot)
}

#plot per population
populations = c("med", "red", "singapore")

for (pop in populations) {
  plot <- map_plot_juvenile(population = pop) 
  assign(paste0("map_grown_", pop), plot)
  
}

#######################################
#function for larvae maps
map_plot_larvae <- function(population) {
  map_data <- all_yearly_mean_larvae %>%
    filter(period == "current", origin == population)
  map_data$lon <- ifelse(map_data$lon > 180, map_data$lon - 360, map_data$lon)
  world_map <- map_data("world")
  plot <- ggplot() +
    geom_polygon(
      data = world_map,
      aes(x = long, y = lat, group = group),
      fill = "gray90", color = "gray50", size = 0.3
    ) +
    geom_point(
      data = map_data,
      aes(x = lon, y = lat, color = mean_survival),
      size = 0.5
    ) +
    scale_color_viridis_c(option = "plasma", limits = c(0, 100)) +
    labs(
      title = paste("Survival for current larvae", population), 
      x = "Longitude",
      y = "Latitude",
      color = "Survival"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(), 
      panel.border = element_rect(color = "black", fill = NA)  
    )
  print(plot)
}

#plot per population
populations = c("med", "red", "singapore")

for (pop in populations) {
  plot <- map_plot_larvae(population = pop) 
  assign(paste0("map_larvae_", pop), plot)
  
}


#####################################################################
# difference in survival from current to future maps
library(ggplot2)
library(viridis)
library(scales)
library(dplyr)
library(maps)

plot_survival_change <- function(data, pop, stage) {
  world_map <- map_data("world")
  
  # Filter for the current and future survival data
  current_data <- data %>%
    filter(origin == pop, period == "current") %>%
    select(lat, lon, mean_survival) %>%
    rename(survival_current = mean_survival)  # Rename column for clarity
  
  future_data <- data %>%
    filter(origin == pop, period == "future") %>%
    select(lat, lon, mean_survival) %>%
    rename(survival_future = mean_survival)  # Rename column for clarity
  
  # Merge datasets by lat/lon to compute the difference
  diff_data <- merge(current_data, future_data, by = c("lat", "lon"))
  
  # Compute survival change
  diff_data$survival_change <- diff_data$survival_future - diff_data$survival_current  
  
  # Adjust longitude if necessary
  diff_data$lon <- ifelse(diff_data$lon > 180, diff_data$lon - 360, diff_data$lon)
  
  # Custom colors
  custom_colors <- c("darkred", "red", "white", "green", "darkgreen")
  color_positions <- scales::rescale(c(-100, -20, 0, 20, 100), to = c(0, 1))
  
  # Create the map
  p <- ggplot() +
    geom_polygon(
      data = world_map,
      aes(x = long, y = lat, group = group),
      fill = "gray90", color = "gray50", linewidth = 0.3
    ) +
    geom_point(
      data = diff_data,
      aes(x = lon, y = lat, color = survival_change),
      size = 0.5
    ) +
    scale_color_gradientn(
      colors = custom_colors,
      values = color_positions,
      limits = c(-100, 100)
    ) +
    labs(
      title = paste("Change in Survival: Future vs Current (", pop, " - ", stage, ")", sep = ""),
      x = "Longitude",
      y = "Latitude",
      color = "Survival"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA)
    )
  
  print(p)
  assign(paste0("survival_change_", pop, "_", stage), p, envir = .GlobalEnv)
}

populations <- c("med", "red", "singapore")

# Run for grown dataset
for (pop in populations) {
  plot_survival_change(all_yearly_mean_juvenile, pop, "grown")
}

# Run for larvae dataset
for (pop in populations) {
  plot_survival_change(all_yearly_mean_larvae, pop, "larvae")
}

