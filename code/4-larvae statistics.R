###stress experiment reproductive products - STATISTICS
#change path to adjust file location if needed
larvae_binomial <- read.csv("~/Library/CloudStorage/OneDrive-mail.tau.ac.il/Papers/Unger et al. 2025/data/larvae_results.csv")

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

