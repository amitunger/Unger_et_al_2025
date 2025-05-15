#blood flow statistics
#change path to adjust file location if needed
res <- read.csv("~/Library/CloudStorage/OneDrive-mail.tau.ac.il/Papers/Unger et al. 2025/data/blood_flow.csv")

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


# SINGAPORE Linear Mixed-Effects Model (LMM)  
library(lme4)
library(emmeans)
library(lmerTest) 

# Create a new identifier column
res_singapore$identifier <- paste("plate", res_singapore$plate, "number", res_singapore$subject_number, sep = "_")

#making temp25 and salinity31 the control for comparison:
res_singapore$temperature <- relevel(res_singapore$temperature, ref = "25")

res_singapore$salinity <- relevel(res_singapore$salinity, ref = "31")

# Fit the mixed-effects model
lmer_model <- lmer(time ~ temperature * salinity * day + (1|identifier), data = res_singapore)
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

