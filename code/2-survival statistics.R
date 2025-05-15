#survival statistics:
#change path to adjust file location if needed
results <- read.csv("~/Library/CloudStorage/OneDrive-mail.tau.ac.il/Papers/Unger et al. 2025/data/survival_results.csv")
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

