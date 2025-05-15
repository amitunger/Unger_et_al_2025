#kaplan-meir survival analysis
library(survival)
library(dplyr)

# Define the function
run_survival_analysis <- function(data, population_name) {
  # Filter for the desired population
  pop_data <- data %>% filter(origin == population_name)
  
  # Define variables
  day <- pop_data$day
  event <- pop_data$event
  salinity <- pop_data$salinity
  temperature <- pop_data$temperature
  
  # Descriptive statistics
  cat("\n===== Descriptive Stats for", population_name, "=====\n")
  print(summary(day))
  print(summary(event))
  print(summary(salinity))
  print(summary(temperature))
  
  ## KM by salinity
  cat("\n===== Kaplan-Meier by Salinity =====\n")
  km_sal <- survfit(Surv(day, event) ~ salinity)
  print(summary(km_sal))
  
  # Plot KM curve by salinity
  plot(km_sal, conf.int=FALSE, xlab="Time [days]", ylab="Survival [%]",
       main=paste("KM-Model by salinity in", population_name),
       col=c("#CC9933", "#CC3399", "#33CC99"), las=1)
  legend("topright", legend=levels(as.factor(salinity)), lty=1,
         col=c("#CC9933", "#CC3399", "#33CC99"), bty="n", cex=0.6)
  
  # Log-rank test for salinity
  cat("\nLog-rank test (salinity):\n")
  print(survdiff(Surv(day, event) ~ salinity))
  
  ## KM by temperature
  cat("\n===== Kaplan-Meier by Temperature =====\n")
  km_temp <- survfit(Surv(day, event) ~ temperature)
  print(summary(km_temp))
  
  # Plot KM curve by temperature
  plot(km_temp, conf.int=FALSE, xlab="Time [days]", ylab="Survival [%]",
       main=paste("KM-Model by temperature in", population_name),
       col=c("#3399CC", "#FF0066", "#666000"), las=1)
  legend("topright", legend=levels(as.factor(temperature)), lty=1,
         col=c("#3399CC", "#FF0066", "#666000"), bty="n", cex=0.6)
  
  # Log-rank test for temperature
  cat("\nLog-rank test (temperature):\n")
  print(survdiff(Surv(day, event) ~ temperature))
}

# Load the data
#change path to adjust file location if needed
data <- read_csv("~/Library/CloudStorage/OneDrive-mail.tau.ac.il/Papers/Unger et al. 2025/data/survival_results.csv")

# Run for each population
run_survival_analysis(data, "med")
run_survival_analysis(data, "red")
run_survival_analysis(data, "Singapore")


