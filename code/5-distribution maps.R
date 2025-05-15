## Potential distribution of P. nigra under a global change scenario
#includes Figures 4-6
#create map of "beaches" - areas that are close to land and up to 100 meters depth
library(ncdf4)
library(terra)
library(dplyr)

nc_depths = nc_open("~/Documents/Papers/Unger et al. 2025/distribution_maps/deptho_Ofx_MPI-ESM1-2-HR_ssp585_r1i1p1f1_gn.nc")
depth = ncvar_get(nc_depths, "deptho")
nc_shallow = depth
nc_shallow[!is.na(depth)]=1
nc_shallow[(depth>100) & !is.na(depth)]=0

# Get the dimensions of the matrix
nrows <- nrow(nc_shallow)
ncols <- ncol(nc_shallow)

# Iterate over the matrix (excluding edges)
output_matrix <- nc_shallow
for (i in 2:(nrows - 1)) {
  for (j in 2:(ncols - 1)) {
    # Check if the center pixel is 0
    if (!is.na(nc_shallow[i, j]) & (nc_shallow[i, j] == 0)) {
      # Extract the 3x3 neighborhood
      neighbors <- nc_shallow[(i - 1):(i + 1), (j - 1):(j + 1)]
      
      # Check if any of the neighbors are NA
      if (any(is.na(neighbors))) {
        # Set the output pixel to 1
        output_matrix[i, j] <- 1
      }
    }
  }
}

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
