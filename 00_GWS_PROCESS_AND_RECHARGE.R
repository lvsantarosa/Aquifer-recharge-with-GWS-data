########################################################################
# Script to calculate daily difference, accumulate, and obtain the annual maximum value per pixel
# Author: Lucas Vituri Santarosa 
########################################################################

setwd('c:/Users/lucas/Documents/02_ARTIGOS/04_BAURU_GWS/')

library(sf)          
library(terra)      
library(tidyverse)
library(glue)
library(lubridate)

# Data Processing --------------------------------------------------

# List containing all files in the GRACE folder to create the raster
file_list <- list.files(path = '01_GWS/01_DADOS/SAB_GWS/', full.names = TRUE, pattern = ".nc4$")

# CRS to convert coordinate systems of the shapefile
CRS <- "+proj=longlat +datum=WGS84"

# Filtering the shapefile for the state of interest
shp <- vect('00_SHP/00_LIMITE_AREA/GRID.gpkg') %>% # Define the file
       project(CRS)                                                # Convert the CRS for clipping

# Loading the raster file
gws <- terra::rast(file_list) %>% crop(shp)

# Plot to check the data
plot(gws[[1]])
lines(shp)

# Creating a monthly sequence
start_date <- as.Date('2003/10/01')
end_date <- as.Date('2021/09/30')
Date <-  seq(from = start_date, to = end_date, by= 'day')

names(gws) <- Date

# Recharge with daily data -----------------------------------------------

GRID <- vect('00_SHP/00_LIMITE_AREA/GRID.gpkg')
plot(GRID[1])

# Step 1: Import and organize the data

for(i in 1:length(GRID$ID)){
  
  nf <- gws %>% mask(GRID[i]) %>% 
    as.data.frame(na.rm = TRUE) %>% 
    pivot_longer(cols = '2003-10-01':'2021-09-30', 
                 names_to = "Date", values_to = 'WaterLevel') %>% 
    group_by(Date) %>% 
    summarise_at(vars(WaterLevel), list(WaterLevel = mean))
  
  nf   
  
  # Step 2: Calculate groundwater level variation
  data <- nf %>%
    arrange(Date) %>%
    mutate(
      WaterLevelChange = WaterLevel - lag(WaterLevel, default = first(WaterLevel))
    )
  
  # Step 3: Minimum significant variation 
  threshold <- 0.5
  data <- data %>%
    mutate(
      SignificantChange = ifelse(abs(WaterLevelChange) >= threshold, WaterLevelChange, 0)
    )
  
  # Step 4: Identify Positive and Negative Periods
  data <- data %>%
    mutate(
      PeriodType = case_when(
        SignificantChange > 0 ~ "Positive",
        SignificantChange < 0 ~ "Negative",
        TRUE ~ "Neutral"
      )
    )
  
  # Step 5: Calculate the difference between Positive and Negative Periods
  # Find the last negative point before each positive point
  data <- data %>%
    mutate(
      LastNegativeLevel = lag(ifelse(PeriodType == "Negative", WaterLevel, NA), order_by = Date)
    ) %>%
    fill(LastNegativeLevel, .direction = "down") %>%
    mutate(
      DifferenceFromLastNegative = ifelse(PeriodType == "Positive", WaterLevel - LastNegativeLevel, NA)
    )
  
  
  # Step 6: Extract the largest DifferenceFromLastNegative values per hydrological year
  # Add columns for hydrological year and month
  data <- data %>%
    mutate(
      HydroYear = ifelse(month(Date) >= 10, year(Date) + 1, year(Date)),
      Month = month(Date, label = TRUE)
    )
  
  # Extract the maximum hydrological annual values
  annual_max <- data %>%
    filter(!is.na(DifferenceFromLastNegative)) %>%
    group_by(HydroYear) %>%
    summarize(
      MaxDifference = max(DifferenceFromLastNegative, na.rm = TRUE),
      .groups = 'drop'
    )
  name <- GRID$ID[i]
  
  # Save the annual results in a new CSV file
  write.csv(annual_max, glue("01_GWS/01_DADOS/CSV/{name}.csv"), row.names = FALSE)
  
  print(name)
}


# Merging the CSV files and creating a shapefile with values per location --------

# Get the list of CSV files in the folder
csv_files <- list.files('01_GWS/01_DADOS/CSV/', pattern = "\\.csv$", full.names = TRUE)

# Create a list to store the read data frames
data_frame_list <- lapply(csv_files, read.csv)

# Add a column with the original file name (without the .csv extension)
file_names <- basename(csv_files)
file_names <- sub("\\.csv$", "", file_names)  # Remove the .csv extension

for (i in seq_along(data_frame_list)) {
  data_frame_list[[i]]$ID <- file_names[i]
}

# Combine the data frames into a single data frame
combined_data_frame <- do.call(rbind, data_frame_list)

transformed_df <- combined_data_frame  %>%
  pivot_wider(names_from = HydroYear, values_from = MaxDifference, values_fill = NA)
transformed_df

merged_GRID <- merge(GRID, transformed_df, by = 'ID')

writeVector(merged_GRID, '02_RESULTADOS/RECHARGE_SAB_GWS_GRACE_YEAR.shp', overwrite=TRUE)

# Define the range of years
years <- 2004:2021

raster <- list()

# Loop to rasterize each column
for (year in years) {
  
  # Rasterize the corresponding column
  raster <- terra::rasterize(merged_GRID, gws[[1]], field = as.character(year))
  
  # Plot the raster
  plot(raster, main = paste("Raster for the year", year), breaks=c(0, 50, 150, 250, 350, 450, 550, 650))
  
  writeRaster(raster, glue('02_RESULTADOS/RECHARGE_SAB_GWS_GRACE_{year}.tif'), overwrite=TRUE)
}

