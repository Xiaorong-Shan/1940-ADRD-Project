library( ggplot2)
library( data.table)
library( viridis)
library( tidyverse)
library( sf)
library(readxl)
library(raster)
library(fst)
library(dplyr)

#==========================================================================================================
#Read 2010 county shape file
#==========================================================================================================
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"  

dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_2010_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_2010.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))

roadiness.trans.county.select <- roadiness.trans.county[, c("statefp10", "countyfp10")]

#==========================================================================================================
#2010 model exposure
#==========================================================================================================
combined_df.all <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/Well_Locations/well_idw/2010_well_county_idw_12km_100buffer_NHGIS.csv")

combined_df.all <- combined_df.all %>%
  mutate(statefp10 = sprintf("%02d", as.numeric(statefp10)))

combined_df.all <- combined_df.all %>%
  mutate(countyfp10 = sprintf("%03d", as.numeric(countyfp10)))

#merge 2010 model exposure with county shape file
combined_df.sf <- merge(combined_df.all, roadiness.trans.county.select, 
                   by = c("statefp10", "countyfp10"))

# Sum PM2.5_con for each unique combination of statefp10 and countyfp10
result <- combined_df.sf %>%
  group_by(statefp10, countyfp10) %>%
  summarise(sum_well_idw = sum(well_idw, na.rm = TRUE)) %>%
  ungroup()
#==========================================================================================================
#2010 measurement
#==========================================================================================================
monitor.all <- read.csv(file ="/home/xshan2/HAQ_LAB/xshan2/R_Code/2010_Measurement/annual_conc_by_monitor_2010.csv")

monitor.s <- monitor.all[, c("State.Code", "County.Code", "State.Name", "County.Name", "Latitude", "Longitude", "Parameter.Name", "Parameter.Code", "Sample.Duration", "Arithmetic.Mean", "Arithmetic.Standard.Dev", "Units.of.Measure", "Metric.Used")]

monitor.s <- monitor.s %>%
  mutate(State.Code = sprintf("%02d", as.numeric(State.Code)))

monitor.s <- monitor.s %>%
  mutate(County.Code = sprintf("%03d", as.numeric(County.Code)))

#==========================================================================================================
#Let's start evaluation by species
#==========================================================================================================
#function
calculate_metrics <- function(data, observed_col, predicted_col) {
  # Extract the observed and predicted values
  observed <- data[[observed_col]]
  predicted <- data[[predicted_col]]
  
  # Define individual metric calculations
  # Calculate the squared differences
  squared_diffs <- (predicted - observed) ^ 2
  
  # Calculate R, R^2
  pearson_r <- round(cor(observed, predicted, method = "pearson"), 2)
  spearman_r <- round(cor(observed, predicted, method = "spearman"), 2)
  r_squared <- round(summary(lm(predicted ~ observed))$r.squared, 2)
  
  # Return a list of metrics
 return(list(
    Pearson_R = pearson_r,
    Spearman_R = spearman_r,
    R_squared = r_squared
  ))
}


#==========================================================================================================
#Parameter.Name = Benzene, Parameter.Code = 45201, Sample.Duration = 24 HOUR

monitor.s_c6h6 <- monitor.s[monitor.s$Parameter.Code == 45201 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_c6h6.sf <- merge(result, monitor.s_c6h6, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_well_c6h6 <- calculate_metrics(monitor.s_c6h6.sf, 'Arithmetic.Mean', 'sum_well_idw')

#==========================================================================================================
#Parameter.Name = Sulfur dioxide, Parameter.Code = 42401, Sample.Duration = 1 HOUR

monitor.s_so2 <- monitor.s[monitor.s$Parameter.Code == 42401 & monitor.s$Sample.Duration == "1 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_so2.sf <- merge(result, monitor.s_so2, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_well_so2 <- calculate_metrics(monitor.s_so2.sf, 'Arithmetic.Mean', 'sum_well_idw')



#==========================================================================================================
#Parameter.Name = Total NMOC (non-methane organic compound), Parameter.Code = 43102, Sample.Duration = 24 HOUR

monitor.s_nmoc <- monitor.s[monitor.s$Parameter.Code == 43102 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_nmoc.sf <- merge(result, monitor.s_nmoc , by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_well_nmoc <- calculate_metrics(monitor.s_nmoc.sf, 'Arithmetic.Mean', 'sum_well_idw')


  
  
#===========================================================================================================
#Total Sulfur
Total_OC_EC_S <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/2010_Measurement/total_EC_OC_S.csv")

Total_OC_EC_S <- Total_OC_EC_S %>%
  mutate(State.Code = sprintf("%02d", as.numeric(State.Code)))

Total_OC_EC_S <- Total_OC_EC_S %>%
  mutate(County.Code = sprintf("%03d", as.numeric(County.Code)))
  
#merge new exposure dataset with automobile

total_measurement_well_idw.sf <- merge(result, Total_OC_EC_S, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))
  
  
Total_S_STP.dt <- total_measurement_well_idw.sf[total_measurement_well_idw.sf$pollutant == "Total_S_STP", ]

evaluat_well_Total_S_STP <- calculate_metrics(Total_S_STP.dt, 'Exposure', 'sum_well_idw')
  
Total_S_LC.dt <- total_measurement_well_idw.sf[total_measurement_well_idw.sf$pollutant == "Total_S_LC", ]

evaluat_well_Total_S_LC <- calculate_metrics(Total_S_LC.dt, 'Exposure', 'sum_well_idw')
  
  
  
  
monitor.s_no2 <- monitor.s[monitor.s$Parameter.Code == 44201 & monitor.s$Sample.Duration == "8-HR RUN AVG BEGIN HOUR", ]

aggregated_means <- monitor.s_no2 %>%
  group_by(State.Code, County.Code) %>%
  summarise(Mean_Arithmetic_Mean = mean(Arithmetic.Mean, na.rm = TRUE))
  
#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_no2.sf <- merge(result, aggregated_means, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_no2 <- calculate_metrics(monitor.s_no2.sf, 'Mean_Arithmetic_Mean', 'sum_well_idw')
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
