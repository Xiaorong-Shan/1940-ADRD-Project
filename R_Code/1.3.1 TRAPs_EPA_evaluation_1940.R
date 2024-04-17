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
combined_df.all <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/auto_county_2010_NHGIS.csv")

combined_df.all <- combined_df.all %>%
  mutate(statefp10 = sprintf("%02d", as.numeric(statefp10)))

combined_df.all <- combined_df.all %>%
  mutate(countyfp10 = sprintf("%03d", as.numeric(countyfp10)))

#merge 2010 model exposure with county shape file
combined_df.sf <- merge(combined_df.all, roadiness.trans.county.select, 
                   by = c("statefp10", "countyfp10"))

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

#==========================================================================================================
#function
calculate_metrics <- function(data, observed_col, predicted_col) {
  # Extract the observed and predicted values
  observed <- data[[observed_col]]
  predicted <- data[[predicted_col]]
  
  # Define individual metric calculations
  # Calculate the squared differences
  squared_diffs <- (predicted - observed) ^ 2
  
  # Calculate RMSE, MB, NMB, NME, R, R^2
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
#Parameter.Name = PM2.5 - Local Conditions, Parameter.Code = 88101, Sample.Duration = 24 HOUR

monitor.local_PM25 <- monitor.s[monitor.s$Parameter.Code == 88101 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
model_PM25.sf <- merge(combined_df.sf, monitor.local_PM25, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_PM25 <- calculate_metrics(model_PM25.sf, 'Arithmetic.Mean', 'county_gas')

#==========================================================================================================
#Parameter.Name = Nitrogen dioxide (NO2), Parameter.Code = 42602, Sample.Duration = 1 HOUR
  
monitor.s_no2 <- monitor.s[monitor.s$Parameter.Code == 42602 & monitor.s$Sample.Duration == "1 HOUR", ]

aggregated_means <- monitor.s_no2 %>%
  group_by(State.Code, County.Code) %>%
  summarise(Mean_Arithmetic_Mean = mean(Arithmetic.Mean, na.rm = TRUE))
  
#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_no2.sf <- merge(combined_df.sf, aggregated_means, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_no2 <- calculate_metrics(monitor.s_no2.sf, 'Mean_Arithmetic_Mean', 'county_gas')



#==========================================================================================================
#Parameter.Name = Oxides of nitrogen (NOx), Parameter.Code = 42603, Sample.Duration = 1 HOUR
  
monitor.s_nox <- monitor.s[monitor.s$Parameter.Code == 42603 & monitor.s$Sample.Duration == "1 HOUR", ]

aggregated_means <- monitor.s_nox %>%
  group_by(State.Code, County.Code) %>%
  summarise(Mean_Arithmetic_Mean = mean(Arithmetic.Mean, na.rm = TRUE))

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_nox.sf <- merge(combined_df.sf, aggregated_means, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_nox <- calculate_metrics(monitor.s_nox.sf, 'Mean_Arithmetic_Mean', 'county_gas')  



#==========================================================================================================
#OMI NO2

satellite.no2 <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/2010_Measurement/satellite/OMI_Satellite_NO2_2010_county_NHGIS.csv")

satellite.no2 <- satellite.no2 %>%
  mutate(statefp10 = sprintf("%02d", as.numeric(statefp10)))

satellite.no2 <- satellite.no2 %>%
  mutate(countyfp10 = sprintf("%03d", as.numeric(countyfp10)))

#merge 2010 model exposure with county shape file
model_omi.sf <- merge(satellite.no2, combined_df.sf, 
                   by = c("statefp10", "countyfp10"))

evaluat_auto_omi_no2 <- calculate_metrics(model_omi.sf, 'no2_ppb', 'county_gas') 

#===========================================================================================================
#OC

Total_OC_EC_S <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/2010_Measurement/total_EC_OC_S.csv")

Total_OC_EC_S <- Total_OC_EC_S %>%
  mutate(State.Code = sprintf("%02d", as.numeric(State.Code)))

Total_OC_EC_S <- Total_OC_EC_S %>%
  mutate(County.Code = sprintf("%03d", as.numeric(County.Code)))
  
#merge new exposure dataset with automobile

total_measurement_auto.sf <- merge(combined_df.sf, Total_OC_EC_S, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))
  
  
Unadjusted_total_OC_TOR.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Unadjusted_total_OC_TOR", ]

evaluat_auto_Un_OC_TOR <- calculate_metrics(Unadjusted_total_OC_TOR.dt, 'Exposure', 'county_gas')   
  
Unadjusted_total_OC_TOT.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Unadjusted_total_OC_TOT", ]


evaluat_auto_Un_OC_TOT <- calculate_metrics(Unadjusted_total_OC_TOT.dt, 'Exposure', 'county_gas')   



  
#===========================================================================================================
#EC


Unadjusted_total_EC_TOR.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Unadjusted_total_EC_TOR", ]

evaluat_auto_Un_EC_TOR <- calculate_metrics(Unadjusted_total_EC_TOR.dt, 'Exposure', 'county_gas')   
  
  
Unadjusted_total_EC_TOT.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Unadjusted_total_EC_TOT", ]

evaluat_auto_Un_EC_TOT <- calculate_metrics(Unadjusted_total_EC_TOT.dt, 'Exposure', 'county_gas')   

#===========================================================================================================
#Iron

monitor.iron_pm10 <- monitor.s[monitor.s$Parameter.Code == 85126 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.iron_pm10.sf <- merge(combined_df.sf, monitor.iron_pm10, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_iron_pm10 <- calculate_metrics(monitor.iron_pm10.sf, 'Arithmetic.Mean', 'county_gas')  

#Iron

monitor.iron_pm10_25 <- monitor.s[monitor.s$Parameter.Code == 86126 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.iron_pm10_25.sf <- merge(combined_df.sf, monitor.iron_pm10_25, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_iron_pm10_25 <- calculate_metrics(monitor.iron_pm10_25.sf, 'Arithmetic.Mean', 'county_gas')  

#Iron

monitor.iron_pm10 <- monitor.s[monitor.s$Parameter.Code == 82126 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.iron_pm10.sf <- merge(combined_df.sf, monitor.iron_pm10, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_iron_pm10 <- calculate_metrics(monitor.iron_pm10.sf, 'Arithmetic.Mean', 'county_gas')  

#===========================================================================================================
#Zinc

monitor.zinc_pm10 <- monitor.s[monitor.s$Parameter.Code == 85167 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.zinc_pm10.sf <- merge(combined_df.sf, monitor.zinc_pm10, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_zinc_pm10 <- calculate_metrics(monitor.zinc_pm10.sf, 'Arithmetic.Mean', 'county_gas')  




monitor.zinc_pm10_pm25 <- monitor.s[monitor.s$Parameter.Code == 86167 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.zinc_pm10_pm25.sf <- merge(combined_df.sf, monitor.zinc_pm10_pm25, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_zinc_pm10_pm25 <- calculate_metrics(monitor.zinc_pm10_pm25.sf, 'Arithmetic.Mean', 'county_gas')  



monitor.zinc_pm10_pm25 <- monitor.s[monitor.s$Parameter.Code == 82167 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.zinc_pm10_pm25.sf <- merge(combined_df.sf, monitor.zinc_pm10_pm25, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_zinc_pm10_pm25 <- calculate_metrics(monitor.zinc_pm10_pm25.sf, 'Arithmetic.Mean', 'county_gas')  



#===========================================================================================================
#Ammonium

monitor.zinc_pm10 <- monitor.s[monitor.s$Parameter.Code == 85167 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.zinc_pm10.sf <- merge(combined_df.sf, monitor.zinc_pm10, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_zinc_pm10 <- calculate_metrics(monitor.zinc_pm10.sf, 'Arithmetic.Mean', 'county_gas')  





#Iron PM2.5
monitor.s_nox <- monitor.s[monitor.s$Parameter.Code == 88126 & monitor.s$Sample.Duration == "24 HOUR", ]

aggregated_means <- monitor.s_nox %>%
  group_by(State.Code, County.Code) %>%
  summarise(Mean_Arithmetic_Mean = mean(Arithmetic.Mean, na.rm = TRUE))

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_nox.sf <- merge(combined_df.sf, aggregated_means, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_nox <- calculate_metrics(monitor.s_nox.sf, 'Mean_Arithmetic_Mean', 'county_gas')  








monitor.s_nox <- monitor.s[monitor.s$Parameter.Code == 88167 & monitor.s$Sample.Duration == "24 HOUR", ]

aggregated_means <- monitor.s_nox %>%
  group_by(State.Code, County.Code) %>%
  summarise(Mean_Arithmetic_Mean = mean(Arithmetic.Mean, na.rm = TRUE))

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_nox.sf <- merge(combined_df.sf, aggregated_means, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_nox <- calculate_metrics(monitor.s_nox.sf, 'Mean_Arithmetic_Mean', 'county_gas')  






monitor.s_nox <- monitor.s[monitor.s$Parameter.Code == 88306 & monitor.s$Sample.Duration == "24 HOUR", ]

aggregated_means <- monitor.s_nox %>%
  group_by(State.Code, County.Code) %>%
  summarise(Mean_Arithmetic_Mean = mean(Arithmetic.Mean, na.rm = TRUE))

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_nox.sf <- merge(combined_df.sf, aggregated_means, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

evaluat_auto_nox <- calculate_metrics(monitor.s_nox.sf, 'Mean_Arithmetic_Mean', 'county_gas')  














