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
combined_df.all <- read.csv("/scratch/xshan2/R_Code/Automobiles/auto_county_2010_DistanceEF_NHGIS.csv")

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
#Parameter.Name = PM2.5 - Local Conditions, Parameter.Code = 88101, Sample.Duration = 24 HOUR

monitor.local_PM25 <- monitor.s[monitor.s$Parameter.Code == 88101 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
model_PM25.sf <- merge(combined_df.sf, monitor.local_PM25, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_pm25_local.pearson <- cor(model_PM25.sf$county_gas, model_PM25.sf$Arithmetic.Mean, method = "pearson")
auto_pm25_local.spearman <- cor(model_PM25.sf$county_gas, model_PM25.sf$Arithmetic.Mean, method = "spearman")

#Parameter.Name = PM2.5 Total Atmospheric, Parameter.Code = 88500, Sample.Duration = 24-HR BLK AVG

monitor.s_PM25 <- monitor.s[monitor.s$Parameter.Code == 88500 & monitor.s$Sample.Duration == "24-HR BLK AVG", ]

#merge 2010 PM2.5 measurement with total exposure dataset
model_measurement.sf <- merge(combined_df.sf, monitor.s_PM25, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_pm25.pearson <- cor(model_measurement.sf$county_gas, model_measurement.sf$Arithmetic.Mean, method = "pearson")
auto_pm25.spearman <- cor(model_measurement.sf$county_gas, model_measurement.sf$Arithmetic.Mean, method = "spearman")
  
#==========================================================================================================
#Parameter.Name = PM10 Total 0-10um STP, Parameter.Code = 81102, Sample.Duration = 24 HOUR

monitor.s_PM10 <- monitor.s[monitor.s$Parameter.Code == 81102 & monitor.s$Sample.Duration == "24 HOUR", ]

pm10_data <- subset(combined_df.sf, emission_type == "PM10")

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_PM10.sf <- merge(pm10_data, monitor.s_PM10, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))


auto_pm10.pearson <- cor(monitor.s_PM10.sf$emission_value, monitor.s_PM10.sf$Arithmetic.Mean, method = "pearson")
auto_pm10.spearman <- cor(monitor.s_PM10.sf$emission_value, monitor.s_PM10.sf$Arithmetic.Mean, method = "spearman")

#==========================================================================================================
#Parameter.Name = Nitrogen dioxide (NO2), Parameter.Code = 42602, Sample.Duration = 1 HOUR
  
monitor.s_no2 <- monitor.s[monitor.s$Parameter.Code == 42602 & monitor.s$Sample.Duration == "1 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_no2.sf <- merge(combined_df.sf, monitor.s_no2, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_no2.pearson <- cor(monitor.s_no2.sf$county_gas, monitor.s_no2.sf$Arithmetic.Mean, method = "pearson")
auto_no2.spearman <- cor(monitor.s_no2.sf$county_gas, monitor.s_no2.sf$Arithmetic.Mean, method = "spearman")
  

#==========================================================================================================
#Parameter.Name = Oxides of nitrogen (NOx), Parameter.Code = 42603, Sample.Duration = 1 HOUR
  
monitor.s_nox <- monitor.s[monitor.s$Parameter.Code == 42603 & monitor.s$Sample.Duration == "1 HOUR", ]

nox_data <- subset(combined_df.sf, emission_type == "NOx")

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_nox.sf <- merge(nox_data, monitor.s_nox, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_nox.pearson <- cor(monitor.s_nox.sf$emission_value, monitor.s_nox.sf$Arithmetic.Mean, method = "pearson")
auto_nox.spearman <- cor(monitor.s_nox.sf$emission_value, monitor.s_nox.sf$Arithmetic.Mean, method = "spearman")
  
#==========================================================================================================
#Parameter.Name = Reactive oxides of nitrogen (NOy), Parameter.Code = 42600, Sample.Duration = 1 HOUR

monitor.s_noy <- monitor.s[monitor.s$Parameter.Code == 42600 & monitor.s$Sample.Duration == "1 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_noy.sf <- merge(combined_df.sf, monitor.s_noy, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_noy.pearson <- cor(monitor.s_noy.sf$county_gas, monitor.s_noy.sf$Arithmetic.Mean, method = "pearson")
auto_noy.spearman <- cor(monitor.s_noy.sf$county_gas, monitor.s_noy.sf$Arithmetic.Mean, method = "spearman")

#==========================================================================================================
#Parameter.Name = Carbon monoxide, Parameter.Code = 42101, Sample.Duration = 1 HOUR

monitor.s_co <- monitor.s[monitor.s$Parameter.Code == 42101 & monitor.s$Sample.Duration == "1 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_co.sf <- merge(combined_df.sf, monitor.s_co, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

co_data <- subset(combined_df.sf, emission_type == "CO")

auto_co.pearson <- cor(monitor.s_co.sf$emission_value, monitor.s_co.sf$Arithmetic.Mean, method = "pearson")
auto_co.spearman <- cor(monitor.s_co.sf$emission_value, monitor.s_co.sf$Arithmetic.Mean, method = "spearman")

#==========================================================================================================
#Parameter.Name = Benzene, Parameter.Code = 45201, Sample.Duration = 24 HOUR

monitor.s_c6h6 <- monitor.s[monitor.s$Parameter.Code == 45201 & monitor.s$Sample.Duration == "24 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_c6h6.sf <- merge(combined_df.sf, monitor.s_c6h6, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_c6h6.pearson <- cor(monitor.s_c6h6.sf$county_gas, monitor.s_c6h6.sf$Arithmetic.Mean, method = "pearson")
auto_c6h6.spearman <- cor(monitor.s_c6h6.sf$county_gas, monitor.s_c6h6.sf$Arithmetic.Mean, method = "spearman")

#==========================================================================================================
#Parameter.Name = Sulfur dioxide, Parameter.Code = 42401, Sample.Duration = 1 HOUR

monitor.s_so2 <- monitor.s[monitor.s$Parameter.Code == 42401 & monitor.s$Sample.Duration == "1 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_so2.sf <- merge(combined_df.sf, monitor.s_so2, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_so2.pearson <- cor(monitor.s_so2.sf$county_gas, monitor.s_so2.sf$Arithmetic.Mean, method = "pearson")
auto_so2.spearman <- cor(monitor.s_so2.sf$county_gas, monitor.s_so2.sf$Arithmetic.Mean, method = "spearman")

#==========================================================================================================
#Parameter.Name = Methane, Parameter.Code = 43201, Sample.Duration = 1 HOUR

monitor.s_ch6 <- monitor.s[monitor.s$Parameter.Code == 43201 & monitor.s$Sample.Duration == "1 HOUR", ]

#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_ch6.sf <- merge(combined_df.sf, monitor.s_ch6, by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_ch6.pearson <- cor(monitor.s_ch6.sf$county_gas, monitor.s_ch6.sf$Arithmetic.Mean, method = "pearson")
auto_ch6.spearman <- cor(monitor.s_ch6.sf$county_gas, monitor.s_ch6.sf$Arithmetic.Mean, method = "spearman")

#==========================================================================================================
#Parameter.Name = Total NMOC (non-methane organic compound), Parameter.Code = 43102, Sample.Duration = 24 HOUR

monitor.s_nmoc <- monitor.s[monitor.s$Parameter.Code == 43102 & monitor.s$Sample.Duration == "24 HOUR", ]

hc_data <- subset(combined_df.sf, emission_type == "HC")
#merge 2010 PM2.5 measurement with total exposure dataset
monitor.s_nmoc.sf <- merge(combined_df.sf, monitor.s_nmoc , by.x = c("statefp10", "countyfp10"), by.y = c("State.Code", "County.Code"))

auto_nmoc.pearson <- cor(monitor.s_nmoc.sf$emission_value, monitor.s_nmoc.sf$Arithmetic.Mean, method = "pearson")
auto_nmoc.spearman <- cor(monitor.s_nmoc.sf$emission_value, monitor.s_nmoc.sf$Arithmetic.Mean, method = "spearman")

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

auto_omi.pearson <- cor(model_omi.sf$county_gas, model_omi.sf$no2_ppb, method = "pearson")
auto_omi.spearman <- cor(model_omi.sf$county_gas, model_omi.sf$no2_ppb, method = "spearman")

#==========================================================================================================
#V5GGL04-pm25
  

satellite.pm25 <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/2010_Measurement/satellite/V5GGL04-pm25_2010_county_NHGIS.csv")

satellite.pm25 <- satellite.pm25 %>%
  mutate(statefp10 = sprintf("%02d", as.numeric(statefp10)))

satellite.pm25 <- satellite.pm25 %>%
  mutate(countyfp10 = sprintf("%03d", as.numeric(countyfp10)))

#merge 2010 model exposure with county shape file
model_pm.sf <- merge(satellite.pm25, combined_df.sf, 
                   by = c("statefp10", "countyfp10"))

auto_pm.pearson <- cor(model_pm.sf$county_gas, model_pm.sf$pm25, method = "pearson")
auto_pm.spearman <- cor(model_pm.sf$county_gas, model_pm.sf$pm25, method = "spearman")



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

auto_Un_OC_TOR.pearson <- cor(Unadjusted_total_OC_TOR.dt$county_gas, Unadjusted_total_OC_TOR.dt$Exposure, method = "pearson")
auto_Un_OC_TOR.spearman <- cor(Unadjusted_total_OC_TOR.dt$county_gas, Unadjusted_total_OC_TOR.dt$Exposure, method = "spearman")
  
  
Unadjusted_total_OC_TOT.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Unadjusted_total_OC_TOT", ]

auto_Un_OC_TOT.pearson <- cor(Unadjusted_total_OC_TOT.dt$county_gas, Unadjusted_total_OC_TOT.dt$Exposure, method = "pearson")
auto_Un_OC_TOT.spearman <- cor(Unadjusted_total_OC_TOT.dt$county_gas, Unadjusted_total_OC_TOT.dt$Exposure, method = "spearman")


Adjusted_total_OC_TOT.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Adjusted_total_OC_TOT", ]

auto_Ad_OC_TOT.pearson <- cor(Adjusted_total_OC_TOT.dt$county_gas, Adjusted_total_OC_TOT.dt$Exposure, method = "pearson")
auto_Ad_OC_TOT.spearman <- cor(Adjusted_total_OC_TOT.dt$county_gas, Adjusted_total_OC_TOT.dt$Exposure, method = "spearman")  
  
  
Adjusted_total_OC_TOR.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Adjusted_total_OC_TOR", ]

auto_Ad_OC_TOR.pearson <- cor(Adjusted_total_OC_TOR.dt$county_gas, Adjusted_total_OC_TOR.dt$Exposure, method = "pearson")
auto_Ad_OC_TOR.spearman <- cor(Adjusted_total_OC_TOR.dt$county_gas, Adjusted_total_OC_TOR.dt$Exposure, method = "spearman")  





  
#===========================================================================================================
#EC


Unadjusted_total_EC_TOR.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Unadjusted_total_EC_TOR", ]

auto_Un_EC_TOR.pearson <- cor(Unadjusted_total_EC_TOR.dt$county_gas, Unadjusted_total_EC_TOR.dt$Exposure, method = "pearson")
auto_Un_EC_TOR.spearman <- cor(Unadjusted_total_EC_TOR.dt$county_gas, Unadjusted_total_EC_TOR.dt$Exposure, method = "spearman")
  
  
Unadjusted_total_EC_TOT.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Unadjusted_total_EC_TOT", ]

auto_Un_EC_TOT.pearson <- cor(Unadjusted_total_EC_TOT.dt$county_gas, Unadjusted_total_EC_TOT.dt$Exposure, method = "pearson")
auto_Un_EC_TOT.spearman <- cor(Unadjusted_total_EC_TOT.dt$county_gas, Unadjusted_total_EC_TOT.dt$Exposure, method = "spearman")


Total_EC_TOR.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Total_EC_TOR", ]

auto_Total_EC_TOR.pearson <- cor(Total_EC_TOR.dt$county_gas, Total_EC_TOR.dt$Exposure, method = "pearson")
auto_Total_EC_TOR.spearman <- cor(Total_EC_TOR.dt$county_gas, Total_EC_TOR.dt$Exposure, method = "spearman")  
  
  
Total_EC_TOT.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Total_EC_TOT", ]

auto_Total_EC_TOT.pearson <- cor(Total_EC_TOT.dt$county_gas, Total_EC_TOT.dt$Exposure, method = "pearson")
auto_Total_EC_TOT.spearman <- cor(Total_EC_TOT.dt$county_gas, Total_EC_TOT.dt$Exposure, method = "spearman")   
  
  
#===========================================================================================================
#Total Sulfur
  
Total_S_STP.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Total_S_STP", ]

auto_Total_S_STP.pearson <- cor(Total_S_STP.dt$county_gas, Total_S_STP.dt$Exposure, method = "pearson")
auto_Total_S_STP.spearman <- cor(Total_S_STP.dt$county_gas, Total_S_STP.dt$Exposure, method = "spearman")
    
  
Total_S_LC.dt <- total_measurement_auto.sf[total_measurement_auto.sf$pollutant == "Total_S_LC", ]

auto_Total_S_LC.pearson <- cor(Total_S_LC.dt$county_gas, Total_S_LC.dt$Exposure, method = "pearson")
auto_Total_S_LC.spearman <- cor(Total_S_LC.dt$county_gas, Total_S_LC.dt$Exposure, method = "spearman")  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  
  
  
  
  
  
  
  
  
  
