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

 result.sf <- merge(result, roadiness.trans.county.select, 
                   by = c("statefp10", "countyfp10")) %>% st_as_sf()

#==========================================================================================================
#Read CSN_2010 measurement file
#==========================================================================================================
CSN_2010 <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/2010_Measurement/Intp_IMPROVE_CSN/CSN_data_for_analysis_10232022.csv")

# Convert CSN_2010 to a data.table if it's not already one
setDT(CSN_2010)

# Create a unique list of SiteCode, Latitude, and Longitude
Site_location_list <-  unique(CSN_2010[, .(SiteCode, Latitude, Longitude)], by = c("SiteCode", "Latitude", "Longitude"))

# Now perform the operation
mean_vals <- CSN_2010[, .(Mean_Val = mean(Val, na.rm = TRUE)), by = .(SiteCode, CompName, Unit)]

mean_vals.lonlat <- merge(mean_vals, Site_location_list, by = "SiteCode")

mean_vals.sf <- st_as_sf(mean_vals.lonlat, coords = c("Longitude", "Latitude"), crs = 4326)

#convert the crs to p4s
mean_vals.trans.sf<- st_transform(mean_vals.sf, crs = p4s)

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
  #rmse_val <- sqrt(sum(squared_diffs) / length(observed)) 
  #mb_val <- mean(predicted - observed)/ length(observed)
  #nmb_val <- sum(predicted - observed) / sum(observed) 
  #nme_val <- sum(abs(predicted - observed)) / sum(observed) 
  pearson_r <- round(cor(observed, predicted, method = "pearson"), 2)
  spearman_r <- round(cor(observed, predicted, method = "spearman"), 2)
  r_squared <- round(summary(lm(predicted ~ observed))$r.squared, 2)
  
  # Return a list of metrics
 return(list(
    #RMSE = round(rmse_val, 2),
    #MB = round(mb_val, 2),
    #NMB = round(nmb_val, 2),
    #NME = round(nme_val, 2),
    Pearson_R = pearson_r,
    Spearman_R = spearman_r,
    R_squared = r_squared
  ))
}

#==========================================================================================================
#S
#==========================================================================================================
subset_s <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "S", ]

nrow(subset_s)

intersection_s <- st_join(subset_s, result.sf, join = st_intersects)

intersection_s <- na.omit(intersection_s)

evaluat_well_idw_s <- calculate_metrics(intersection_s, 'sum_well_idw', 'Mean_Val')

print(evaluat_well_idw_s)

#==========================================================================================================
#PM2.5
#==========================================================================================================
subset_pm <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "Accept.PM2.5", ]

nrow(subset_pm)

intersection_pm <- st_join(subset_pm, result.sf, join = st_intersects)

intersection_pm <- na.omit(intersection_pm)

evaluat_well_idw_pm <- calculate_metrics(intersection_pm, 'sum_well_idw', 'Mean_Val')

print(evaluat_well_idw_pm)


#==========================================================================================================
#SO4
#==========================================================================================================
subset_so4 <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "SO4", ]

nrow(subset_so4)

intersection_so4 <- st_join(subset_so4, result.sf, join = st_intersects)

intersection_so4 <- na.omit(intersection_so4)

evaluat_well_idw_so4 <- calculate_metrics(intersection_so4, 'sum_well_idw', 'Mean_Val')

print(evaluat_well_idw_so4)


#==========================================================================================================
#NH4+
#==========================================================================================================
subset_nh4 <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "NH4+", ]

nrow(subset_nh4)

intersection_nh4 <- st_join(subset_nh4, result.sf, join = st_intersects)

intersection_nh4 <- na.omit(intersection_nh4)

evaluat_well_idw_nh4 <- calculate_metrics(intersection_nh4, 'sum_well_idw', 'Mean_Val')

print(evaluat_well_idw_nh4)


#==========================================================================================================
#NO3
#==========================================================================================================
subset_no3 <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "NO3", ]

nrow(subset_no3)

intersection_no3 <- st_join(subset_no3, result.sf, join = st_intersects)

intersection_no3 <- na.omit(intersection_no3)

evaluat_well_idw_no3 <- calculate_metrics(intersection_no3, 'sum_well_idw', 'Mean_Val')

print(evaluat_well_idw_no3)








































                   
