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
#Fe
#==========================================================================================================
subset_fe <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "Fe", ]

nrow(subset_fe)

intersection_fe <- st_join(subset_fe, combined_df.sf, join = st_intersects)

intersection_fe <- na.omit(intersection_fe)

evaluat_auto_fe <- calculate_metrics(intersection_fe, 'county_gas', 'Mean_Val')


#==========================================================================================================
#Zn
#==========================================================================================================
subset_zn <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "Zn", ]

nrow(subset_zn)

intersection_zn <- st_join(subset_zn, combined_df.sf, join = st_intersects)

intersection_zn <- na.omit(intersection_zn)

evaluat_auto_zn <- calculate_metrics(intersection_zn, 'county_gas', 'Mean_Val')


#==========================================================================================================
#NO3
#==========================================================================================================
subset_no3 <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "NO3", ]

nrow(subset_no3)

intersection_no3 <- st_join(subset_no3, combined_df.sf, join = st_intersects)

intersection_no3 <- na.omit(intersection_no3)

evaluat_auto_no3 <- calculate_metrics(intersection_no3, 'county_gas', 'Mean_Val')


#==========================================================================================================
#OC
#==========================================================================================================
subset_oc <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "EC.88", ]

nrow(subset_oc)

intersection_oc <- st_join(subset_oc, combined_df.sf, join = st_intersects)

intersection_oc <- na.omit(intersection_oc)

evaluat_auto_oc <- calculate_metrics(intersection_oc, 'county_gas', 'Mean_Val')



#==========================================================================================================
#EC
#==========================================================================================================
subset_ec <- mean_vals.trans.sf[mean_vals.trans.sf$CompName == "OC.88", ]

nrow(subset_ec)

intersection_ec <- st_join(subset_ec, combined_df.sf, join = st_intersects)

intersection_ec <- na.omit(intersection_ec)

evaluat_auto_ec <- calculate_metrics(intersection_ec, 'county_gas', 'Mean_Val')











































