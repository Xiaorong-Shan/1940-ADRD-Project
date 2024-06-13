library( ggplot2)
library( data.table)
library( viridis)
library( tidyverse)
library( sf)
library(readxl)
library(raster)
library(fst)

generator2010 <- read_excel('/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/2010_eGRID_emission/eGRID2010_Data.xls',
                            sheet = 4, skip = 4) %>% data.table

# limit to facilities operating by 2010 and not retired by 2010
cols_keep <- c( "SEQPLT10", "OPRCODE", "PNAME", "PSTATABB", "CNTYNAME",
                "FIPSST", "FIPSCNTY","PLPFGNCT", "LAT", "LON", "PLSO2AN")

# Use backticks for column names with spaces
generator2010 <- generator2010[, ..cols_keep]

generator2010_county.sf <- 
    st_as_sf(generator2010 %>% 
             filter(!is.na(LON) & !is.na(LAT)),
             coords = c('LON', 'LAT'),
             crs = 'WGS84')

print(generator2010_county.sf) 

#==========================================================================================================
#county shape file
#==========================================================================================================
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"  

generator2010_county.sf.trans <-
  st_transform( generator2010_county.sf, crs = p4s) 

dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_2010_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_2010.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))

roadiness.trans.county$ID <- 1:nrow(roadiness.trans.county)

#create a grid
fishnet.sf <-
  st_bbox( roadiness.trans.county) %>%
  st_as_sfc() %>%
  st_make_grid( cellsize = 36000) %>%
  st_sf()

fishnet.sf$ID_recept <- 1:nrow( fishnet.sf)

grid_idw <- read.fst("/home/xshan2/HAQ_LAB/xshan2/R_Code/Roadiness/IDW_grid_shape/36km_grid_idw_all_combined.fst")

grid_idw.dt <- merge(grid_idw, fishnet.sf, by = c("ID_recept"))

grid_idw.sf <- st_as_sf(grid_idw.dt)

#============================================================================================
#spatial join two sf objects
#============================================================================================
joined_sf <- st_join(generator2010_county.sf.trans, fishnet.sf, join = st_within)

dt <- setDT(joined_sf)
# Count the number of points per grid cell
counts_dt <- dt[, .N, by = c("ID_recept", "PLPFGNCT", "PLSO2AN")] #ID_recept, fuel type and so2 emissions

counts_dt <- na.omit(counts_dt) #remove plant outside the us mainland

setnames(counts_dt, "N", "count")
setnames(counts_dt, "PLPFGNCT", "fuel_type")
setnames(counts_dt, "PLSO2AN", "so2_emiss")

counts_dt$weight <- counts_dt$so2_emiss * counts_dt$count

counts_dt.sf <- merge(counts_dt, grid_idw.sf, by = c("ID_recept"))

counts_dt.sf$exp_idw <- counts_dt.sf$weight * counts_dt.sf$exposure

# Assuming counts_dt.sf is your data.table
pp_idw.df <- counts_dt.sf[, .(ID_recept, fuel_type, exp_idw)]

pp_idw.df$year <- "2010"

#write.fst(pp_idw.df, "/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/2010_eGRID_emission/36km_eGRID2010_pp_idw.fst")

#==========================================================================================================================================
#extract it to county level
#first convert sf object to raster by using rasterFromXYZ, and then use extract function to get the value in county level
#==========================================================================================================================================

counts_dt.sf.t <- st_as_sf(counts_dt.sf)

counts_dt.sf.centroids <- st_centroid(counts_dt.sf.t)

exp_inverse_exposure_xyz <- st_coordinates(counts_dt.sf.centroids) %>%
  as.data.frame() %>%
  cbind(counts_dt.sf.centroids["exp_idw"])

exp_inverse_exposure_xyz.dt <- exp_inverse_exposure_xyz[, !names(exp_inverse_exposure_xyz) %in% "geometry", drop = FALSE]

exp_inverse_exposure_xyz.dt$production_type <-counts_dt.sf.centroids$fuel_type

# Get unique production types
unique_production_types <- unique(exp_inverse_exposure_xyz.dt$production_type)

# Initialize an empty list for storing raster extraction results
extraction_results <- list()

setDT(exp_inverse_exposure_xyz.dt)

roadiness_dt <- as.data.table(st_set_geometry(roadiness.trans.county, NULL))

# Loop through each unique production type
for (type in unique_production_types) {
  # Filter data for the current production type
  filtered_data <- exp_inverse_exposure_xyz.dt[production_type == type, .(X, Y, exp_idw)]
  
  # Create raster from the filtered data
  r <- rasterFromXYZ(as.matrix(filtered_data[, .(X, Y, exp_idw)]), crs = crs(roadiness.trans.county))
  
  # Extract mean values from the raster based on roadiness.trans.county geometry
  extracted_values <- raster::extract(r, 
                      roadiness.trans.county, 
                      fun = mean, 
                      na.rm = TRUE, 
                      weights = TRUE, 
                      exact = FALSE)
                      
  # Add the extracted values to the results list, naming the list element after the production type
  #extraction_results[[type]] <- extracted_values
  
  # Create a temporary data.table from extracted values
  temp_dt <- data.table(ID = 1:nrow(roadiness_dt), value = extracted_values)
  
  # Name the column after the production type and merge
  setnames(temp_dt, "value.V1", type)
  roadiness_dt <- merge(roadiness_dt, temp_dt, by = "ID", all.x = TRUE)
}

pp_idw.dt  <-
  as.data.table(roadiness_dt) %>%
  melt( id.vars = c('statefp10', 'countyfp10', 'countyns10', "geoid10", 'name10', 'namelsad10', "lsad10", "classfp10", 'mtfcc10', 'csafp10', "cbsafp10", "metdivfp10", "funcstat10", "aland10", "awater10",  "intptlat10", "intptlon10", "gisjoin", "shape_area", "shape_len", "jfips10", "jcnam10","ID"),
        variable.name = 'production_type',
        value.name = 'pp_idw')

pp_idw.dt.saved <- pp_idw.dt[, .(statefp10, countyfp10, name10, production_type, pp_idw)]

pp_idw.dt.saved$data_type <- "pp_idw"
pp_idw.dt.saved$year <- "2010"

write.csv(pp_idw.dt.saved, "/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/2010_pp_county_idw_eGRID_NHGIS.csv")






