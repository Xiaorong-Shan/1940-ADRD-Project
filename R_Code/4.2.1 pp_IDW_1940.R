library( ggplot2)
library( data.table)
library( viridis)
library( tidyverse)
library( sf)
library(readxl)
library(raster)
library(fst)

generator2010ret <- read_excel( '/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/december_generator2022.xlsx',
                                      sheet = 3, skip = 2) %>% data.table
generator2010act <- read_excel( '/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/december_generator2022.xlsx',
                                      sheet = 1, skip = 2) %>% data.table

#==============================================================================================================
#filter 1940

# limit to facilities operating by 1940 and not retired by 1940
cols_keep <- c( "Entity ID", "Plant ID", "Plant Name", "Plant State", "County",
                "Nameplate Capacity (MW)", "Operating Year", "Generator ID", #measure of the generator capacity Plant Name, EIA 860, annual average
                "Energy Source Code", "Latitude", "Longitude")
                
# Use backticks for column names with spaces
generator1940ret <- generator2010ret[`Operating Year` <= 1940 & `Retirement Year` > 1940, ..cols_keep]
generator1940act <- generator2010act[`Operating Year` <= 1940, ..cols_keep]
generator1940 <- rbind( generator1940ret, generator1940act)

#remove other energy
generator1940 <- generator1940 %>%
  filter(`Energy Source Code` != "PUR")

# plot total nameplate capacity by county
generator1940[ (`Energy Source Code` %in% c( 'BIT', 'SUB', 'LIG')),
               Coal := `Nameplate Capacity (MW)`]
generator1940[ (`Energy Source Code` %in% c( 'DFO', 'RFO')),
               Petroleum := `Nameplate Capacity (MW)`]
generator1940[ (`Energy Source Code` %in% c( 'BFG', 'NG', 'OG', 'PUR')),
               Gas := `Nameplate Capacity (MW)`]
generator1940[ (`Energy Source Code` %in% c( 'WAT', 'BLQ', 'WDS')),
               Renewable := `Nameplate Capacity (MW)`]

# sum by fuel type and county
fueltype_names <- c( 'Coal', 'Petroleum', 'Gas', 'Renewable')


# Summarizing data by lon, lat
generator1940_county <- generator1940[, lapply( .SD, sum, na.rm = T),
                                      .SDcols = fueltype_names, by = .(`Latitude`, `Longitude`)]
#melt the table for production_type
generator1940_county.m <-
    as.data.table(generator1940_county) %>%
    melt( id.vars = c('Longitude', 'Latitude'),
          variable.name = 'production_type',
          value.name = 'max_capacity')

# Keep only rows where max_capacity > 0
generator1940_county.m <- generator1940_county.m[max_capacity > 0]



#==============================================================================================================
#SO2 emission estimation per fuel type

# Define emission factors for different production types
emission_factors <- c("Coal" = 7.46, "Gas" = 0.1, "Petroleum" = 2.54, "Renewable" = 0.59)

# Calculate SO2_estimate and add the new column to the dataset
generator1940_county.m <- generator1940_county.m %>%
  mutate(SO2_estimate = max_capacity * 8760 * 10e-3 * sapply(production_type, function(pt) emission_factors[pt]))

# Print the updated dataset
print(generator1940_county.m)

#=============================================================================
#make hyads input
#generator1940_county.clean <- generator1940_county.m
#generator1940_county.clean$max_capacity <- NULL

#generator1940_county.c <- dcast( generator1940_county.clean,
              Longitude + Latitude ~ production_type,
              fun.aggregate = sum,
              value.var = 'SO2_estimate',
              na.rm = TRUE)

#generator1940_county.c <- na.omit(generator1940_county.c)

#write.csv(generator1940_county.c, "/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_hyads/pp_hyads_1940_input.csv")


generator1940_county.sf <- 
    st_as_sf(generator1940_county.m %>% 
             filter(!is.na(Longitude) & !is.na(Latitude)),
             coords = c('Longitude', 'Latitude'),
             crs = 'WGS84') 
 
print(generator1940_county.sf) 
#generator1940_county[, COUNTY := tolower( COUNTY)]







#==========================================================================================================
#county shape file
#==========================================================================================================
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"  

generator1940_county.sf.trans <-
  st_transform( generator1940_county.sf, crs = p4s) %>% as.data.table

dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_1940_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_1940.shp'))

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

generator1940_county.sf$ID <- 1:nrow(generator1940_county.sf)
# function to calculate inverse-distance weighting for each facility
inv_distancer <- function( n, 
                           p.sf = generator1940_county.sf, 
                           grid.sf = fishnet.sf,
                           minimum_dist = 18000){
  print( scales::percent( n / nrow( p.sf)))
  # transform crs
  grid.sf <- st_transform( grid.sf, st_crs( p.sf$geometry))
  
 # Calculate distances between point n and grid centroids
  site_dist <- as.vector(
    st_distance(p.sf[n, ]$geometry, st_centroid(grid.sf)$geometry) #/ 1e3 for kilometers if needed
  )
  

  
  # distance is 1 in same grid cell
  #site_dist[site_dist == 0] <- min( site_dist[ site_dist > 0])
   site_dist[site_dist < minimum_dist] <- minimum_dist
  
 # output datset
  out <- data.table( N = n,
                     ID = p.sf$ID[n],
                     production_type = p.sf$production_type[n],
                     SO2_estimate = p.sf$SO2_estimate[n],
                     ID_recept = grid.sf$ID_recept,
                     inv_dist = 1 / site_dist)
  
 out<-na.omit(out)
 out[ , exp_inv_dist := SO2_estimate * inv_dist]
  
  return( out)
}



## =============================================================
# 4. Now, you can multiply inv. dist by some scaling factor if you have it
## =============================================================
exp_inverse_dist <- lapply( 1:nrow( generator1940_county.sf.trans),
                            inv_distancer) %>%
  rbindlist
## =============================================================
# 5. sum exposure across sources
## =============================================================
exp_inverse_dist_all <-
 exp_inverse_dist[, .( exposure = sum( exp_inv_dist)),
                  by = c('ID_recept', 'production_type')]
                  
exp_inverse_dist_all$year <- "1940"

fstFileName <- paste("2010_1940_EF_SO2_Emission/1940_EF_SO2_Esitmate_pp_grd_36km_idw.fst")

setwd("/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/")

write.fst(exp_inverse_dist_all, fstFileName)

print("I am half done!")

exp_inverse_dist_all.sf <- 
  merge(exp_inverse_dist_all, 
        fishnet.sf, 
        by = c('ID_recept')) 


#==========================================================================================================================================
#extract it to county level
#first convert sf object to raster by using rasterFromXYZ, and then use extract function to get the value in county level
#==========================================================================================================================================

exp_inverse_dist_all.sf.t <- st_as_sf(exp_inverse_dist_all.sf)

exp_inverse_dist_all.centroids <- st_centroid(exp_inverse_dist_all.sf.t)

#exp_inverse_exposure.centroids <- exp_inverse_dist_all.centroids[, c("exposure", "geometry")]

exp_inverse_exposure_xyz <- st_coordinates(exp_inverse_dist_all.centroids) %>%
  as.data.frame() %>%
  cbind(exp_inverse_dist_all.centroids["exposure"])

exp_inverse_exposure_xyz.dt <- exp_inverse_exposure_xyz[, !names(exp_inverse_exposure_xyz) %in% "geometry", drop = FALSE]

exp_inverse_exposure_xyz.dt$production_type <-exp_inverse_dist_all.centroids$production_type

# Get unique production types
unique_production_types <- unique(exp_inverse_exposure_xyz.dt$production_type)

# Initialize an empty list for storing raster extraction results
extraction_results <- list()

setDT(exp_inverse_exposure_xyz.dt)

roadiness_dt <- as.data.table(st_set_geometry(roadiness.trans.county, NULL))


# Loop through each unique production type
for (type in unique_production_types) {
  # Filter data for the current production type
  filtered_data <- exp_inverse_exposure_xyz.dt[production_type == type, .(X, Y, exposure)]
  
  filtered_matrix <- as.matrix(filtered_data[, .(X, Y, exposure)])
  # Create raster from the filtered data
  r <- rasterFromXYZ(filtered_matrix, crs = crs(roadiness.trans.county))
  
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
  melt( id.vars = c('state', 'county', 'decade', "nhgisnam", 'nhgisst', 'nhgiscty', "icpsrst", "icpsrcty", 'icpsrnam', 'statenam', "icpsrsti", "icpsrctyi", "icpsrfip", "gisjoin", "gisjoin2",  "pid", "x_centroid", "y_centroid", "jnhgis40", "jname40", "ID"),
        variable.name = 'production_type',
        value.name = 'pp_idw')

pp_idw.dt.saved <- pp_idw.dt[, .(state, county, nhgisnam, statenam, production_type, pp_idw)]

pp_idw.dt.saved$data_type <- "pp_idw"
pp_idw.dt.saved$year <- "1940"

write.csv(pp_idw.dt.saved, "/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/2010_1940_EF_SO2_Emission/1940_pp_county_idw_EF_NHGIS.csv")

sum_pp_idw_id <- pp_idw.dt.saved %>%
  group_by(state, county) %>%
  summarise(
    sum_pp_idw = sum(pp_idw, na.rm = TRUE) # Sum exp_inverse_dist for each ID_recept, remove NA values if there are any
  ) %>%
  ungroup()
  
  
sum_pp_idw_id.sf <- 
  merge(sum_pp_idw_id, 
        roadiness.trans.county, 
        by = c('state', 'county')) 


ggplot( ) +
 # add the disperser grid
  geom_sf( data = sum_pp_idw_id.sf,
           aes( fill =  sum_pp_idw, geometry = geometry),
           size = 0.1) +
 #geom_sf(data = states, alpha = 0.1, fill = 'white', size = 1) +
 coord_sf(xlim = c(-3000000, 2500000), ylim = c(-2000000, 1500000), expand = FALSE) +
  scale_fill_viridis(name = "1940 PP IDW", # Use RdYlGn palette
                          limits = c(0, 2),
                          breaks = c(0, 1, 2),
                          labels = c('0', '1', '2'),
                          oob = scales::squish) +
    theme_minimal() +
    theme(
      rect = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'bottom',
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 20),
      strip.text.x = element_text(size = 30, face = "bold"),
      legend.key.size = unit(1, "cm"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

ggsave("/scratch/xshan2/R_Code/powerplant/pp_county_1940.pdf") 


#=============================================================================================================
#exposure file saved, prepare for normalize


sum_pp_idw_id <- pp_idw.dt.saved %>%
  group_by(state, county) %>%
  summarise(
    pp_idw = sum(pp_idw, na.rm = TRUE) # Sum exp_inverse_dist for each ID_recept, remove NA values if there are any
  ) %>%
  ungroup()


sum_pp_idw_id$production_type <- "All"


pp_idw.dt.saved <- pp_idw.dt[, .(state, county, production_type, pp_idw)]


pp_idw.dt.1940 <- bind_rows(sum_pp_idw_id, pp_idw.dt.saved)


pp_idw.dt.1940$year <- "1940"

write.csv(pp_idw.dt.1940, "/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/2010_1940_EF_SO2_Emission/1940_pp_county_idw_EIA_all_NHGIS.csv")









