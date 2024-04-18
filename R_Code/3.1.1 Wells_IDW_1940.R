library( sf)
library( data.table)
library( magrittr)
library( ggplot2)
library( raster)
library( dplyr)
library( viridis)
library(gridExtra)
library( parallel)
library(fst)


wells1940 <-  read.csv(file = '/projects/HAQ_LAB/xshan2/R_Code/renv/Wells 1900-1940-usa.csv')
#wells2010 <-  read.csv(file = '/projects/HAQ_LAB/xshan2/R_Code/renv/Wells 1900-1940-usa.csv')

link_locations <- data.table(matrix(ncol = 4, nrow = nrow(wells1940)))

# Set the names of the columns
setnames(link_locations, c("ID", "longitude", "latitude", "Production.Type"))

link_locations$ID <- 1:nrow(wells1940)

link_locations$longitude <- wells1940$Surface.Hole.Longitude..WGS84.

link_locations$latitude <- wells1940$Surface.Hole.Latitude..WGS84.

link_locations$Production.Type <- wells1940$Production.Type

#create a spatial object from the lat/lon
# use the sf package for spatial processing
## NOTE: check the crs of your lat/lon -- i use WGS84 as an example
link_locations.sf <- 
    st_as_sf(link_locations %>% 
             filter(!is.na(longitude) & !is.na(latitude)),
             coords = c('longitude', 'latitude'),
             crs = 'WGS84')
 
print(link_locations.sf) 
# we want to use an equal area projection, here's one I often use:
#p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"  

link_locations.sf.trans <-
  st_transform( link_locations.sf, crs = p4s)
  
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_1940_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_1940.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))

roadiness.trans.county$ID_recept <- 1:nrow( roadiness.trans.county)

link_locations.dt <- as.data.table(link_locations.sf.trans)
## =============================================================
# 2. create grid of receptors
#     I'll create this over the state of TX
## =============================================================

## =============================================================
# 3. write/apply a function to calculate inverse distance for each source
#     define a minimum distance--here, I use 2000 (1/2 the resolution of the grid)
#     created above. There's no default for this, we should explore the sensitivity of the
#     results to this value
## =============================================================
point_counts <- link_locations.sf.trans %>%
  st_join(roadiness.trans.county) %>%
  group_by(ID_recept, Production.Type) %>%
  summarise(
    count = n(),
  ) %>%
  ungroup()

# Compute the centroid of each grid cell
centroids <- st_centroid(st_geometry(roadiness.trans.county[point_counts$ID_recept, ]))

# Add the centroid geometry to the point_counts data frame
point_counts <- point_counts %>%
  st_set_geometry(centroids)

# Convert the point_counts sf object to a data table
point_counts_dt <- as.data.table(point_counts)

point_counts_dt <- na.omit(point_counts)

inv_distancer <- function( n, 
                           p.sf = point_counts_dt, 
                           grid.sf = roadiness.trans.county,
                           minimum_dist = 2000){
  print( scales::percent( n / nrow( p.sf)))
  # transform crs
  grid.sf <- st_transform( grid.sf, st_crs( p.sf$geometry))
  
  # calculate distances
  site_dist <- as.vector(
    st_distance(p.sf$geometry[n], 
                st_centroid(grid.sf))) 
  
  # distance is 1 in same grid cell
  site_dist[site_dist == 0] <- min( site_dist[ site_dist > 0])
  
  # output datset
  out <- data.table( N = n,
                     p.sf[n, c('Production.Type', 'count')],
                     grid.sf[, c( 'statenam', 'nhgisnam', 'state', 'county', 'geometry')],
                     inv_dist = 1 / site_dist)
 
 out[out$inv_dist<0.00005] <- NA                      
 out[ , exp_inv_dist := count * inv_dist]
  return( out)
}


## =============================================================
# 4. Now, you can multiply inv. dist by some scaling factor if you have it
## =============================================================
exp_inverse_dist <- lapply( 1:nrow( point_counts_dt),
                            inv_distancer) %>%
  rbindlist


## =============================================================
# 5. sum exposure across sources
## =============================================================
exp_inverse_dist_sum <- 
  exp_inverse_dist[, .( exposure = sum( exp_inv_dist)),
                   by = .( Production.Type, county)] %>%
  merge( roadiness.trans.county, by = c('state', 'county'))

#normalize
#MEAN <- mean(exp_inverse_dist_sum$exposure)

#SD <- sd(exp_inverse_dist_sum$exposure)

#exp_inverse_dist_sum$normalize <- (exp_inverse_dist_sum$exposure - MEAN)/SD


exp_inverse_dist_sum.g <- exp_inverse_dist_sum[,-c("geometry")]

exp_inverse_dist_sum.g$year <- "1940"

write.csv(exp_inverse_dist_sum.g, "/scratch/xshan2/R_Code/Well/well_county_1940.csv")

