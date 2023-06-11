library( sf)
library( data.table)
library( magrittr)
library( ggplot2)
library( raster)
library( areal)
library( dplyr)
library( viridis)
library( rlist)
library(gridExtra)
library( parallel)
library(fst)

wells1940 <-  read.csv(file = '/projects/HAQ_LAB/xshan2/R_Code/renv/Wells 1900-1940-usa.csv') # lon/lat at 176 col

# set the array_num to proceess accorting to the array task id in the SLURM array job 
#array_num <- Sys.getenv("SLURM_ARRAY_TASK_ID")
#array_num <- ifelse( array_num == '' | is.na( array_num), 1, array_num)
#message("I am working on No. ", array_num, " array job")

# steps:
# 1. create spatial object of sources
# 2. create spatial grid of receptors
#     (instead of a grid, you could use spatial political
#      borders like census tracts)
# 3. apply function to each source that:
#     a) calculates distance between source and each receptor location
#     b) gathers the output, including calculating inverse distance
# 4. if applicable, multiply inverse distance by weighting factor, such as emissions or activity metric


## =============================================================
# 1. create example spatial data set of locations
# # use lat/lon with WGS84 coordinade reference system (crs)
#  a???-if you find a lat/lon dataset and crs is not provided,
#    it's usually safe to assume WGS84
# see information here: https://docs.qgis.org/2.8/en/docs/gentle_gis_introduction/coordinate_reference_systems.html#:~:text=A%20coordinate%20reference%20system%20(CRS,real%20places%20on%20the%20earth.
## =============================================================
# create a data.table of lat/lons and years
# this would come from cohort participant locations/addresses
#pt <- 'DISPOSAL'
#CAwells1940 <- wells1940[wells1940$Production.Type==pt,]
#link_locations <- data.table(matrix(nrow = nrow(CAwells1940), ncol = 3))
#link_locations <- setnames(link_locations,c("ID","longitude","latitude"))
#link_locations$ID <- 1:nrow(CAwells1940)
#link_locations$longitude <- CAwells1940$Surface.Hole.Longitude..WGS84.
#link_locations$latitude <- CAwells1940$Surface.Hole.Latitude..WGS84.

link_locations <- data.table(matrix(nrow = nrow(wells1940), ncol = 3))
link_locations <- setnames(link_locations,c("ID","longitude","latitude"))
link_locations$ID <- 1:nrow(wells1940)
link_locations$longitude <- wells1940$Surface.Hole.Longitude..WGS84.
link_locations$latitude <- wells1940$Surface.Hole.Latitude..WGS84.


#create a spatial object from the lat/lon
# use the sf package for spatial processing
## NOTE: check the crs of your lat/lon -- i use WGS84 as an example
link_locations.sf <-
    st_as_sf( link_locations %>% filter_at(vars(longitude,latitude),all_vars(!is.na(.))) ,
              coords = c( 'longitude', 'latitude'),
              crs = 'WGS84')
  
# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
  
  
link_locations.sf.trans <-
  st_transform( link_locations.sf, crs = p4s)
  
## =============================================================
# 2. create grid of receptors
#     I'll create this over the state of TX
## =============================================================
# get TX shapefile object, transform to our crs
tx.sf <- USAboundaries::us_states() %>%
  filter(!state_abbr %in% c("PR", "AK", "HI")) %>%
  st_transform( crs = p4s)

# create a grid, use 4km resoulation
fishnet.sf <-
  st_bbox( tx.sf) %>%
  st_as_sfc() %>%
  st_make_grid( cellsize = 4000) %>%
  st_sf()

# define a receptor ID
fishnet.sf$ID_recept <- 1:nrow( fishnet.sf)

fishnet.r <- raster(xmn=-2274550, ymn=-1603146, xmx=2425450, ymx=1296854, res=4000, crs= p4s)
fishnet.r[] <- 0

link_locations <- st_coordinates( link_locations.sf.trans)

pointcount <- function(fishnet.r, link_locations){
  # make a raster of zeroes like the input
  r3 = fishnet.r
  r3[] = 0
  # get the cell index for each point and make a table:
  counts = table(cellFromXY(fishnet.r,xy = link_locations))
  # fill in the raster with the counts from the cell index:
  r3[as.numeric(names(counts))] = counts
  return(r3)
}

r3 <- pointcount(fishnet.r,link_locations)

a <- as.data.frame(rasterToPoints(r3)) 
well_num <- a[a[, "layer"]>0, ] #filter out the data

well_num$ID <- 1:nrow(well_num)


well_num.sf <-
  st_as_sf( well_num,
            coords = c( 'x', 'y'),
            crs = p4s)%>%
  st_transform( st_crs( fishnet.sf$geometry))

# make a plot with all data so far
# ggplot( ) +
#   geom_sf( data = tx.sf, fill = 'white', size = 2) +
#   geom_sf( data = fishnet.sf,
#            mapping = aes( fill = ID_recept),
#            color = 'grey50', size = .01, alpha = .9) +
#   geom_sf( data = link_locations.sf.trans,
#            mapping = aes( color = as.factor( ID))) +
#   theme_minimal() +
#   theme( legend.position = 'bottom')

## =============================================================
# 3. write/apply a function to calculate inverse distance for each source
#     define a minimum distance--here, I use 2000 (1/2 the resolution of the grid)
#     created above. There's no default for this, we should explore the sensitivity of the
#     results to this value
## =============================================================
inv_distancer <-
  function( n,
            source.sf,
            receptor.sf,
            minimum_dist = 2000){
    
    # calculate distances
    site_dist <-
      as.vector(
        st_distance( source.sf[n,],
                     receptor.sf)) #/ 1e3
    
    # minimum distance should be half resolution of grid
    site_dist[site_dist < minimum_dist] <- minimum_dist
    
    # output datset
    out <- data.table( ID = source.sf$ID[n],
                       ID_recept = receptor.sf$ID_recept,
                       inv_dist = 1 / site_dist)
    
    out[out$inv_dist<0.00005] <- NA
    out<-na.omit(out)
    
    return( out)
  }

# N_array <- 100

# N_list <- split( 1:nrow(well_num.sf), ceiling( seq_along( 1:nrow(well_num.sf))/ N_array))

exp_inverse_dist <-
 lapply(
    1:nrow(well_num.sf),
    inv_distancer,
    source.sf = well_num.sf,
    receptor.sf = fishnet.sf) %>%
  rbindlist

## =============================================================
# 4. Now, you can multiply inv. dist by some scaling factor if you have it
## =============================================================



## =============================================================
# 5. sum exposure across sources
## =============================================================
exp_inverse_dist_all <-
 exp_inverse_dist[, .( exposure = sum( inv_dist)),
                  by = ID_recept]

exp_inverse_dist_all$Production.Type <- "DISPOSAL"

# fstFileName <- paste("exp_inverse_pt_w_01_",as.numeric(array_num),'.fst', sep="")
fstFileName <- paste("exp_all_pt_1T.fst")

setwd("/scratch/xshan2/R_Code/PT/")

write.fst(exp_inverse_dist_all, fstFileName)

print("I am done!")
