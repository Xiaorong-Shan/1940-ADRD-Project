library(ggplot2)
library(USAboundariesData)
library(pryr)
library(fst)
library(data.table)
library(sf)
library(viridis)
library(raster)

#read all fst files together
exp_dir <- "/projects/HAQ_LAB/xshan2/disperseR_Linux/main/output/hyads"

p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

files <- list.files(exp_dir,
                    pattern = '*.fst',
                    full.names = TRUE) 

df_list <- rep(list(NA), length(files))

for (i in seq_along(files)){
  df_list[[i]] <- fst::read_fst(files[i], as.data.table=T)

}

combine_fst <- do.call(rbind, df_list)

# write.fst(combine_fst, "/projects/HAQ_LAB/xshan2/disperseR_Linux/main/output/hyads/pp_exp_1951_com.fst")

total_exposure.dt<- subset(combine_fst, select = -c(N) )

# create raster object
total_exposure.r <- rasterFromXYZ( total_exposure.dt,
                                 crs = p4s)


###############################################################
#read the county shp file and extract exposure to county level
###############################################################
#read the county shp file
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/nhgis0001_shapefile_tl2008_us_county_1940"

# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'US_county_1940_conflated.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))

roadiness.trans.county <- na.omit(roadiness.trans.county)

roadiness.trans.county <- roadiness.trans.county[, c("NHGISNAM","geometry")] #only keep county name and geometry

countyname <- unique(roadiness.trans.county$NHGISNAM)

coal_county <-
    raster::extract( total_exposure.r$exp_coal, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

gas_county <-
    raster::extract( total_exposure.r$exp_gas, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

petroleum_county <-
    raster::extract( total_exposure.r$exp_petroleum, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

renewable_county <-
    raster::extract( total_exposure.r$exp_renewable, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
 i_link.dt <- 
    data.table( as.data.table( roadiness.trans.county),
                coal= coal_county[,1],
                gas = gas_county[,1],
                petroleum = petroleum_county[,1],
                renewable= renewable_county[,1])

pp_exposure.dt <- subset(i_link.dt, select = -c(NHGISNAM) )

#transpose the table
pp_exposure.m  <-
    as.data.table(pp_exposure.dt ) %>%
    melt( id.vars = 'geometry',
          variable.name = 'fuel_type',
          value.name = 'hyads_exp')

# download some US data
states <- USAboundaries::us_states() %>% st_transform(crs = crs( total_exposure.r))

# plot the PM2.5 concentration for 11 models               
ggplot( ) +
  # add state coundaries
  geom_sf( data = states,
           aes( geometry = geometry),
           color = 'grey50',
           inherit.aes = FALSE) +
 # add the disperser grid
  geom_sf( data = pp_exposure.m,
           aes( fill =  hyads_exp, geometry = geometry),
           color = NA) +
  # change the fill & color scale
  scale_fill_viridis( limits = c( 0, 10), oob = scales::squish) +
  scale_color_viridis( limits = c( 0, 10), oob = scales::squish) +
  facet_wrap( . ~ fuel_type, ncol = 3) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  theme_minimal() +
  theme( axis.title = element_text( size = 20),
         axis.text = element_blank(),
         strip.text = element_text( size = 20))

ggsave('/scratch/xshan2/R_Code/disperseR/pp_coal_1951_fuel.pdf')

##extract the exposure you want to plot
#pp_exp_coal <- combine_fst[ ,c('x', 'y', 'exp_coal')]

##Go through each row and determine if a value is zero
#row_sub = apply(pp_exp_coal, 1, function(row) all(row !=0 ))
##Subset as usual, remove all 0 values
#pp_exp_coal <- pp_exp_coal[row_sub,]

# create raster object
#pp_exp_coal.r <- rasterFromXYZ( pp_exp_coal,
#                                 crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")

# create sf polygon object
#pp_exp_coal.sp <- rasterToPolygons( pp_exp_coal.r)
#pp_exp_coal.sf <- st_as_sf( pp_exp_coal.sp)


# plot the exposure                
#ggplot( ) +
 # add the disperser grid
#  geom_sf( data = pp_exp_coal.sf,
#           aes( fill = exp_coal, geometry = geometry),
#           color = NA) +
  # add state coundaries
#  geom_sf( data = states,
#          aes( geometry = geometry),
#            fill = NA, color = 'grey50',
#           inherit.aes = FALSE) +
  # change the fill & color scale
#  scale_fill_viridis( limits = c( 0, 60), oob = scales::squish) +
#  scale_color_viridis( limits = c( 0, 60), oob = scales::squish) +
  # be sure to show 0 in the color scales
#  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
#  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
#  theme_minimal() +
#  theme( axis.title = element_text( size = 20),
#         axis.text = element_blank(),
#         strip.text = element_text( size = 20))
