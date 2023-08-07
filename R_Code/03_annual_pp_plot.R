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

#sum the exposure by same x and y
total_exposure.sum <- aggregate(.~x+y,data=total_exposure.dt,FUN=sum)

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

#keep the state & county name and geometry info
geo_county <- roadiness.trans.county[, c("NHGISNAM","geometry", "STATENAM")]

coal_county <-
    raster::extract( total_exposure.r$exp_coal, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

gas_county <-
    raster::extract( total_exposure.r$exp_gas, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

petroleum_county <-
    raster::extract( total_exposure.r$exp_petroleum, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

renewable_county <-
    raster::extract( total_exposure.r$exp_renewable, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
 i_link.dt <- 
    data.table( as.data.table( geo_county),
                coal= coal_county[,1],
                gas = gas_county[,1],
                petroleum = petroleum_county[,1],
                renewable= renewable_county[,1])

pp_exposure.m  <-
    as.data.table( i_link.dt ) %>%
    melt( id.vars = c('geometry', 'NHGISNAM', 'STATENAM'),
          variable.name = 'fuel_type',
          value.name = 'hyads_exp')

###############################################################
#read the facility locations and plot them on the map
###############################################################

unit.facility.c <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/facility_location.csv")

unit.facility.c <- subset(unit.facility.c, select = -c(X) )
#change the datatable format
unit.facility.m  <-
    as.data.table( unit.facility.c) %>%
    melt( id.vars = c('lon', 'lat', 'Height', 'year', 'uID', 'ID'),
          variable.name = 'fuel_type',
          value.name = 'capacity')

unit.facility.m <- unit.facility.m[unit.facility.m$capacity>0,]

#turn the lon and lat into geometry point
facility.sf <- st_as_sf(unit.facility.m, coords = c("lon","lat"),
                        crs='+init=epsg:4326 +proj=longlat +ellps=WGS84
                       +datum=WGS84 +no_defs +towgs84=0,0,0',
                        agr = "constant") %>%
   st_transform( st_crs( p4s))

#rescale to control the points' size
point.rescale <- rescale(facility.sf$capacity, to = c(1, 5))

facility.sf$resecale_capacity <- point.rescale

###############################################################
# download some US data
###############################################################
states <- USAboundaries::us_states() %>% st_transform(crs = crs( p4s))

###############################################################
# plot the hyads exposure for different fuel type               
###############################################################
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
  scale_fill_viridis( limits = c( 0, 200000), 
                     breaks = c( 0, 100000, 200000),
                     labels = c( '0.0', '100000', '200000'),
                     oob = scales::squish) +
  scale_color_viridis( limits = c( 0, 200000),
                     breaks = c( 0, 100000, 200000),
                     labels = c( '0.0', '100000', '200000'),
                      oob = scales::squish) +
  facet_wrap( . ~ fuel_type, ncol = 2) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  theme_minimal() +
  theme( axis.title = element_text( size = 20),
         axis.text = element_blank(),
         legend.position = 'bottom',
         strip.text = element_text( size = 20))

ggsave('/scratch/xshan2/R_Code/disperseR/pp_coal_1951_fuel.pdf')

###############################################################
# normalize the hyads exposure             
###############################################################
#normalize
MEAN <- mean(pp_exposure.m$hyads_exp)

SD <- sd(pp_exposure.m$hyads_exp)

pp_exposure.m$normalize <- (pp_exposure.m$hyads_exp - MEAN)/SD

# plot the PM2.5 concentration for 11 models               
ggplot( ) +
  # add state coundaries
 # add the disperser grid
  geom_sf( data = pp_exposure.m,
           aes( fill =  normalize, geometry = geometry),
           color = NA) +
  # change the fill & color scale
  scale_fill_viridis(  name = "normalize hyads exposure",
                     limits = c( -1, 6), 
                     breaks = c( -1, 3, 6),
                     labels = c( '-1', '3', '6'),
                     oob = scales::squish) +
  scale_color_viridis(  name = "normalize hyads exposure",
                     limits = c( -1, 6), 
                     breaks = c( -1, 3, 6),
                     labels = c( '-1', '3', '6'),
                     oob = scales::squish) +
  facet_wrap( . ~ fuel_type, ncol = 2) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  # ggtitle("Mean & SD of 11 models")  + 
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  theme(rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20)) 

###############################################################
#if we want to plot the exposure with its facility location
###############################################################
#fuel type include Coal Gas Petroleum Renewable
petro_exp <- pp_exposure.m[pp_exposure.m$fuel_type=="petroleum",]
#coal_exp <- pp_exposure.m[pp_exposure.m$fuel_type=="coal",]
#gas_exp <- pp_exposure.m[pp_exposure.m$fuel_type=="gas",]
#renewable_exp <- pp_exposure.m[pp_exposure.m$fuel_type=="renewable",]

petro_location <- facility.sf[facility.sf$fuel_type=="Petroleum",]
#coal_location <- facility.sf[facility.sf$fuel_type=="Coal",]
#gas_location <- facility.sf[facility.sf$fuel_type=="Gas",]
#renew_location <- facility.sf[facility.sf$fuel_type=="Renewable",]

# plot the PM2.5 concentration for 11 models               
ggplot( ) +
  # add state coundaries
  geom_sf( data = states,
           aes( geometry = geometry),
           color = 'grey50',
           inherit.aes = FALSE) +
  # add the disperser grid
  geom_sf( data = petro_exp,
           aes( fill =  hyads_exp, geometry = geometry),
           color = NA) +
  geom_sf( data = petro_location,
           inherit.aes = FALSE,
           color = 'red', size = petro_location$resecale_capacity)+
  # change the fill & color scale
  scale_fill_viridis( limits = c( 0, 200000), 
                      breaks = c( 0, 100000, 200000),
                      labels = c( '0.0', '100000', '200000'),
                      oob = scales::squish) +
  scale_color_viridis( limits = c( 0, 200000),
                       breaks = c( 0, 100000, 200000),
                       labels = c( '0.0', '100000', '200000'),
                       oob = scales::squish) +
  #facet_wrap( . ~ fuel_type, ncol = 2) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  # ggtitle("Mean & SD of 11 models")  + 
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  theme(rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20))  




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


