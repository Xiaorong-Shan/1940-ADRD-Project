library( raster)
library( sf)
library( magrittr)
library( USAboundaries)
library( data.table)
library( ggplot2)
library( viridis)
library(ncdf4)
library(dplyr)

p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

files_all <- list.files( "/projects/HAQ_LAB/xshan2/CIMP6_input_1940/HistorialConcentration_data/",
                         pattern = 'Monthly.*.nc',
                         full.names = TRUE)

file_dims <- 
  lapply( files_all,
          function( f){
            print( f)
            
            # isolate the name
            name_in <- gsub( '.*concentrations_from_|_in_historical.*', '', f)
            print( name_in)
            
            # read in the raster
            raster_in <- brick( f) %>% rotate()
            names_1940 <- grep( '1940', names( raster_in), value = TRUE) #only take the 1940 vector from raster
            raster_1940 <- raster_in[[ names_1940]]
            
            raster_1940_mean <- mean( raster_1940) 
            names( raster_1940_mean) <- name_in #change the layer name to dataset
            print( raster_1940_mean)
            
            #just print some info
            message( paste( 'Dimension of', name_in, 'is'))
            print( dim( raster_in))
            
            # return the dimentions
            return( raster_1940_mean)
          })

# combine all into brick: 11 layers in total
raster1940_brick <- 
  brick( file_dims)
  
#read the county shp file
#dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/nhgis0035_shapefile_tl2000_us_tract_1940"
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/nhgis0001_shapefile_tl2008_us_county_1940"

##This shp file has lots of missing data
#roadiness_county <-
#    st_read( file.path( dir.in, 'US_tractcounty_1940.shp'))
 
#this shp file has the complete whole US cencus
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'US_county_1940_conflated.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))

roadiness.trans.county <- na.omit(roadiness.trans.county)

geo_county <- roadiness.trans.county[, c("NHGISNAM","geometry", "STATENAM")] #only keep county name and geometry

#extract the PM2.5 concentration for each model (11 in total)
  Con_BCC.ESM1 <- 
    raster::extract( raster1940_brick$BCC.ESM1, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_CESM2.WACCM <- 
    raster::extract( raster1940_brick$CESM2.WACCM, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

  Con_CNRM.ESM2.1 <- 
    raster::extract( raster1940_brick$CNRM.ESM2.1, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_GFDL.ESM4 <- 
    raster::extract( raster1940_brick$GFDL.ESM4, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_GISS.E2.1.G <- 
    raster::extract( raster1940_brick$GISS.E2.1.G, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_HadGEM3.GC31.LL <- 
    raster::extract( raster1940_brick$HadGEM3.GC31.LL, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
   
  Con_MIROC.ES2L <- 
    raster::extract( raster1940_brick$MIROC.ES2L, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_MPI.ESM.1.2.HAM <- 
    raster::extract( raster1940_brick$MPI.ESM.1.2.HAM, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_MRI.ESM2.0 <- 
    raster::extract( raster1940_brick$MRI.ESM2.0, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_NorESM2.LM <- 
    raster::extract( raster1940_brick$NorESM2.LM, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_UKESM1.0.LL <- 
    raster::extract( raster1940_brick$UKESM1.0.LL, 
                     geo_county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
# create a data table of the results
  i_link.dt <- 
    data.table( as.data.table( geo_county),
                Con_BCC.ESM1= Con_BCC.ESM1[,1],
                Con_CESM2.WACCM = Con_CESM2.WACCM[,1],
                Con_CNRM.ESM2.1 = Con_CNRM.ESM2.1[,1],
                Con_GFDL.ESM4= Con_GFDL.ESM4[,1],
                Con_GISS.E2.1.G = Con_GISS.E2.1.G[,1],
                Con_HadGEM3.GC31.LL = Con_HadGEM3.GC31.LL[,1],
                Con_MIROC.ES2L= Con_MIROC.ES2L[,1],
                Con_MPI.ESM.1.2.HAM = Con_MPI.ESM.1.2.HAM[,1],
                Con_MRI.ESM2.0 = Con_MRI.ESM2.0[,1],
                Con_NorESM2.LM = Con_NorESM2.LM[,1],
                Con_UKESM1.0.LL = Con_UKESM1.0.LL[,1])
                
#st_write(i_link.dt, "/projects/HAQ_LAB/xshan2/R_Code/Exposure_products/county_historical_CIMP6/county_historical_CIMP6.shp")

#transpose the table
CMIP6.Con.m  <-
    as.data.table(i_link.dt ) %>%
    melt( id.vars = c('geometry', 'NHGISNAM', 'STATENAM'),
          variable.name = 'con_model',
          value.name = 'PM2.5_con')

# download some US data
states <- USAboundaries::us_states() %>% st_transform(crs = p4s)

# plot the PM2.5 concentration for 11 models               
ggplot( ) +
  # add state coundaries
  geom_sf( data = states,
           aes( geometry = geometry),
           color = 'grey50',
           inherit.aes = FALSE) +
 # add the disperser grid
  geom_sf( data = CMIP6.Con.m,
           aes( fill =  PM2.5_con, geometry = geometry),
           color = NA) +
  # change the fill & color scale
  scale_fill_viridis( limits = c( 0, 10), oob = scales::squish) +
  scale_color_viridis( limits = c( 0, 10), oob = scales::squish) +
  facet_wrap( . ~ con_model, ncol = 3) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  theme_minimal() +
  theme( axis.title = element_text( size = 20),
         axis.text = element_blank(),
         strip.text = element_text( size = 20))

ggsave("/scratch/xshan2/R_Code/Roadiness/CMIP6_11model_con.pdf")

## take mean and standard deviation, convert to sf

cmip6_con.dt <- subset(CMIP6.Con.dt, select = -c(geometry) )

#calculate the sd and mean
cmip6_con.dt <- transform(cmip6_con.dt, sd=apply(cmip6_con.dt, 1, sd, na.rm=TRUE))
cmip6_con.dt <- transform(cmip6_con.dt, mean=apply(cmip6_con.dt, 1, mean, na.rm=TRUE))

#create a data table for mean and sd
cmip6_mean_sd.dt<-data.frame(matrix(ncol=3,nrow=nrow(CMIP6.Con.dt)))
colnames(cmip6_mean_sd.dt)<-c('geometry','mean','sd')

cmip6_mean_sd.dt$geometry <- CMIP6.Con.dt$geometry
cmip6_mean_sd.dt$mean <- cmip6_con.dt$mean
cmip6_mean_sd.dt$sd <- cmip6_con.dt$sd

#transpose the table
cmip6_mean_sd.m  <-
    as.data.table(cmip6_mean_sd.dt ) %>%
    melt( id.vars = 'geometry',
          variable.name = 'value',
          value.name = 'PM2.5_con')

# plot the PM2.5 concentration for 11 models               
ggplot( ) +
  # add state coundaries
  geom_sf( data = states,
           aes( geometry = geometry),
           color = 'grey50',
           inherit.aes = FALSE) +
 # add the disperser grid
  geom_sf( data = cmip6_mean_sd.m,
           aes( fill =  PM2.5_con, geometry = geometry),
           color = NA) +
  # change the fill & color scale
  scale_fill_viridis( limits = c( 0, 10), oob = scales::squish) +
  scale_color_viridis( limits = c( 0, 10), oob = scales::squish) +
  facet_wrap( . ~ value, ncol = 3) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  theme_minimal() +
  theme( axis.title = element_text( size = 20),
         axis.text = element_blank(),
         strip.text = element_text( size = 20))

ggsave("/scratch/xshan2/R_Code/Roadiness/CMIP6_mean_sd_con.pdf")

## ============================================================================= ##
##save the file, IDW in csv and geometry in shp, then find way to combine thems
## ============================================================================= ##
cmip6_county_11 <- CMIP6.Con.m[, -c("geometry")]
write.csv(cmip6_county_11, "/scratch/xshan2/R_Code/CMIP6/cmip6_councentration_county_11_models.csv")

cmip6_county_geo <- 
  cmip6_county_11 %>%
  merge( roadiness.trans.county, by = c('NHGISNAM', 'STATENAM'))
  



















