library( raster)
library( sf)
library( magrittr)
library( USAboundaries)
library( data.table)
library( ggplot2)
library( viridis)
library(ncdf4)
library(dplyr)

p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"  

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
            names_2010 <- grep( '2010', names( raster_in), value = TRUE) #only take the 2010 vector from raster
            raster_2010 <- raster_in[[ names_2010]]
            
            raster_2010_mean <- mean( raster_2010) 
            names( raster_2010_mean) <- name_in #change the layer name to dataset
            print( raster_2010_mean)
            
            #just print some info
            message( paste( 'Dimension of', name_in, 'is'))
            print( dim( raster_in))
            
            # return the dimentions
            return( raster_2010_mean)
          })

# combine all into brick: 11 layers in total
raster2010_brick <- 
  brick( file_dims)
  
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_2010_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_2010.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(raster2010_brick))


#extract the PM2.5 concentration for each model (11 in total)
  Con_BCC.ESM1 <- 
    raster::extract( raster2010_brick$BCC.ESM1, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_CESM2.WACCM <- 
    raster::extract( raster2010_brick$CESM2.WACCM, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)

  Con_CNRM.ESM2.1 <- 
    raster::extract( raster2010_brick$CNRM.ESM2.1, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_GFDL.ESM4 <- 
    raster::extract( raster2010_brick$GFDL.ESM4, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_GISS.E2.1.G <- 
    raster::extract( raster2010_brick$GISS.E2.1.G, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_HadGEM3.GC31.LL <- 
    raster::extract( raster2010_brick$HadGEM3.GC31.LL, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
   
  Con_MIROC.ES2L <- 
    raster::extract( raster2010_brick$MIROC.ES2L, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_MPI.ESM.1.2.HAM <- 
    raster::extract( raster2010_brick$MPI.ESM.1.2.HAM, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_MRI.ESM2.0 <- 
    raster::extract( raster2010_brick$MRI.ESM2.0, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_NorESM2.LM <- 
    raster::extract( raster2010_brick$NorESM2.LM, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
  Con_UKESM1.0.LL <- 
    raster::extract( raster2010_brick$UKESM1.0.LL, 
                     roadiness.trans.county,
                     fun = mean,
                     na.rm= TRUE,
                     weights = TRUE,
                     exact = FALSE)
  
# create a data table of the results
  i_link.dt <- 
    data.table( as.data.table( roadiness.trans.county),
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
                
#st_write(i_link.dt, "/projects/HAQ_LAB/xshan2/R_Code/Exposure_products/county_historical_CIMP6/county_1940_CIMP6.shp")

i_link.dt_selected <- subset(i_link.dt, select = c("Con_BCC.ESM1", "Con_CESM2.WACCM", "Con_CNRM.ESM2.1", "Con_GFDL.ESM4",
         "Con_GISS.E2.1.G", "Con_HadGEM3.GC31.LL", "Con_MIROC.ES2L", "Con_MPI.ESM.1.2.HAM", "Con_MRI.ESM2.0", "Con_NorESM2.LM",
         "Con_UKESM1.0.LL", 'statefp10', "countyfp10", "name10", "geometry"))
         
#transpose the table
CMIP6.Con.m  <-
    as.data.table(i_link.dt_selected) %>%
    melt( id.vars = c('statefp10', "countyfp10", "name10", "geometry"),
          variable.name = 'con_model',
          value.name = 'PM2.5_con')

CMIP6.Con.m.sf <- st_as_sf(CMIP6.Con.m)
CMIP6.Con.m.trans <- st_transform(CMIP6.Con.m.sf,
                                  crs = crs(p4s))
cmip6_county_11 <- as.data.table(st_drop_geometry(CMIP6.Con.m.trans))
cmip6_county_11$decade <- "2010"
cmip6_county_11$data_type <- "CMIP6"
write.csv(cmip6_county_11, "/home/xshan2/HAQ_LAB/xshan2/CIMP6_input_1940/cmip6_2010_county_NHGIS.csv")

print("I am done")
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
  theme_bw()+
  labs( fill = expression( PM['2.5']*', µg'~m^{'-3'}),
        color= expression( PM['2.5']*', µg'~m^{'-3'}))+
  guides( fill = 'none') +
  # ggtitle("Mean & SD of 11 models")  + 
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  theme(rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20)) 

ggsave("/scratch/xshan2/R_Code/Roadiness/CMIP6_11model_con2.pdf")


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
  facet_wrap( . ~ value) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  theme_bw()+
  labs( fill = expression( PM['2.5']*', µg'~m^{'-3'}),
        color= expression( PM['2.5']*', µg'~m^{'-3'}))+
  guides( fill = 'none') +
  # ggtitle("Mean & SD of 11 models")  + 
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  theme(rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20)) 

ggsave("/scratch/xshan2/R_Code/Roadiness/CMIP6_mean_sd_con.pdf")

## ============================================================================= ##
##save the file, IDW in csv and geometry in shp, then find way to combine thems
## ============================================================================= ##
cmip6_county_11 <- CMIP6.Con.m[, -c("geometry")]
write.csv(cmip6_county_11, "/scratch/xshan2/R_Code/CMIP6/cmip6_1940_county_11_models.csv")

cmip6_county_geo <- 
  cmip6_county_11 %>%
  merge( roadiness.trans.county, by = c('NHGISNAM', 'STATENAM'))
  
