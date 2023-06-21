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

files <- list.files(exp_dir,
                    pattern = '*.fst',
                    full.names = TRUE) 

df_list <- rep(list(NA), length(files))

for (i in seq_along(files)){
  df_list[[i]] <- fst::read_fst(files[i], as.data.table=T)

}

combine_fst <- do.call(rbind, df_list)

write.fst(combine_fst, "/projects/HAQ_LAB/xshan2/disperseR_Linux/main/output/hyads/pp_exp_1951_com.fst")

#extract the exposure you want to plot
pp_exp_coal <- combine_fst[ ,c('x', 'y', 'exp_coal')]

##Go through each row and determine if a value is zero
row_sub = apply(pp_exp_coal, 1, function(row) all(row !=0 ))
##Subset as usual, remove all 0 values
pp_exp_coal <- pp_exp_coal[row_sub,]

# create raster object
pp_exp_coal.r <- rasterFromXYZ( pp_exp_coal,
                                 crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
                                 
# create sf polygon object
pp_exp_coal.sp <- rasterToPolygons( pp_exp_coal.r)
pp_exp_coal.sf <- st_as_sf( pp_exp_coal.sp)

# download some US data
states <- USAboundaries::us_states()

pp_exp_coal.sf$geometry <-
  st_transform( pp_exp_coal.sf$geometry,
                crs = st_crs( states))

# plot the exposure                
ggplot( ) +
 # add the disperser grid
  geom_sf( data = pp_exp_coal.sf,
           aes( fill = exp_coal, geometry = geometry),
           alpha = .75, color = NA) +
  # add state coundaries
  geom_sf( data = states,
           aes( geometry = geometry),
           alpha = .5, color = 'grey50',
           inherit.aes = FALSE) +
  # change the fill & color scale
  scale_fill_viridis( limits = c( 0, 60), oob = scales::squish) +
  scale_color_viridis( limits = c( 0, 60), oob = scales::squish) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # set boundaries over mainland of US
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  # set thematic elements
  theme_minimal() +
  theme( axis.title = element_text( size = 20),
         axis.text = element_blank(),
         strip.text = element_text( size = 20))

ggsave('/projects/HAQ_LAB/xshan2/R_Code/powerplant/pp_coal_1951.pdf')
