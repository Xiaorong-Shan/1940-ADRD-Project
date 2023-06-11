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

# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

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

setwd("/scratch/xshan2/R_Code/PT/")

exp_inverse_dist_all <-read.fst("exp_all_pt_1T.fst")

exp_inverse_dist_all <- as.data.table(exp_inverse_dist_all)

#exp_inverse_dist_all <- subset(exp_inverse_dist_all, Proudction.Type == "OTHER" | Proudction.Type =="BRINE" )

exp_inverse_dist_all.sf <-
  merge( fishnet.sf,
         exp_inverse_dist_all,
         by = 'ID_recept')

ggplot( exp_inverse_dist_all.sf,
        aes( fill = exposure)) +
  geom_sf( size = 1, color = NA) +
  scale_fill_gradient( limits = c( 0, 0.01), high = 'red', low = 'white', oob = scales::squish) +
  geom_sf( data = tx.sf, alpha = 0.1, fill = 'white', size = 1) +
  coord_sf( xlim=c(-3000000, 2500000),ylim=c(-2000000,1500000)) +
  
  expand_limits( fill = 0) +
  theme_minimal() +
  theme( legend.position = 'bottom',
         legend.text = element_text( angle = 30)) +
  theme_bw()+
  ggtitle("Exposure for all Well Status")  + 
  theme(plot.title = element_text(size = 20, face = "bold"))

ggsave(file="/scratch/xshan2/R_Code/WS/WS-4km-AllT.pdf")