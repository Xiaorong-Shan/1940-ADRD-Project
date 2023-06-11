library( fst)
library( data.table)
library( USAboundaries)
library( raster)
library( sf)
library( magrittr)
library( ggplot2)

p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

# directory where hysplit is saved
hysplit_out_dir <- '/Users/xshan2/Documents/disperseR/main/output/hysplit/stack1/2018/01/'

# hysplit_out_dir <- 'C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/01_2019fall_Campfire/disperseR/hysplit/stack1/2014/02/'

# list files that correspond with given unit
hysp_files_CAMP <- 
  list.files( path = hysplit_out_dir,
              pattern = '_CAMP_',
              full.names = TRUE)

# read in all the files and combine
# in the function, retrieve the emission date
hysp_output_in <- 
  lapply( hysp_files_CAMP,
          function( filename){
            # using regular expressions, more info at ?regexp
            emission_date <- 
              gsub( '.*CAMP_|_.*\\.fst', '', filename) 
            
            # read the file
            file.in <- read.fst( filename, as.data.table = TRUE)
            
            # fix strange issue with Pdate
            file.in[, Pdate := as( Pdate, 'character')]
            
            # assign an emission date
            file.in[, emiss_date := emission_date]
          }
  ) %>% rbindlist

# limit to hours 2:23 (change for different averaging times)
hour_limits <- 0:24
hysp_output_trim <- hysp_output_in[ hour %in% hour_limits]

# merge with emissions amounts
# first, create synthetic emission table
# create emissions by date
# emiss_records <- 
#   data.table( emiss_date = 
#                 seq.Date( as.Date( '2014-01-08'), as.Date('2014-01-17'), 'day') %>%
#                 as.character(),
#               emiss = 1:10)

emiss_records <- read.csv("/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/01_2019fall_Campfire/disperseR/emission-stack1.csv")
# perform the merge
hysp_output_emiss <- 
  merge( hysp_output_trim,
         emiss_records,
         by = 'emiss_date')


# create sf object
hysp_output_emiss.sf <- 
  st_as_sf( hysp_output_emiss,
            coords = c( 'lon', 'lat'),
            crs = 'WGS84') %>%
  st_transform( p4s)
# plot( hysp_output_emiss.sf)


# create mesh for exposure grid
# create raster with resolution res.link.
# I use 12km here
# testing with California since this is camp fire data
res.link <- 12000
spdf <- us_states( states = 'MA') %>% st_transform( p4s)
# create a grid, use 4km resoulation
fishnet.sf <-
  st_bbox( spdf) %>%
  st_as_sfc() %>%
  st_make_grid( cellsize = res.link) %>%
  st_sf()

# assign a grid ID to the raster
fishnet.sf$gridID <- 1:nrow(fishnet.sf)

# spatial join the raster and the locations
hysp_output_join <- 
  st_join( hysp_output_emiss.sf,
           fishnet.sf,
           join = st_within)

# check with plot of grid ID
# plot( spdf[, 'statefp'])
# plot( hysp_output_join[, 'hour'])

# extract corrdinates
# note that these are centroids!
fishnet.dt <- 
  st_centroid( fishnet.sf) %>%
  st_coordinates() %>%
  data.table()
fishnet.dt[, gridID := fishnet.sf$gridID]

# create data table and merge with coordinates
hysp_output_join.dt <- 
  data.table( hysp_output_join) %>%
  merge( fishnet.dt,
         by = 'gridID',
         allow.cartesian = TRUE)


# sum emissions by grid ID
hysp_exposure <- 
  hysp_output_join.dt[, .( exposure = sum( emiss)),
                      by = .( Pdate, gridID, X, Y)]

# merge with original fishnet and plot
hysp_exposure.sf <- 
  merge( hysp_exposure,
         fishnet.sf,
         by = 'gridID')

#delete the exposure results 0
hysp_exposure.sf <- hysp_exposure.sf[hysp_exposure.sf$exposure != 0, ]

#######################################################################################
##save the exposure results files and make it have same rows names as gridlinks results
hysp_exposure.sf <-as.data.table(hysp_exposure.sf)

hysp_exposure.save <- hysp_exposure.sf[ , -c("gridID", "geometry")]

colnames(hysp_exposure.save) <- c("yearmonth","x","y","hyads")

hysp_exposure.save$uID <- "CAMP"

hysp_exposure.save <- as.data.table(hysp_exposure.save)
# wrtite.fst(hysp_exposure.save, "./2014-01-stack1")

Dates <- unique( hysp_exposure.save[, c("yearmonth")])
Dates$facility <- "stack1"
  
fun <- function(i){
  daily_file <- subset(hysp_exposure.save, yearmonth == Dates$yearmonth[i])
  
  # set the patn and file name
  exp_dir <- "/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/01_2019fall_Campfire/disperseR/stack1/hyads/grids/2018/"
  file.mo <- file.path(exp_dir,
                       paste0(
                         'grid_daily_weight_exposure_',
                         paste0(as.character(Dates[i])),
                         '.fst'
                       ))
  
  write.fst(daily_file, path = file.mo)
  return(file.mo)
}


lapply(1:nrow(Dates), fun)

########################################################################################

# plot
# ggplot( hysp_exposure.sf) + 
#   geom_sf( data = spdf, color = 'black') + 
#   geom_sf( data = hysp_exposure.sf,
#            aes( fill = exposure, geometry = geometry),
#            color = NA) + 
#   scale_fill_viridis( limits = c( 0, 1), oob = scales::squish) +
#   scale_color_viridis( limits = c( 0, 1), oob = scales::squish) +
#   # be sure to show 0 in the color scales
#   expand_limits( fill = 0, color = 0) +
#   facet_wrap( . ~ Pdate)

