library( ggplot2)
library( data.table)
library( viridis)
library( tidyverse)
library( sf)

# d_ampd2 <- fread( '~/Dropbox/Harvard/ARP/Data_AMPD_EIA/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv')
# 
# ampd_unique <- unique( d_ampd2[Initial.Year.of.Operation <= 1940,
#                                .( Facility.ID..ORISPL., Unit.ID,
#                                   Initial.Year.of.Operation,
#                                   Facility.Name.x, State.x, County.x,
#                                   Facility.Latitude.x, Facility.Longitude.x,
#                                   Source.Category.x)])
# fwrite( ampd_unique, '~/Dropbox/GeorgeMason/Grants/2020/NIH_1940s_PP/prelim_analysis/ampd_coal_plants.csv')
# 
# 
# # plot facilities by initial operating year
# ggplot( d_ampd2[Initial.Year.of.Operation <= 1940]) +
#   geom_polygon(data = map_data("state"),
#                aes( x = long,
#                     y = lat,
#                     group = group),
#                fill = 'white',
#                color = "black",
#                size = .25) +
#   geom_point( aes( x = Facility.Longitude.x,
#                    y = Facility.Latitude.x,
#                    color = Initial.Year.of.Operation)) +
#   scale_color_viridis( option = 'A',
#                        end = .8)

generator2010ret <- readxl::read_xls( '/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/GeneratorsY2010.xls',
                                      sheet = 3) %>% data.table
generator2010act <- readxl::read_xls( '/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/GeneratorsY2010.xls',
                                      sheet = 1) %>% data.table

# limit to facilities operating by 1940 and not retired by 1940
cols_keep <- c( "UTILITY_ID", "PLANT_CODE", "PLANT_NAME", "STATE", "COUNTY",
                "NAMEPLATE", "OPERATING_YEAR", "GENERATOR_ID", #measure of the generator capacity NAMEPLATE, EIA 860, annual average
                "ENERGY_SOURCE_1", "ENERGY_SOURCE_2")

generator1940ret <- generator2010ret[OPERATING_YEAR <= 1940 & RETIREMENT_YEAR > 1940, ..cols_keep]
generator1940act <- generator2010act[OPERATING_YEAR <= 1940, ..cols_keep]
generator1940 <- rbind( generator1940ret, generator1940act)


# plot total nameplate capacity by county
generator1940[ (ENERGY_SOURCE_1 %in% c( 'BIT', 'ANT', 'LIG', 'SUB', 'WC', 'RC')),
               Coal := NAMEPLATE]
generator1940[ (ENERGY_SOURCE_1 %in% c( 'DFO', 'JF', 'KER', 'PC', 'RFO', 'WO')),
               Petroleum := NAMEPLATE]
generator1940[ (ENERGY_SOURCE_1 %in% c( 'BFG', 'NG', 'OG', 'PG', 'SG', 'SGC')),
               Gas := NAMEPLATE]
generator1940[ (ENERGY_SOURCE_1 %in% c( 'AB', 'MSW', 'OBS', 'WDS', 'OBL', 'SLW', 'BLQ', 'WDL', 'LFG', 'OBG')),
               Renewable := NAMEPLATE]

# sum by fuel type and county
fueltype_names <- c( 'Coal', 'Petroleum', 'Gas', 'Renewable')

generator1940_county <- generator1940[, lapply( .SD, sum, na.rm = T),
                                      .SDcols = fueltype_names, by = .( STATE, COUNTY)]

# plot
counties.sf <- USAboundaries::us_counties( )
states48 <- state.abb[!(state.abb %in% c( 'HI', 'AK'))]
counties.sf <- counties.sf[ counties.sf$state_abbr %in% states48,]
counties.sf$name <- tolower( counties.sf$name)
generator1940_county[, COUNTY := tolower( COUNTY)]
generator1940_county.sf <- merge( generator1940_county,
                                  counties.sf[, c( 'state_abbr', 'name', 'geometry')], 
                                  by.x = c( 'STATE', 'COUNTY'),
                                  by.y = c( 'state_abbr', 'name'),
                                  all.y = T)

#=============================================================================================#
#or use county level
#read the county shp file
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/nhgis0001_shapefile_tl2008_us_county_1940"

# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
  st_read( file.path( dir.in, 'US_county_1940_conflated.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                       crs = st_crs(p4s))

roadiness.trans.county <- na.omit(roadiness.trans.county)

roadiness.trans.county$state_abbr <- state.abb[match(roadiness.trans.county$STATENAM, state.name)]

generator2010_county[, COUNTY := tolower( COUNTY)]
generator2010_county.sf <- merge( generator2010_county,
                                  roadiness.trans.county[, c( 'state_abbr', 'NHGISNAM', 'geometry')], 
                                  by.x = c( 'STATE', 'COUNTY'),
                                  by.y = c( 'state_abbr', 'NHGISNAM'),
                                  all.y = T)


generator2010_county.sf.m <- melt( generator2010_county.sf,
                                   id.vars = c( 'STATE', 'COUNTY', 'geometry'),
                                   measure.vars = fueltype_names)
generator2010_county.sf.m[is.na( value), value := 0]
#=============================================================================================#

# melt
generator1940_county.sf.m <- melt( generator1940_county.sf,
                                   id.vars = c( 'STATE', 'COUNTY', 'geometry'),
                                   measure.vars = fueltype_names)
generator1940_county.sf.m[is.na( value), value := 0]
# us states dataset
states48 <- state.abb[!(state.abb %in% c( 'HI', 'AK'))]
states_raw <- USAboundaries::us_states()
us_states_raw <- states_raw[ states_raw$state_abbr %in% states48,]

gg_facility_locs <- 
  ggplot( generator1940_county.sf.m[ STATE %in% states48 & 
                                       !( variable %in% 'Renewable')]) + 
  geom_sf( data = us_states_raw, fill = NA) +
  geom_sf( aes( fill = value, geometry = geometry), color = NA, size = 0) + 
  scale_fill_gradient( low = 'azure', high = 'blueviolet', limits = c( 0, 40),
                       breaks = c( 0, 3, 10, 30),
                       oob = scales::squish,
                       trans = scales::pseudo_log_trans()) + 
  facet_wrap( . ~ variable, nrow = 1) +
  labs( fill = bquote( atop( 'Capacity', '(MW)'))) +
  theme_minimal() + 
  theme( axis.text = element_blank(),
         legend.direction = 'vertical',
         legend.position = 'right',
         legend.text = element_text( size = 12),
         legend.title = element_text( size = 10),
         strip.text = element_text( size = 14),
         panel.grid = element_blank())

ggsave( gg_facility_locs,
        filename = '~/Dropbox/GeorgeMason/Grants/2020/NIH_1940s_PP/prelim_analysis/facility_locs_1940.png',
        width = 3.5*3, height = 2.25, scale = 1.2)


generator2040_coal <- generator1940[!is.na( Coal)]
fwrite( generator2040_coal, '~/Dropbox/GeorgeMason/Grants/2020/NIH_1940s_PP/prelim_analysis/eia_coal_plants.csv')



nrow( generator2040_coal)
nrow( generator2040_pet)
nrow( generator2040_gas)
nrow( generator2040_ab)
nrow( generator2040_lrb)
nrow( generator2040_grb)

# plot counties by total nameplate capacity for rach fuel type
length( unique( generator2040_coal$COUNTY))
length( unique( generator2040_pet$COUNTY))
length( unique( generator2040_gas$COUNTY))

## ============================================== ##
## create inverse distance weighted metrics
## ============================================== ##
# define a coordinate reference system
#p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"


generator1940_gen <- generator1940[, lapply( .SD, sum, na.rm = T),
                                   .SDcols = fueltype_names, 
                                   by = .( UTILITY_ID, PLANT_CODE, PLANT_NAME, STATE, COUNTY)]
generator1940_gen[, COUNTY := tolower( COUNTY)]
generator1940_gen.sf <- merge( generator1940_gen,
                               counties.sf[, c( 'state_abbr', 'name', 'geometry')], 
                               by.x = c( 'STATE', 'COUNTY'),
                               by.y = c( 'state_abbr', 'name'))
generator1940_gen.sf[ , geometry := st_transform( geometry, p4s)]


# melt to long form
generator1940_gen.sf.m <- 
  melt( generator1940_gen.sf,
        id.vars = c( 'UTILITY_ID', 'PLANT_CODE', 'PLANT_NAME', 'STATE', 'COUNTY', 'geometry'))

# assign each facility the centroid of its county because we don't have plant-specific coordinates
generator1940_gen.sf.m[, centroid_geo := st_centroid( geometry)] 

# limit to just capacity > 0
generator1940_gen_use <- generator1940_gen.sf.m[value > 0]

# function to calculate inverse-distance weighting for each facility
inv_distancer <- function( n, 
                           p.sf = generator1940_gen_use, 
                           grid.sf = counties.sf){
  print( scales::percent( n / nrow( p.sf)))
  # transform crs
  grid.sf <- st_transform( grid.sf, st_crs( p.sf$centroid_geo))
  
  # calculate distances
  site_dist <- as.vector( 
    st_distance(p.sf[n,centroid_geo], 
                st_centroid( grid.sf))) #/ 1e3
  
  # distance is 1 in same grid cell
  site_dist[site_dist == 0] <- min( site_dist[ site_dist > 0])
  
  # output datset
  out <- data.table( N = n,
                     p.sf[n, .( UTILITY_ID, PLANT_CODE, PLANT_NAME, STATE, COUNTY, variable, value)],
                     grid.sf[, c( 'statefp', 'countyfp', 'countyns', 'geoid', 'state_name', 'state_abbr')],
                     inv_dist = 1 / site_dist)
  
  out[ , exp_inv_dist := value * inv_dist]
  
  return( out)
}

# do the calculation
exp_inverse_dist <- lapply( 1:nrow( generator1940_gen_use),
                            inv_distancer) %>%
  rbindlist

setnames( exp_inverse_dist,
          c( 'variable', 'value'),
          c( 'FUEL', 'NAMEPLATE_CAPACITY'))

counties.sf <- counties.sf[,-9] #remove duplicate name

# sum the IDW
exp_inverse_dist_sum <- 
  exp_inverse_dist[, .( exposure = sum( exp_inv_dist)),
                   by = .( FUEL, geoid)] %>%
  merge( counties.sf, by = 'geoid')

#normalize
MEAN <- mean(exp_inverse_dist_sum$exposure)

SD <- sd(exp_inverse_dist_sum$exposure)

exp_inverse_dist_sum$normalize <- (exp_inverse_dist_sum$exposure - MEAN)/SD

# plot exposure from each
exp_inverse_dist_f <- 
  ggplot( exp_inverse_dist_sum,
          aes( geometry = geometry,
               fill = exposure)) + 
  # geom_sf( data = counties.sf, color = 'black', fill = 'white') + 
  geom_sf( color = NA, ) + 
  scale_fill_gradient( low = 'white', high = 'red') + 
  facet_wrap( . ~ FUEL)

#plot the normalize
ggplot( exp_inverse_dist_sum,
          aes( geometry = geometry,
               fill = normalize)) + 
  # geom_sf( data = counties.sf, color = 'black', fill = 'white') + 
  geom_sf( color = NA) + 
  scale_fill_gradient( name = "IDW exposure",
                      low = 'white', high = 'red',
                       limits = c( -1, 1), 
                       breaks = c( -1, 0, 1),
                       labels = c( '-1', '0', '1'),
                       oob = scales::squish) + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20))+ 
  facet_wrap( . ~ FUEL)

ggsave( exp_inverse_dist_f,
        filename = '/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/prelim_analysis/PowerPlant/exp_inverse_dist_facility_1940.png',
        width = 3.5*2, height = 2.25*2, scale = 1.2)

save( exp_inverse_dist,
      exp_inverse_dist_sum,
      file =  '~/Dropbox/GeorgeMason/Grants/2020/NIH_1940s_PP/prelim_analysis/inv_dist_facilities.RData')


##############################################################################################
###plot the petro station and its exposure here
##############################################################################################
petro_idw <- exp_inverse_dist_sum[exp_inverse_dist_sum$FUEL == "Petroleum",]
coal_idw <- exp_inverse_dist_sum[exp_inverse_dist_sum$FUEL == "Coal",]
gas_idw <- exp_inverse_dist_sum[exp_inverse_dist_sum$FUEL == "Gas",]
renew_idw <- exp_inverse_dist_sum[exp_inverse_dist_sum$FUEL == "Renewable",]

petro_location <- generator1940_gen_use[generator1940_gen_use$variable == "Petroleum",]
coal_location <- generator1940_gen_use[generator1940_gen_use$variable == "Coal",]
gas_location <- generator1940_gen_use[generator1940_gen_use$variable == "Gas",]
renew_location <- generator1940_gen_use[generator1940_gen_use$variable == "Renewable",]

petro.station <- petro_location[, c("centroid_geo")]
coal.station <- coal_location[, c("centroid_geo")]
gas.station <- gas_location[, c("centroid_geo")]
renew.station <- renew_location[, c("centroid_geo")]

colnames(petro.station) <- "geometry"
colnames(coal.station) <- "geometry"
colnames(gas.station) <- "geometry"
colnames(renew.station) <- "geometry"

petro.station.t <- st_as_sf(petro.station)  %>% st_transform(crs = st_crs(petro_idw$geometry))
coal.station.t <- st_as_sf(coal.station)  %>% st_transform(crs = st_crs(coal_idw$geometry))
gas.station.t <- st_as_sf(gas.station)  %>% st_transform(crs = st_crs(gas_idw$geometry))
renew.station.t <- st_as_sf(renew.station)  %>% st_transform(crs = st_crs(renew_idw$geometry))

ggplot( ) +
  # add the disperser grid
  geom_sf( data = petro_idw,
           aes( geometry = geometry,
                fill = exposure)) +
  geom_sf( data = petro.station.t,
           aes(geometry = geometry),
           color = 'blue', size = 0.5)+
scale_fill_gradient( low = 'white', high = 'red',
                     limits = c( 0, 0.002), 
                     breaks = c( 0, 0.001, 0.002),
                     labels = c( '0.0', '0.001', '0.002')) + 
  theme(legend.position = 'bottom')

#another style without background
ggplot( ) +
  geom_sf( data = petro_idw,
           aes( geometry = geometry,
                fill = idw_nrm), color=NA) +
  scale_fill_gradient( name = "IDW exposure",
                       low = 'blue', high = 'red',
                       limits = c( -1, 1),
                       breaks = c( -1, 0, 1),
                       labels = c( '-1', '0', '1'),
                       oob = scales::squish) +
  geom_sf( data = petro.station.t,
           aes(geometry = geometry),
           color = 'green', size = 0.5)+
  theme(plot.title = element_text(size = 20, face = "bold"),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20))
## ============================================== ##
## check out CMIP6 gridded emissions
## ============================================== ##
library( raster)
library( ncdf4)

# https://handle-esgf.dkrz.de/lp/21.14100/5b2e3831-1d4d-4566-ab60-d5f1cda1a73b
file_in <- '~/Dropbox/Harvard/Meetings_and_People/JoanCasey/1940s_PowerPlants/SO2-em-anthro_input4MIPs_emissions_CMIP_CEDS-2017-05-18_gn_190001-194912.nc'

emiss <- raster::brick( file_in, varname = "SO2_em_anthro",
                        lvar = 3, level = 5)

emiss1940 <- emiss[['X1940.07.15']]
emiss1940[emiss1940 == 0] <- NA
plot( emiss1940, xlim = c( -180, -40), ylim = c( 0, 90))
plot( emiss[['X1949.12.16']])
sum( values( emiss[['X1940.01.16']]))

## ============================================================================= ##
##save the file, IDW in csv and geometry in shp, then find way to combine thems
## ============================================================================= ##

idw_pp <- exp_inverse_dist_sum[, -c("geometry")]
wrtite.csv(idw_pp, "./idw_pp.csv")

#geometry info
geo_sf <- counties.sf[, c("geoid", "geometry")]

exp_inverse_dist_sum <- 
  idw_pp %>%
  merge( geo_sf, by = 'geoid')

ncin <- nc_open(file_in)
names( ncin$var)
names( ncin$dim)
