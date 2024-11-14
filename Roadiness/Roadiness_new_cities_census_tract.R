library( sf)
library( data.table)
library( magrittr)
library( ggplot2)
library( raster)
library( areal)
library( dplyr)
library( viridis)
library( rlist)

# define a coordinate reference system
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

## ===================================================== ##
##  read in the locations for calculating roadiness
## ===================================================== ##
# read census tract
# read in the shape files and merge to single dataset
USdata <- read_sf("C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/nhgis0035_shapefile_tl2000_us_tract_1940/US_tract_1940.shp")
USstdata <- read_sf("C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/nhgis0035_shapefile_tl2000_us_tract_1940/US_tractcounty_1940.shp")
plot( USdata[USdata$NHGISCTY == '1130'& USdata$NHGISST=='480',1]) #DallasTX
# plot( USdata[USdata$NHGISCTY == '0310'& USdata$NHGISST=='080',1]) #DenverCO
# plot( USdata[USdata$NHGISCTY == '1250'& USdata$NHGISST=='260',1]) #Oakland (not correct, road for CA, border for MD)
# plot( USdata[USdata$NHGISCTY == '1010'& USdata$NHGISST=='420',1]) #Philadelphia
#plot( USdata[USdata$NHGISCTY == '0790',])
#plot( USstdata[USstdata$NHGISCTY == '0790'&USstdata$NHGISST=='550',])
USdata[,7]=st_transform(USdata[,7],p4s) # convert the coord to as defined in p4s (important)!!!
citynums = as.data.frame(USdata)[,c('NHGISST','NHGISCTY')]
citynums$NHGISNAM = ''
citynums$ed=''
for(i in 1:nrow(citynums)){
  cityname = USstdata[USstdata$NHGISST==citynums[i,1] & USstdata$NHGISCTY == citynums[i,2],]$NHGISNAM
  citynums[i,3]=cityname
  citynums[i,4]=i
}
USdata$city<-citynums[,3]
USdata$ed<-citynums[,4]
ed.in<-USdata[,7:9]

## ===================================================== ##
##  read in the road shapefiles
## ===================================================== ##
# file.loc.road <- '~/Dropbox/GeorgeMason/Grants/2020/NIH_1940s_PP/data/stgrids_edited_both'
file.loc.road <- "C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/Elisabeth_new_data/processed_city_street_grids"
# List all .shp files within each city folder (including subdirectories)
city_folders <- list.dirs(file.loc.road, recursive = FALSE, full.names = TRUE)
files.road <- unlist(lapply(city_folders, function(folder) {
  list.files(folder, pattern = '\\.shp$', full.names = TRUE)
}))

# read in the shape files and merge to single dataset
roads.in <- 
  lapply( files.road,
          function( f) {
            in.sf <- read_sf( f) %>%
              st_transform( p4s) %>%
              st_cast( 'LINESTRING')
            
            city <- gsub( '.*/|_stgrid.*', '', f)
            # in.sf$city <- city
            in.sf$city <- substr(city,1,nchar(city)-2)  # we use a shorter city name (remove state abbr).
            
            in.sf$length <- st_length( in.sf)
            return( in.sf[, c( 'length', 'city', 'geometry')])
          }) %>%
  rbindlist %>%
  st_as_sf

# not sure why I have to do this.
roads.in <- roads.in[1:nrow( roads.in),]

# Clean up city names in roads.in to match those in ed.in
roads.in <- roads.in %>%
  mutate(city = case_when(
    grepl("atlanta", city, ignore.case = TRUE) ~ "Fulton",
    grepl("buffalo", city, ignore.case = TRUE) ~ "Erie",
    grepl("chicago", city, ignore.case = TRUE) ~ "Cook",
    grepl("cincinnati", city, ignore.case = TRUE) ~ "Hamilton",
    grepl("cleveland", city, ignore.case = TRUE) ~ "Cuyahoga",
    grepl("dc", city, ignore.case = TRUE) ~ "District Of Columbia",
    grepl("detroit", city, ignore.case = TRUE) ~ "Wayne",
    grepl("jerseycity", city, ignore.case = TRUE) ~ "Hudson",
    grepl("kansascity", city, ignore.case = TRUE) ~ "Jackson",
    grepl("la_", city, ignore.case = TRUE) ~ "Los Angeles",
    grepl("louisville", city, ignore.case = TRUE) ~ "Jefferson",
    grepl("milwaukee", city, ignore.case = TRUE) ~ "Milwaukee",
    grepl("nola", city, ignore.case = TRUE) ~ "Orleans",
    grepl("nyc", city, ignore.case = TRUE) ~ "New York",
    grepl("portland", city, ignore.case = TRUE) ~ "Multnomah",
    grepl("rochester", city, ignore.case = TRUE) ~ "Monroe",
    grepl("seattle", city, ignore.case = TRUE) ~ "King",
    grepl("sf", city, ignore.case = TRUE) ~ "San Francisco",
    grepl("stlouis", city, ignore.case = TRUE) ~ "St Louis",
    TRUE ~ city # Keep original value if no match found
  ))

# View the updated unique city names
unique(roads.in$city)
## ===================================================== ##
##  sum lengths of roads in each ED
## ===================================================== ##
cities <- intersect( roads.in$city,
                     ed.in$city)
cities <- list.remove(cities,c(4))

#issue cities: "Los Angeles"
# Vector of desired city names
desired_cities <- c("St Louis")

# Extract only these cities from the cities list
extracted_cities <- cities[cities %in% desired_cities]

roadiness_by_ed <- 
  lapply( #cities,
          extracted_cities,
          function( c){
            print( c)
            #first, limit to the roads ans ed's for that city
            roads.c <- roads.in[ roads.in$city == c,]
            eds.c <- ed.in[ ed.in$city == c,] %>%
              st_make_valid
            
            st_crs(roads.c) <- p4s 
            #take the maximum extent
            ext.roads <- st_bbox( roads.c) # st_bbox returns xmin,xmax,ymin and ymax of the shape.
            ext.eds <- st_bbox( eds.c)
            ext.use <- extent( min( ext.roads$xmin, ext.eds$xmin),  # wrong coord sys causes the time to increase.
                               max( ext.roads$xmax, ext.eds$xmax),
                               min( ext.roads$ymin, ext.eds$ymin),
                               max( ext.roads$ymax, ext.eds$ymax))
            
            # create the raster
            road_grid <- raster( ext.use,
                                 res = 1000,
                                 crs = p4s,
                                 vals = 0)
            
            # convert to sf (takes some time)
            road_grid.sf <- 
              rasterToPolygons( road_grid) %>%
              st_as_sf
            
            # set road counter to zero
            road_grid.sf$id <- 1:nrow( road_grid.sf)
              
            # spatial join road segments over ED's
            ras.roads <- 
              st_join( roads.c, road_grid.sf, join = st_intersects) %>% 
              data.table
            
            # sum length by grid cell
            ras.summary <- ras.roads[, .( road_leng = sum( length)),
                                     by = .( id)]
            
            # map back to raster
            # add to road grid
            tmp <- merge( data.table( road_grid.sf[,c( 'id')]), 
                          ras.summary, by = c( 'id'), all.x = T)
            tmp[is.na( road_leng), road_leng := 0]
            tmp[ , road_leng := as.vector( road_leng)]
            road_leng.sf <- st_as_sf( tmp)
            
            # roadiness time! 
            # calculate grid-monitor distances in km
            # use lapply to do each grid cell
            id.unq <- unique( road_leng.sf$id)
            roadiness <- 
              lapply( id.unq,
                      function( i){
                        # grab the grid cell corresponding to id
                        site.sf <- road_leng.sf[i,]
                        
                        # get centroids for all grid cells
                        road_grid.c <- data.table( coordinates( road_grid),
                                                   values( road_grid))
                        road_grid.c.sf <- st_as_sf( road_grid.c, coords = c( 'x', 'y'),
                                                    crs = p4s)
                        
                        # get the distance from that point to all points
                        site_dist <- as.vector( 
                          st_distance( site.sf, 
                                       road_grid.c.sf)) / 1000
                        
                        # if the distance is zero, assign it 1
                        site_dist[site_dist < 1] <- 1
                        
                        # roadiness is the sum of nroads / dist
                        roadiness.dt <- data.table( leng.dist1  = sum( road_leng.sf$road_leng / site_dist),
                                                    leng.dist1.5  = sum( road_leng.sf$road_leng / site_dist^1.5),
                                                    leng.dist2  = sum( road_leng.sf$road_leng / site_dist^2),
                                                    leng.dist3  = sum( road_leng.sf$road_leng / site_dist^3))
                        
                        # assign id
                        roadiness.dt[, `:=` ( id = i)]
                      }) %>% rbindlist
            
            # merge roadiness with spatial object
            road_grid_roadiness.sf <- 
              merge( road_leng.sf, roadiness, by = c( 'id'))
            
            # convert to data table and melt
            road_grid_roadiness.dt <- 
              data.table( road_grid_roadiness.sf)[, geometry := NULL]
            road_grid_roadiness.m <- 
              melt( road_grid_roadiness.dt, id.vars = c( 'id'),
                    variable.name = 'type.r', value.name = 'roadiness.grid')
            
            # spatially average over ED's
            # take areas of zips over grids
            weights.dt <- 
              aw_intersect( eds.c, 
                            source = road_grid_roadiness.sf, 
                            areaVar = "area") %>%
              data.table
            
            # take total areas of zip fores
            zips.a <- data.table( ed = eds.c$ed,
                                  ed.area = as.vector( st_area( eds.c)))
            
            # merge together for weighting dataset
            weights.m <- merge( weights.dt, zips.a, by = c( 'ed'))
            weights.m[, areaWeight := area / ed.area]
            
            # intersect weights and grids
            ncin.m.intersect <- 
              merge( weights.m, road_grid_roadiness.m, 
                     by = c( 'id'), allow.cartesian = TRUE)
            
            # multiply weights by grid pm25, sum by 
            ncin.m.intersect[, roadiness.a := areaWeight * roadiness.grid]
            ed.roadiness <- 
              ncin.m.intersect[, .( roadiness = sum( roadiness.a)), 
                               by = .( ed, type.r)]
            
            # cast for smaller file size
            ed.roadiness.c <- 
              dcast( ed.roadiness, ed ~ type.r, 
                     value.var = 'roadiness')
            
            # add back in city
            ed.roadiness.c[, city := c]
            
            return( ed.roadiness.c)
          }) %>% 
  rbindlist

# scale for interpretability
roadiness_by_ed[ , roadiness2 := scale( leng.dist2)]

# merge together for plotting
ed.roadiness.all <- 
  merge( ed.in,
         roadiness_by_ed,
         by = c( 'city', 'ed') )

plot( ed.roadiness.all[ ed.roadiness.all$city == 'New York',])
roadiness_by_ed[city == 'New York'][which.max( road_leng)]

#================================================================================
#save dataset
#================================================================================
# Remove geometry from ed.roadiness.all
ed_roadiness_no_geom <- st_drop_geometry(ed.roadiness.all)

# Ensure ed column in both datasets is numeric
ed_roadiness_no_geom$ed <- as.numeric(ed_roadiness_no_geom$ed)
USdata$ed <- as.numeric(USdata$ed)

# Merge the two data tables by city and ed
merged_data <- ed_roadiness_no_geom %>%
  left_join(USdata, by = c("city", "ed"))

# View the merged data
print(head(merged_data))

# Remove geometry from ed.roadiness.all
merged_data_no_geom <- merged_data %>% select(-geometry)

#write.csv(merged_data, "C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/Elisabeth_new_data/data/1940_NHGIS_Roadiness_st_louis_cities.csv", row.names = FALSE)

# create a plot with all cities
# legend_min_max <- c( 0, quantile( ed.roadiness.all$lend.dist2_scale, .8))
legend_min_max <- c( -1, 1)

plots_list <- 
  lapply( cities,
          function( c){
            ggplot( ed.roadiness.all[ed.roadiness.all$city == c,]) +
              geom_sf( aes( fill = roadiness2),
                       color = NA) +
              scale_fill_viridis( limits = legend_min_max,
                                  oob = scales::squish,
                                  direction = 1) +
              facet_wrap( . ~ city) +
              theme_minimal() + 
              theme( axis.text = element_blank(),
                     legend.position = 'none',
                     strip.text = element_text( size = 8))
          })

plot_all <- cowplot::plot_grid(plotlist = plots_list)
plot_all

## write the roadiness data to a file??first, scale an normalize roadiness 2

write.csv( roadiness_by_ed[, .( city, ed, road_leng, roadiness2)],'./RoadinessData/roadiness_by_city_census.csv')
