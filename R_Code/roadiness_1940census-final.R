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
#p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m" 

## ===================================================== ##
##  read in the locations for calculating roadiness
## ===================================================== ##
# read census tract
# read in the shape files and merge to single dataset
USdata <- read_sf("./nhgis0035_shapefile_tl2000_us_tract_1940/US_tract_1940.shp")
USstdata <- read_sf("./nhgis0035_shapefile_tl2000_us_tract_1940/US_tractcounty_1940.shp")
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
file.loc.road <- "./stgrids_edited_both"
files.road <- list.files( file.loc.road, 
                          pattern = '.shp$',
                          full.names = TRUE)

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

## ===================================================== ##
##  sum lengths of roads in each ED
## ===================================================== ##
roads_tracts <-st_join( roads.in, USdata)
cities <- unique(roads_tracts$city.x)
cityx2y = as.data.frame(roads_tracts)[,c('city.x','city.y')] #creates a table to check cities' different names.
cityx2y=unique(cityx2y)#simplify repeted cities
cityx2y=na.omit(cityx2y)#delete empty roads which some cities don't have old names
for(i in 1:nrow(ed.in)){ #change the old names of cities in ed.in to new names (same as in stgird).
  if(!is.na(cityx2y[cityx2y$city.y==ed.in[i,]$city,]$city.x[1])){
    ed.in[i,2]=cityx2y[cityx2y$city.y==ed.in[i,]$city,]$city.x[1]
  }
}

#st_write(roads_tracts, "roads_tracts.shp")
# cities <- intersect( roads.in$city,
#                      ed.in$city)
# cities <- list.remove(cities,c(4))

roadiness_by_ed <- 
  lapply( cities,
          function( c){
            print( c)
            #first, limit to the roads ans ed's for that city
            roads.c <- roads.in[ roads.in$city == c,]
            eds.c <- ed.in[roads_tracts[roads_tracts$city.x==c,"city.y"],] %>%
              st_make_valid
            
            #take the maximum extent
            ext.roads <- st_bbox( roads.c) # st_bbox returns xmin,xmax,ymin and ymax of the shape.
            ext.eds <- st_bbox( eds.c)
            ext.use <- extent( min( ext.roads$xmin, ext.eds$xmin),  # wrong coord sys causes the time to increase.
                               max( ext.roads$xmax, ext.eds$xmax),
                               min( ext.roads$ymin, ext.eds$ymin),
                               max( ext.roads$ymax, ext.eds$ymax))
            
            # if all NA in boundary box, skip this city
            if( is.na( ext.use[1]))
              return( data.table( GISJOIN = NA,
                                  road_leng = NA,
                                  leng.dist1 = NA,
                                  leng.dist1.5 = NA,
                                  leng.dist2 = NA,
                                  leng.dist3 = NA,
                                  city = 'c'))
            
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
roadiness_by_ed=na.omit(roadiness_by_ed)

# merge together for plotting (one city may have two names).
ed.roadiness.all <- 
  merge( ed.in,
         roadiness_by_ed,
         by = c( 'city','ed') )

plot( ed.roadiness.all[ ed.roadiness.all$city == 'Dallas',])
roadiness_by_ed[city == 'Dallas'][which.max( road_leng)]

for(i in 1:length(cities)){ #delete cities whose roadiness is not calculated.
  if(length(ed.roadiness.all[ed.roadiness.all$city==cities[i],]$city)==0){
    cities[i]=NA
  }
}
cities=na.omit(cities)

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
                     strip.text = element_text( size = 50))
          })

plot_all <- cowplot::plot_grid(plotlist = plots_list)
plot_all

## write the roadiness data to a file??first, scale an normalize roadiness 2
roadiness_by_ed$GISJOIN=''
roadiness_by_ed$GISJOIN2=''
for (i in 1:nrow(roadiness_by_ed)){
  roadiness_by_ed[i,8]=USdata[as.character(roadiness_by_ed[i,1])==USdata$ed,]$GISJOIN[1]
  roadiness_by_ed[i,9]=USdata[as.character(roadiness_by_ed[i,1])==USdata$ed,]$GISJOIN2[1]
}

write.csv( roadiness_by_ed[, .( city, ed, road_leng, roadiness2,GISJOIN,GISJOIN2)],'./roadiness_by_city_census.csv')

combine_city = as.data.frame(ed.in)[,c('city')]

############################################################
##make the file to show the missing data in ed.in or road.in
############################################################
tract_city = unique(ed.in$city)
road_city = unique(roads.in$city)
city_x = as.data.frame(ed.in)[,c('city')]
city_y = as.data.frame(roads.in)[,c('city')]
combine_city <- union(city_x,city_y)
combine_city=as.data.frame(combine_city)
combine_city$tract =''
combine_city$road =''

for(i in 1:nrow(combine_city)){
  if(combine_city[i,1] %in% tract_city){
    combine_city[i,2]=TRUE
  }
  else{combine_city[i,2]=FALSE}
  if(combine_city[i,1] %in% road_city){
    combine_city[i,3]=TRUE
  }
  else{combine_city[i,3]=FALSE}
}

# write.csv( combine_city[,],'./Available_Data_Check.csv')
