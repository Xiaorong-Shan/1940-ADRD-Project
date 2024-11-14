library(sf)
library(data.table)
library(magrittr)
library(ggplot2)
library(raster)
library(areal)
library(dplyr)
library(viridis)
library(rlist)

# define a coordinate reference system
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

## ===================================================== ##
##  Read in the locations for calculating roadiness at ED level
## ===================================================== ##
# Load the ED shapefile data
EDdata <- readRDS("C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/ED-level/ED_shapefile_all_cities.RDS")
EDdata <- st_transform(EDdata, p4s)  # Make sure to transform it to the correct CRS

# Plot example EDs
# plot(EDdata[EDdata$NHGISCTY == '1130' & EDdata$NHGISST == '480', 1])  # Dallas, TX example

# Extract necessary columns for analysis
EDdata$ed <- 1:nrow(EDdata)  # Create a unique identifier for each ED (optional if there's no existing unique ID column)
EDdata$city <- as.character(EDdata$city)  # Assuming your ED data has a 'city' column indicating the city names
ed.in <- EDdata[, c('city', 'ed', 'geometry')]

## ===================================================== ##
##  Read in the road shapefiles
## ===================================================== ##
# file.loc.road <- '~/Dropbox/GeorgeMason/Grants/2020/NIH_1940s_PP/data/stgrids_edited_both'
file.loc.road <- "C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/Elisabeth_new_data/processed_city_street_grids"
# List all .shp files within each city folder (including subdirectories)
city_folders <- list.dirs(file.loc.road, recursive = FALSE, full.names = TRUE)
files.road <- unlist(lapply(city_folders, function(folder) {
  list.files(folder, pattern = '\\.shp$', full.names = TRUE)
}))

# Read in the road shapefiles and merge them
roads.in <- 
  lapply(files.road, function(f) {
    in.sf <- read_sf(f) %>%
      st_transform(p4s) %>%
      st_cast('LINESTRING')
    
    city <- gsub('.*/|_stgrid.*', '', f)
    in.sf$city <- substr(city, 1, nchar(city) - 2)  # Use shorter city name (remove state abbreviation)
    
    in.sf$length <- st_length(in.sf)
    return(in.sf[, c('length', 'city', 'geometry')])
  }) %>%
  rbindlist %>%
  st_as_sf

#roads.in$city <- "SeattleWA"
## ===================================================== ##
##  Sum lengths of roads in each ED
## ===================================================== ##
city_mapping <- c(
  "atlanta_grid.s"    = "AtlantaGA",
  "buffalo_grid.s"    = "BuffaloNY",
  "chicago_grid.s"    = "ChicagoIL",
  "cincinnati_grid.s" = "CincinnatiOH",
  "cleveland_grid.s"  = "ClevelandOH",
  "dc_grid.s"         = "WashingtonDC",
  "detroit_grid.s"    = "DetroitMI",
  "jerseycity_grid.s" = "JerseyCityNJ",
  "kansascity_grid.s" = "KansasCityKS",
  "la_grid.s"         = "LosAngelesCA",
  "louisville_grid.s" = "LouisvilleKY",
  "milwaukee_grid.s"  = "MilwaukeeWI",
  "nola_grid.s"       = "NewOrleansLA",
  "nyc_grid.s"        = "NewYorkNY",
  "portland_grid.s"   = "PortlandOR",
  "rochester_grid.s"  = "RochesterNY",
  "seattle_grids.s"   = "SeattleWA",
  "sf_grid.s"         = "SanFranciscoCA",
  "stlouis_grid.s"    = "StLouisMO"
)

# Update roads.in$city to match EDdata$city
roads.in$city <- recode(roads.in$city, !!!city_mapping)

cities <- intersect(roads.in$city, ed.in$city)
cities <- list.remove(cities, c(4))  # Example: Remove a specific city if needed

desired_cities <- c("BuffaloNY", "ChicagoIL", "CincinnatiOH", "WashingtonDC", "JerseyCityNJ", "KansasCityKS", "LouisvilleKY", "MilwaukeeWI", "NewOrleansLA", "SeattleWA", "SanFranciscoCA", "StLouisMO")

# Extract only these cities from the cities list
extracted_cities <- cities[cities %in% desired_cities]

#seattle_rows <- ed.in %>% filter(city == "SeattleWA")
roadiness_by_ed <- 
  lapply(extracted_cities, function(c) {
    print(c)
    # Filter data for the current city
    roads.c <- roads.in[roads.in$city == c, ]
    eds.c <- ed.in[ed.in$city == c, ] %>%
      st_make_valid()
    
    st_crs(roads.c) <- p4s 
    
    # Calculate the maximum extent
    ext.roads <- st_bbox(roads.c)
    ext.eds <- st_bbox(eds.c)
    ext.use <- extent(min(ext.roads$xmin, ext.eds$xmin),
                      max(ext.roads$xmax, ext.eds$xmax),
                      min(ext.roads$ymin, ext.eds$ymin),
                      max(ext.roads$ymax, ext.eds$ymax))
    
    # Create a raster
    road_grid <- raster(ext.use, res = 1000, crs = p4s, vals = 0)
    
    # Convert raster to sf
    road_grid.sf <- rasterToPolygons(road_grid) %>% st_as_sf()
    road_grid.sf$id <- 1:nrow(road_grid.sf)
    
    # Spatial join road segments over EDs
    ras.roads <- st_join(roads.c, road_grid.sf, join = st_intersects) %>% data.table
    
    # Sum road lengths by grid cell
    ras.summary <- ras.roads[, .(road_leng = sum(length)), by = .(id)]
    
    # Merge back to road grid
    tmp <- merge(data.table(road_grid.sf[, c('id')]), ras.summary, by = c('id'), all.x = T)
    tmp[is.na(road_leng), road_leng := 0]
    tmp[, road_leng := as.vector(road_leng)]
    road_leng.sf <- st_as_sf(tmp)
    
    # Calculate roadiness for each ED
    id.unq <- unique(road_leng.sf$id)
    roadiness <- lapply(id.unq, function(i) {
      site.sf <- road_leng.sf[i, ]
      
      road_grid.c <- data.table(coordinates(road_grid), values(road_grid))
      road_grid.c.sf <- st_as_sf(road_grid.c, coords = c('x', 'y'), crs = p4s)
      
      site_dist <- as.vector(st_distance(site.sf, road_grid.c.sf)) / 1000
      site_dist[site_dist < 1] <- 1
      
      roadiness.dt <- data.table(leng.dist1 = sum(road_leng.sf$road_leng / site_dist),
                                 leng.dist1.5 = sum(road_leng.sf$road_leng / site_dist^1.5),
                                 leng.dist2 = sum(road_leng.sf$road_leng / site_dist^2),
                                 leng.dist3 = sum(road_leng.sf$road_leng / site_dist^3))
      roadiness.dt[, `:=`(id = i)]
    }) %>% rbindlist
    
    road_grid_roadiness.sf <- merge(road_leng.sf, roadiness, by = c('id'))
    road_grid_roadiness.dt <- data.table(road_grid_roadiness.sf)[, geometry := NULL]
    road_grid_roadiness.m <- melt(road_grid_roadiness.dt, id.vars = c('id'), variable.name = 'type.r', value.name = 'roadiness.grid')
    
    weights.dt <- aw_intersect(eds.c, source = road_grid_roadiness.sf, areaVar = "area") %>% data.table
    
    zips.a <- data.table(ed = eds.c$ed, ed.area = as.vector(st_area(eds.c)))
    
    weights.m <- merge(weights.dt, zips.a, by = c('ed'))
    weights.m[, areaWeight := area / ed.area]
    
    ncin.m.intersect <- merge(weights.m, road_grid_roadiness.m, by = c('id'), allow.cartesian = TRUE)
    ncin.m.intersect[, roadiness.a := areaWeight * roadiness.grid]
    ed.roadiness <- ncin.m.intersect[, .(roadiness = sum(roadiness.a)), by = .(ed, type.r)]
    
    ed.roadiness.c <- dcast(ed.roadiness, ed ~ type.r, value.var = 'roadiness')
    ed.roadiness.c[, city := c]
    
    return(ed.roadiness.c)
  }) %>% rbindlist

# Scale for interpretability
roadiness_by_ed[, roadiness2 := scale(leng.dist2)]

# merge together for plotting
ed.roadiness.all <- 
  merge( ed.in,
         roadiness_by_ed,
         by = c( 'city', 'ed') )

#================================================================================
#save dataset
#================================================================================
# Remove geometry from ed.roadiness.all
ed_roadiness_no_geom <- st_drop_geometry(ed.roadiness.all)

# Ensure ed column in both datasets is numeric
ed_roadiness_no_geom$ed <- as.numeric(ed_roadiness_no_geom$ed)
EDdata$ed <- as.numeric(EDdata$ed)

# Merge the two data tables by city and ed
merged_data <- ed_roadiness_no_geom %>%
  left_join(EDdata, by = c("city", "ed"))

# Remove geometry from ed.roadiness.all
merged_data_no_geom <- merged_data %>% select(-geometry)

# View the merged data
print(head(merged_data_no_geom))

write.csv(merged_data, "C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/Elisabeth_new_data/data/1940_NHGIS_new_cities_ED_no_LA.csv", row.names = FALSE)


plot(ed.roadiness.all[ed.roadiness.all$city == 'SeattleWA',])
roadiness_by_ed[city == 'SeattleWA'][which.max(road_leng)]

# Create a plot with all cities
legend_min_max <- c(-1, 1)

plots_list <- lapply(cities, function(c) {
  ggplot(ed.roadiness.all[ed.roadiness.all$city == c, ]) +
    geom_sf(aes(fill = roadiness2), color = NA) +
    scale_fill_viridis(limits = legend_min_max, oob = scales::squish, direction = 1) +
    facet_wrap(. ~ city) +
    theme_minimal() + 
    theme(axis.text = element_blank(),
          legend.position = 'none',
          strip.text = element_text(size = 8))
})

plot_all <- cowplot::plot_grid(plotlist = plots_list)
plot_all

# Write the roadiness data to a file
write.csv(roadiness_by_ed[, .(city, ed, road_leng, roadiness2)], './RoadinessData/roadiness_by_city_ED.csv')
