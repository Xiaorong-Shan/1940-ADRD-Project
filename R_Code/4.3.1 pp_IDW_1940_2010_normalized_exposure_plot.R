library( ggplot2)
library( data.table)
library( viridis)
library( tidyverse)
library( sf)
library(readxl)
library(raster)
library(fst)
library(USAboundaries)

pp_idw.dt.1940 <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/2010_1940_EF_SO2_Emission/1940_pp_county_idw_EIA_all_NHGIS.csv")

pp_idw.dt.2010 <- read.csv( "/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/2010_eGRID_emission/2010_pp_county_idw_eGRID_all_NHGIS.csv")


names(pp_idw.dt.2010)[names(pp_idw.dt.2010) == "statefp10"] <- "state"
names(pp_idw.dt.2010)[names(pp_idw.dt.2010) == "countyfp10"] <- "county"

pp_idw.dt.1940$state <- as.character(pp_idw.dt.1940$state)
pp_idw.dt.2010$state <- as.character(pp_idw.dt.2010$state)

pp_idw.dt.1940$county <- as.character(pp_idw.dt.1940$county)
pp_idw.dt.2010$county <- as.character(pp_idw.dt.2010$county)

pp_idw.dt.1940$year <- as.character(pp_idw.dt.1940$year)
pp_idw.dt.2010$year <- as.character(pp_idw.dt.2010$year)

pp_idw.dt <-  bind_rows(pp_idw.dt.1940, pp_idw.dt.2010)

pp_idw.dt <- pp_idw.dt[, !names(pp_idw.dt) %in% "X"]



#================================================================================  
#normalize
MEAN <- mean(pp_idw.dt$pp_idw)

SD <- sd(pp_idw.dt$pp_idw)

pp_idw.dt$normalize <- (pp_idw.dt$pp_idw - MEAN)/SD
  
  
write.csv(pp_idw.dt, "/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_idw/1940_2010_pp_idw_normalized.csv") 
  

#================================================================================
#start to plot them
#================================================================================    

#================================================================================   
#1940
pp_1940_plot <-pp_idw.dt %>%
  filter(year %in% c("1940"))
  

p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m" 
  
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_1940_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_1940.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))


pp_1940_plot <- pp_1940_plot %>%
  mutate(state = sprintf("%03d", as.numeric(state)))

pp_1940_plot <- pp_1940_plot  %>%
  mutate(county = sprintf("%04d", as.numeric(county)))

pp_1940_plot.sf <- merge(pp_1940_plot, roadiness.trans.county, 
                        by= c("state", "county"))

# Create the plot
colors <- c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#e34a33")

state_boundaries <- pp_1940_plot.sf %>%
  group_by(statenam) %>%
  summarise(geometry = st_union(geometry))

p <- ggplot() +
  # Add the disperser grid
  geom_sf(data = pp_1940_plot.sf %>% filter(production_type == "Renewable"), aes(fill = normalize, geometry = geometry), size = 0.1) +
  coord_sf(xlim = c(-3000000, 2500000), ylim = c(-2000000, 1500000), expand = FALSE) +
  scale_fill_gradientn(
    colors = colors,
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c('-1', '-0.5', '0', '0.5', '1'),
    oob = scales::squish,
    name = "Normalized exposure",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      label.position = "bottom",
      barwidth = 20,
      barheight = 1
    )
  ) +
  theme_minimal() +
  theme(
    rect = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 30, face = "bold"),
    legend.key.size = unit(1, "cm"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5) 
  )

  
ggsave("/scratch/xshan2/R_Code/powerplant/pp_renewable_1940.pdf", plot = p, device = "pdf", width = 7, height = 5)
  
  
  
#================================================================================   
#2010 
pp_2010_plot <-pp_idw.dt %>%
  filter(year %in% c("2010"))
  

p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m" 
  
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_2010_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_2010.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))
  
names(pp_2010_plot)[names(pp_2010_plot) == "state"] <- "statefp10"
names(pp_2010_plot)[names(pp_2010_plot) == "county"] <- "countyfp10"  
    
  
pp_2010_plot <- pp_2010_plot %>%
  mutate(statefp10 = sprintf("%02d", as.numeric(statefp10)))

pp_2010_plot <- pp_2010_plot  %>%
  mutate(countyfp10 = sprintf("%03d", as.numeric(countyfp10)))

pp_2010_plot.sf <- merge(pp_2010_plot, roadiness.trans.county, 
                        by= c("statefp10", "countyfp10"))
    
p <- ggplot() +
  # Add the disperser grid
  geom_sf(data = pp_2010_plot.sf %>% filter(production_type == "All"), aes(fill = normalize, geometry = geometry), size = 0.1) +
  coord_sf(xlim = c(-3000000, 2500000), ylim = c(-2000000, 1500000), expand = FALSE) +
  scale_fill_gradientn(
    colors = colors,
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c('-1', '-0.5', '0', '0.5', '1'),
    oob = scales::squish,
    name = "Normalized exposure",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      label.position = "bottom",
      barwidth = 20,
      barheight = 1
    )
  ) +
  theme_minimal() +
  theme(
    rect = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 30, face = "bold"),
    legend.key.size = unit(1, "cm"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, hjust = 0.5) 
  )

  
ggsave("/scratch/xshan2/R_Code/powerplant/pp_all_2010.pdf", plot = p, device = "pdf", width = 7, height = 5)
  
    
