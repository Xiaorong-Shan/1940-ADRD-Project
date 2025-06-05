library( data.table)
library( sf)
library( USAboundaries)
library( ggplot2)
library( viridis)
library( tidyverse)
library( sp)
library(raster)
# define a coordinate reference system
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m" 

auto_1940 <-read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/input_data/simple_mv201.csv") #state vehicle registration
county_pop <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/input_data/nhgis0009_ts_nominal_county.csv") #county population

#add year and rbind two list
auto_1940$year <- "1940"

#create a state table with FPIS
state_fips_lookup <- data.frame(
  STATE = c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
            "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", 
            "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", 
            "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", 
            "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", 
            "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", 
            "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", 
            "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", 
            "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", 
            "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", 
            "WEST VIRGINIA", "WISCONSIN", "WYOMING"),
  state = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", 
           "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", 
           "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
           "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", 
           "49", "50", "51", "53", "54", "55", "56")
)
# Convert 2-digit FIPS to 3-digit by appending '0'
state_fips_lookup$state <- paste0(state_fips_lookup$state, "0")

# View the lookup table
head(state_fips_lookup)

# Merge the state FIPS codes into your existing data based on state names
auto_state_total_fips <- merge(auto_1940, state_fips_lookup, by = "STATE", all.x = TRUE)
# View the result
head(auto_state_total_fips)


#========================================================================
# load the 1940 shape files
#========================================================================
#read the county shp file
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_1940_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_1940.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))
                                  
                                  
## if we plot the locations + US coundaries using ggplot, 
## they will automatically be converted to a consistent crs,

auto_state_total.g <- merge (roadiness.trans.county, auto_state_total_fips, by="state")

#========================================================================
# distance-based EF
#========================================================================
#different emission sources in 1940: 1 teragram = 1102311.3109 ton
#unit: kg/year
# Constants for emission factors (kg per kg of gasoline consumed)
HC_factor <- 8.3
NOx_factor <- 3.4
CO_factor <- 53.4
PM10_factor <- 0.8

# National gasoline consumption converted to kg for each pollutant
HC <- HC_factor * 47003000000
NOx <- NOx_factor * 47003000000
CO <- CO_factor * 47003000000
PM10 <- PM10_factor * 47003000000

# Sum the 1940 TOTAL gasoline consumed
Weight_1940 <- sum(auto_state_total.g$TOTAL, na.rm = TRUE)

# Add empty columns for each pollutant at state level
auto_state_total.g$state_HC <- NA
auto_state_total.g$state_NOx <- NA
auto_state_total.g$state_CO <- NA
auto_state_total.g$state_PM10 <- NA

# Calculate state-specific pollutant emissions
auto_state_total.g <- auto_state_total.g %>%
  mutate(
    state_HC = ifelse(year == 1940, TOTAL / Weight_1940 * HC, NA_real_),
    state_NOx = ifelse(year == 1940, TOTAL / Weight_1940 * NOx, NA_real_),
    state_PM10 = ifelse(year == 1940, TOTAL / Weight_1940 * PM10, NA_real_),
    state_CO = ifelse(year == 1940, TOTAL / Weight_1940 * CO, NA_real_)
  )


#========================================================================
# now start to calculate emissions per county
#========================================================================
# Format the ID column to have leading zeros
county_pop <- data.table(county_pop)
county_pop[, STATEFP := sprintf("%02d", as.numeric(STATEFP))]
county_pop[, COUNTYFP := sprintf("%03d", as.numeric(COUNTYFP))]

# Convert 2-digit state to 3-digit, 3-digit county to 4-digit by appending '0'
county_pop$STATEFP <- paste0(county_pop$STATEFP, "0")
county_pop$COUNTYFP <- paste0(county_pop$COUNTYFP, "0")

auto_county_pop <- merge(auto_state_total.g, county_pop, by.x=c("state", "county"), by.y=c("STATEFP", "COUNTYFP") )

#frist calculate state population

auto_county_pop.dt <- auto_county_pop %>%
  group_by(state) %>%
  mutate(
    state_1940_pop = sum(A00AA1940, na.rm = TRUE)
  ) %>%
  ungroup()

auto_county_pop.dt <- as.data.table(auto_county_pop.dt)

setnames(auto_county_pop.dt, "A00AA1940", "county_1940_pop")


# List of pollutants to loop through
pollutants <- c("PM10", "NOx", "HC", "CO")

# Apply county-level calculations for each pollutant
for (pollutant in pollutants) {
  # Create dynamic column names for county and state levels
  county_col_name <- paste("county", pollutant, sep = "_")
  state_col_name <- paste("state", pollutant, sep = "_")
  
  # Calculate county-level emissions based on the proportion of population
  auto_county_pop.dt[[county_col_name]] <- auto_county_pop.dt[[state_col_name]] * 
    (auto_county_pop.dt$county_1940_pop / auto_county_pop.dt$state_1940_pop)
}

# View the updated dataframe
print(auto_county_pop.dt)

#===================================================================================================================
#weighted by county area
#unite: kg/km^2
#===================================================================================================================
# Assuming your data is already an sf object named auto_county_pop.dt
# Ensure area is computed
auto_county_pop.dt[, area_km2 := as.numeric(st_area(geometry)) / 1e6]

# Compute area-weighted emissions
auto_county_pop.dt[, county_HC_areaWeighted   := county_HC / area_km2]
auto_county_pop.dt[, county_PM10_areaWeighted := county_PM10 / area_km2]
auto_county_pop.dt[, county_CO_areaWeighted   := county_CO / area_km2]
auto_county_pop.dt[, county_NOx_areaWeighted  := county_NOx / area_km2]

# Print selected columns
auto_county_pop.dt[, .(GISJOIN, 
                       county_HC_areaWeighted, 
                       county_PM10_areaWeighted, 
                       county_CO_areaWeighted, 
                       county_NOx_areaWeighted)]

#===================================================================================================================
#save the file
auto_county_pop.dt <- st_as_sf(auto_county_pop.dt)
auto_county_total_no_geom <- st_drop_geometry(auto_county_pop.dt)

auto_county_total_no_geom.saved <- auto_county_total_no_geom[, .(decade, nhgisnam, statenam, state, county, county_HC_areaWeighted, county_PM10_areaWeighted, county_CO_areaWeighted, county_NOx_areaWeighted)]

#transpose the table
# Ensure you're using data.table syntax
auto_county_total_no_geom.saved.m <- as.data.table(auto_county_total_no_geom.saved)[
  ,
  melt(.SD,
       id.vars = c('decade', 'state', 'county', 'statenam', 'nhgisnam'),
       variable.name = 'emission_type',
       value.name = 'emission_value')
]

# Replace emission_type with the second word (e.g., HC, CO, NOx, PM10)
auto_county_total_no_geom.saved.m[, emission_type := sub("^[^_]+_([^_]+)_.*$", "\\1", emission_type)]

# Convert kg to metric tons
auto_county_total_no_geom.saved.m[, emission_value := emission_value / 1000] #unit: tons/km^3


#write.csv(auto_county_total_no_geom.saved.m, "/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/auto_county_1940_DistanceEF_NHGIS.csv" )
#write.csv(auto_county_total_no_geom.saved.m, "/scratch/xshan2/R_Code/Automobiles/auto_county_1940_DistanceEF_NHGIS.csv" )

#====================================================================================================================
#plot
# Ensure both are data.tables
setDT(auto_county_total_no_geom.saved.m)
setDT(roadiness.trans.county)

# Merge by 'state' and 'county'
merged_data <- merge(
  auto_county_total_no_geom.saved.m,
  roadiness.trans.county,
  by = c("state", "county"),
  all.x = TRUE  # left join to keep all rows from auto_county_total_no_geom.saved.m
)


# Summary statistics by emission type
emission_summary <- merged_data[, .(
  min    = min(emission_value, na.rm = TRUE),
  q1     = quantile(emission_value, 0.25, na.rm = TRUE),
  median = median(emission_value, na.rm = TRUE),
  mean   = mean(emission_value, na.rm = TRUE),
  q3     = quantile(emission_value, 0.75, na.rm = TRUE),
  max    = max(emission_value, na.rm = TRUE)
), by = emission_type]

print(emission_summary)

# Create the plot
colors <- c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#e34a33")

# Filter for CO only
co_data <- merged_data[emission_type == "CO"]

# Now plot
p <- ggplot() +
  geom_sf(data = co_data, aes(fill = emission_value, geometry = geometry), size = 0.1) +
  coord_sf(xlim = c(-3000000, 2500000), ylim = c(-2000000, 1500000), expand = FALSE) +
  scale_fill_gradientn(
    colors = colors,
    limits = c(0, 5),
    breaks = c(0, 2.5, 5),
    labels = c('0', '2.5', '5'),
    oob = scales::squish,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      barwidth = 20,
      barheight = 1
    )
  ) +
  theme_minimal() +
  labs(title = "1940 CO") + 
  theme(
    rect = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    legend.title = element_text(size = 16),  # Make the legend title bigger
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 30, face = "bold"),
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 25, hjust = 0.5),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("/scratch/xshan2/R_Code/Correlation/auto_1940_distance_EF_weighted_CO.pdf", plot = p, device = "pdf", width = 7, height = 5)
  

colors <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
