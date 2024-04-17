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
# EPA natioanl emissions trends
#========================================================================
#different emission sources in 1940: 1 teragram = 1102311.3109 ton
#final unit: tons/year
# Constants for emission factors (kg per kg of gasoline consumed)
PM_factor <- 0.2
NOx_factor <- 1.3
VOCs_factor <- 4.5
CO_factor <- 22

# National gasoline consumption converted to kg for each pollutant
PM <- PM_factor * 1102311.3109
NOx <- NOx_factor * 1102311.3109
VOCs <- VOCs_factor * 1102311.3109
CO <- CO_factor * 1102311.3109

# Sum the 1940 TOTAL gasoline consumed
Weight_1940 <- sum(auto_state_total.g$TOTAL, na.rm = TRUE)

# Add empty columns for each pollutant at state level
auto_state_total.g$state_PM <- NA
auto_state_total.g$state_NOx <- NA
auto_state_total.g$state_VOCs <- NA
auto_state_total.g$state_CO <- NA

# Calculate state-specific pollutant emissions
auto_state_total.g <- auto_state_total.g %>%
  mutate(
    state_PM = ifelse(year == 1940, TOTAL / Weight_1940 * PM, NA_real_),
    state_NOx = ifelse(year == 1940, TOTAL / Weight_1940 * NOx, NA_real_),
    state_VOCs = ifelse(year == 1940, TOTAL / Weight_1940 * VOCs, NA_real_),
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
pollutants <- c("PM", "NOx", "VOCs", "CO")

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

auto_county_pop.dt <- st_as_sf(auto_county_pop.dt)
auto_county_total_no_geom <- st_drop_geometry(auto_county_pop.dt)

auto_county_total_no_geom.saved <- auto_county_total_no_geom[, .(decade, nhgisnam, statenam, state, county, county_PM, county_NOx, county_VOCs, county_CO)]
auto_county_total_no_geom.saved$production_type <- "TRAPs_emiss_EPA" #unit tons/year
auto_county_total_no_geom.saved$data_type <- "automobile"

write.csv(auto_county_total_no_geom.saved, "/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/auto_1940_emiss_EPA.csv" )



ggplot() + 
  geom_sf(data = auto_county_pop.dt, color = "grey", size = 0.5, aes(fill = county_VOCs)) +
  scale_fill_viridis(
    name = "VOCs emissions in 1940 (tons/year)",
    limits = c(0, 2000),
    breaks = c(0, 1000, 2000),
    labels = c('0', '1000', '2000'),  # Adjust the format here
    oob = scales::squish
  ) +
  expand_limits(fill = 0, color = 0) +
  coord_sf(xlim = c(-3000000, 2500000), ylim = c(-2000000, 1500000)) +
  facet_wrap(~year, ncol = 2) +
  theme_bw() +
  theme(
    rect = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size = 60, face = "bold"),
    legend.text = element_text(size = 60),  # Increase legend text size
    strip.text.x = element_text(size = 100, face = "bold"),
    legend.key.size = unit(2.8, "cm"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 


ggsave("/scratch/xshan2/R_Code/Automobiles/auto_VOCs_1940.pdf", height = 20, width = 40)


