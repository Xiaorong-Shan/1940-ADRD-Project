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

auto_2010 <-read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/input_data/vehicle_num_2010.csv")
county_pop <- read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/input_data/nhgis0009_ts_nominal_county.csv")

#add year and rbind two list
auto_2010$year <- "2010"

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
  statefp10 = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", 
           "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", 
           "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
           "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", 
           "49", "50", "51", "53", "54", "55", "56")
)

# View the lookup table
head(state_fips_lookup)

# Merge the state FIPS codes into your existing data based on state names
auto_state_total_fips <- merge(auto_2010, state_fips_lookup, by = "STATE", all.x = TRUE)
# View the result
head(auto_state_total_fips)

#========================================================================
# load the 2010 shape files
#========================================================================
#read the county shp file
dir.in <- "/projects/HAQ_LAB/xshan2/R_Code/Roadiness/FINAL_2010_county_shapefile_clean"

##This shp file has lots of missing data
# you can find all the census tract data in https://www.nhgis.org/
roadiness_county <-
    st_read( file.path( dir.in, 'counties_contiguous_2010.shp'))

roadiness.trans.county <- st_transform(roadiness_county,
                                  crs = crs(p4s))
## if we plot the locations + US coundaries using ggplot, 
## they will automatically be converted to a consistent crs,

auto_state_total.g <- merge (roadiness.trans.county, auto_state_total_fips, by="statefp10")



#calculate the gasoline use per state, we downscale our dataset from national --> state --> county
#########################################total gasoline consumed --> vehicle registration --> population

GasUse_2010 = 133725262 * 3785.4118 #2010 gasoline consume from mf27


# sum the 1940 TOTAL gasoline consumed
Weight_2010 <- sum(auto_state_total.g$TOTAL, na.rm = TRUE)

# add an empty column named empty_column
auto_state_total.g[, 'state_gas'] = NA

#gasoline per state = national gasoline consumed * (state vehicle registration/total vehicle registration)
auto_state_total.g <- auto_state_total.g %>%
  mutate(
    state_gas = case_when(
      year == 2010 ~ TOTAL / Weight_2010 * GasUse_2010,
      TRUE ~ NA_real_   # You can replace NA_real_ with a specific value or calculation for other years if needed
    )
  )

#========================================================================
# now start to calculate GASOLINE per county
#========================================================================
# Format the ID column to have leading zeros
county_pop <- data.table(county_pop)
county_pop[, STATEFP := sprintf("%02d", as.numeric(STATEFP))]
county_pop[, COUNTYFP := sprintf("%03d", as.numeric(COUNTYFP))]

auto_county_pop <- merge(auto_state_total.g, county_pop, by.x=c("statefp10", "countyfp10"), by.y=c("STATEFP", "COUNTYFP") )

#frist calculate state population

auto_county_pop.dt <- auto_county_pop %>%
  group_by(statefp10) %>%
  mutate(
    state_2010_pop = sum(AV0AA2010, na.rm = TRUE)
  ) %>%
  ungroup()

auto_county_pop.dt <- as.data.table(auto_county_pop.dt)

setnames(auto_county_pop.dt, "AV0AA2010", "county_2010_pop")

#gasoline per county = state gasoline consumed * (county population/state population)
# Convert integer columns to numeric to avoid overflow
auto_county_pop.dt$county_2010_pop <- as.numeric(auto_county_pop.dt$county_2010_pop)
auto_county_pop.dt$state_2010_pop <- as.numeric(auto_county_pop.dt$state_2010_pop)

# Then proceed with the operation
auto_county_pop.dt$county_gas <- auto_county_pop.dt$state_gas * 
  (auto_county_pop.dt$county_2010_pop / auto_county_pop.dt$state_2010_pop)


auto_county_total_no_geom.saved <- auto_county_pop.dt[, .(statefp10, countyfp10, STATE.y, name10, county_gas)]
auto_county_total_no_geom.saved$decade <- "2010"
setnames(auto_county_total_no_geom.saved, "STATE.y", "STATE")
auto_county_total_no_geom.saved$production_type <- "Gasoline"
auto_county_total_no_geom.saved$data_type <- "automobile"

write.csv(auto_county_total_no_geom.saved, "/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/auto_county_2010_NHGIS.csv" )
