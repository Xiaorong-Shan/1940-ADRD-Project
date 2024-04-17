library( data.table)
library( sf)
library( USAboundaries)
library( ggplot2)
library( viridis)
library( tidyverse)
library( sp)
library(raster)
library(fst)

# define a coordinate reference system
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m" 

auto_1940 <-read.csv("/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/input_data/simple_mv201.csv")
county_pop <- read.fst("/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/input_data/county_totPop_1940_2010.fst")
# Filter for rows where year is 1940
county_pop_1940 <- county_pop %>% filter(year == 1940)

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



#calculate the gasoline use per state, we downscale our dataset from national --> state --> county
#########################################total gasoline consumed --> vehicle registration --> population

GasUse_1940 = (26301863 - 365809) * 3785.4118 #NATIONAL: gasoline consume in 1940, data from sheet mf221.xlw, subtract loss gas(vaporation), 1000 galon = 3785.4118 kg



# sum the 1940 TOTAL gasoline consumed
Weight_1940 <- sum(auto_state_total.g$TOTAL, na.rm = TRUE)

# add an empty column named empty_column
auto_state_total.g[, 'state_gas'] = NA

#gasoline per state = national gasoline consumed * (state vehicle registration/total vehicle registration)
auto_state_total.g <- auto_state_total.g %>%
  mutate(
    state_gas = case_when(
      year == 1940 ~ TOTAL / Weight_1940 * GasUse_1940,
      TRUE ~ NA_real_   # You can replace NA_real_ with a specific value or calculation for other years if needed
    )
  )

#========================================================================
# now start to calculate GASOLINE per county
#========================================================================
# Ensure county_pop_1940 is a data.table
county_pop_1940 <- as.data.table(county_pop_1940)

# Subset to include only st_cnty_yr_id and totPop columns
county_pop_1940.dt <- county_pop_1940[, .(st_cnty_yr_id, totPop)]



auto_county_pop <- merge(auto_state_total.g, county_pop, by.x=c("jnhgis40"), by.y=c("st_cnty_yr_id") )

#frist calculate state population

auto_county_pop.dt <- auto_county_pop %>%
  group_by(state) %>%
  mutate(
    state_1940_pop = sum(totPop, na.rm = TRUE)
  ) %>%
  ungroup()

auto_county_pop.dt <- as.data.table(auto_county_pop.dt)

setnames(auto_county_pop.dt, "totPop", "county_1940_pop")

#gasoline per county = state gasoline consumed * (county population/state population)
# Convert integer columns to numeric to avoid overflow
auto_county_pop.dt$county_1940_pop <- as.numeric(auto_county_pop.dt$county_1940_pop)
auto_county_pop.dt$state_1940_pop <- as.numeric(auto_county_pop.dt$state_1940_pop)

# Then proceed with the operation
auto_county_pop.dt$county_gas <- auto_county_pop.dt$state_gas * 
  (auto_county_pop.dt$county_1940_pop / auto_county_pop.dt$state_1940_pop)


auto_county_total_no_geom <- st_drop_geometry(auto_county_pop.dt)

auto_county_total_no_geom.saved <- auto_county_total_no_geom[, .(decade, nhgisnam, statenam, state, county, county_gas)]
auto_county_total_no_geom.saved$production_type <- "Gasoline"
auto_county_total_no_geom.saved$data_type <- "automobile"

write.csv(auto_county_total_no_geom.saved, "/home/xshan2/HAQ_LAB/xshan2/R_Code/Auto_emissions/auto_county_1940_NHGIS.csv" )


ggplot( ) +
 # add the disperser grid
  geom_sf( data = auto_county_pop.dt,
           aes( fill =  county_gas, geometry = geometry),
           size = 0.1) +
 #geom_sf(data = states, alpha = 0.1, fill = 'white', size = 1) +
 coord_sf(xlim = c(-3000000, 2500000), ylim = c(-2000000, 1500000), expand = FALSE) +
  scale_fill_viridis(name = "1940 Gasoline Consumed", # Use RdYlGn palette
                          limits = c(0, 10e05),
                          breaks = c(0, 5e05, 10e05),
                          labels = c('0', '5e05', '10e05'),
                          oob = scales::squish) +
    theme_minimal() +
    theme(
      rect = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'bottom',
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 20),
      strip.text.x = element_text(size = 30, face = "bold"),
      legend.key.size = unit(1, "cm"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

ggsave("/scratch/xshan2/R_Code/Correlation/auto_1940_newpop.pdf") 
