library( data.table)
library( sf)
library( USAboundaries)
library( ggplot2)
library( viridis)
library( tidyverse)
library( sp)

# define a coordinate reference system
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

auto_1940 <-read.csv("/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/Gasoline/simple_mv201.csv")
auto_2010 <- read.csv("/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/Gasoline/Auto_2010/vehicle_num_2010.csv")
county_pop <- read.csv("/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/02_2019fall_ADRD/RoadinessData/Gasoline/nhgis0041/nhgis0009_ts_nominal_county.csv")

#add year and rbind two list
auto_1940$year <- "1940"
auto_2010$year <- "2010"
auto_2010[,"STATE"] <- toupper(auto_2010[,]$STATE)

auto_state_total <- rbind(auto_1940, auto_2010)

## let's load in some US boundary data
us_states <- USAboundaries::us_states() 
us_counties <- USAboundaries::us_counties() 

## if we plot the locations + US coundaries using ggplot, 
## they will automatically be converted to a consistent crs,
## but I think it's best to do this manually just to make sure
us_states.tran <- st_transform( us_states, crs = 'WGS84')
us_states.tran[,"STATE"] <- toupper(us_states.tran[,]$name)

us_counties.tran <- st_transform( us_counties, crs = 'WGS84')

#delete the duplicate column
us_counties.tran <-us_counties.tran[,-9]

#merge the dataset
auto_state_total.g <- merge (us_states.tran, auto_state_total, by=c("STATE"))

# Format the ID column to have leading zeros
county_pop <- data.table(county_pop)
county_pop[, STATEFP := sprintf("%02d", as.numeric(STATEFP))]
county_pop[, COUNTYFP := sprintf("%03d", as.numeric(COUNTYFP))]

auto_county_pop <- merge(us_counties.tran, county_pop, by.x=c("statefp", "countyfp"), by.y=c("STATEFP", "COUNTYFP") )

#calculate the gasoline use per state, we downscale our dataset from national --> state --> county
#########################################total gasoline consumed --> vehicle registration --> population

GasUse_1940 = (26301863 - 365809) * 3785.4118 #NATIONAL: gasoline consume in 1940, data from sheet mf221.xlw, subtract loss gas(vaporation), 1000 galon = 3785.4118 kg

GasUse_2010 = 133725262 * 3785.4118 #2010 gasoline consume from mf27

# Filter the data for the year 1940 and sum the TOTAL column
Weight_1940 <- sum(auto_state_total.g[auto_state_total.g$year == "1940", ]$TOTAL)


Weight_2010 <- sum(auto_state_total.g[auto_state_total.g$year == "2010", ]$TOTAL)
# add an empty column named empty_column
auto_state_total.g[, 'state_gas'] = NA

#gasoline per state = national gasoline consumed * (state vehicle registration/total vehicle registration)
auto_state_total.g <- auto_state_total.g %>%
  mutate(
    state_gas = case_when(
      year == 1940 ~ TOTAL / Weight_1940 * GasUse_1940,
      year == 2010 ~ TOTAL / Weight_2010 * GasUse_2010,
      TRUE ~ NA_real_   # You can replace NA_real_ with a specific value or calculation for other years if needed
    )
  )

#========================================================================
# now start to calculate GASOLINE per county
#========================================================================
#frist calculate state population

auto_county_pop <- auto_county_pop %>%
  group_by(statefp) %>%
  mutate(
    state_1940_pop = sum(A00AA1940, na.rm = TRUE),
    state_2010_pop = sum(AV0AA2010, na.rm = TRUE)
  ) %>%
  ungroup()

auto_county_pop <- as.data.table(auto_county_pop)
#make it as another form
auto_county_1940 <- auto_county_pop[, !c("AV0AA2010", "B78AA2010", "state_2010_pop"), with = FALSE]
setnames(auto_county_1940, "state_1940_pop", "state_pop")
setnames(auto_county_1940, "A00AA1940", "county_pop")
auto_county_1940[, year := 1940]

auto_county_2010 <- auto_county_pop[, !c("A00AA1940", "B78AA2010", "state_1940_pop"), with = FALSE]
setnames(auto_county_2010, "state_2010_pop", "state_pop")
setnames(auto_county_2010, "AV0AA2010", "county_pop")
auto_county_2010[, year := 2010]

auto_county_total <- rbind(auto_county_1940, auto_county_2010)

#gasoline per county = state gasoline consumed * (county population/state population)
# Without using dplyr or tidyr
state_gas_data <- auto_state_total.g[, c("statefp", "state_gas", "year")]
state_gas_data <- st_drop_geometry(state_gas_data)


# Convert year columns to the same type
auto_county_total$year <- as.numeric(auto_county_total$year)
state_gas_data$year <- as.numeric(state_gas_data$year)


# Merge by "state_name"
auto_county_total <- merge(auto_county_total, state_gas_data, by = c("year","statefp"), all.x = TRUE)

# Convert integer columns to numeric to avoid overflow
auto_county_total$county_pop <- as.numeric(auto_county_total$county_pop)
auto_county_total$state_pop <- as.numeric(auto_county_total$state_pop)

# Then proceed with the operation
auto_county_total$county_gas <- auto_county_total$state_gas * 
  (auto_county_total$county_pop / auto_county_total$state_pop)

auto_county_total <- st_as_sf(auto_county_total)
auto_county_total <- st_transform(auto_county_total, crs = p4s)

auto_state_total.g <- st_as_sf(auto_state_total.g)
auto_state_total.g <- st_transform(auto_state_total.g, crs = p4s)

## 1940 motor-cycles registrations
p1 <- ggplot() + 
  geom_sf(data = auto_state_total.g, color = "grey", size = 1, aes(fill = TOTAL)) +
  scale_fill_viridis(
    name = "Vehicle Registration",
    limits = c(0, 5e+06),
    breaks = c(0, 2.5e+06, 5e+06),
    labels = c('0', '2.5e+06', '5e+06'),  # Adjust the format here
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

p1

p2 <- ggplot( ) + 
  geom_sf( data = auto_county_total, color = "grey", aes( fill = county_gas)) +
  # set the x and y limits
  scale_fill_viridis(name = ("Gasoline (kg/year)"),
                     limits = c(0, 1e+08),
                     breaks = c(0, 5e+07, 1e+08),
                     labels = c('0', '5e+07', '1e+08'),
                     oob = scales::squish) +
  expand_limits(fill = 0, color = 0) +
  # Set boundaries over mainland of US
  coord_sf(xlim = c(-3000000, 2500000), ylim = c(-2000000, 1500000)) +
  # Add facets with custom labels
  facet_wrap(~year, ncol = 2) +
  theme_bw()+
  theme(
    rect = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size = 60, face = "bold"),
    legend.text = element_text(size = 60),  # Increase legend text size
    strip.text.x = element_text(size = 100, face = "bold"),
    legend.key.size = unit(2.5, "cm"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 


p2


