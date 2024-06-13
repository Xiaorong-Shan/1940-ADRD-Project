library( ggplot2)
library( data.table)
library( viridis)
library( tidyverse)
library( sf)
library(readxl)
library(raster)
library(fst)

generator2010ret <- read_excel( '/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/december_generator2022.xlsx',
                                      sheet = 3, skip = 2) %>% data.table
generator2010act <- read_excel( '/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/december_generator2022.xlsx',
                                      sheet = 1, skip = 2) %>% data.table

#==============================================================================================================
#filter 1940
# Use backticks for column names with spaces
generator1940ret <- generator2010ret[`Operating Year` <= 1940 & `Retirement Year` > 1940, ..cols_keep]
generator1940act <- generator2010act[`Operating Year` <= 1940, ..cols_keep]
generator1940 <- rbind( generator1940ret, generator1940act)




# plot total nameplate capacity by county
generator1940[ (`Energy Source Code` %in% c( 'BIT', 'SUB', 'LIG')),
               Coal := `Nameplate Capacity (MW)`]
generator1940[ (`Energy Source Code` %in% c( 'DFO', 'RFO')),
               Petroleum := `Nameplate Capacity (MW)`]
generator1940[ (`Energy Source Code` %in% c( 'BFG', 'NG', 'OG', 'PUR')),
               Gas := `Nameplate Capacity (MW)`]
generator1940[ (`Energy Source Code` %in% c( 'WAT', 'BLQ', 'WDS')),
               Renewable := `Nameplate Capacity (MW)`]
generator1940[ (`Energy Source Code` %in% c( 'PUR')),
               Other := `Nameplate Capacity (MW)`]

#count the energy code number
energy_count_1940 <- generator1940 %>%
  group_by(`Energy Source Code`) %>%
  summarise(Count = n())

energy_count_1940 <- energy_count_1940 %>%
  mutate(fuel_type = case_when(
    `Energy Source Code` %in% c('BIT', 'SUB', 'LIG') ~ 'Coal',
    `Energy Source Code` %in% c('DFO', 'RFO') ~ 'Petroleum',
    `Energy Source Code` %in% c('BFG', 'NG', 'OG') ~ 'Gas',
    `Energy Source Code` %in% c('WAT', 'BLQ', 'WDS') ~ 'Renewable',
    `Energy Source Code` %in% c('PUR') ~ 'Other'
  ))
  
energy_count_1940$year <- "1940"


# sum by fuel type and county
fueltype_names <- c( 'Coal', 'Petroleum', 'Gas', 'Renewable', 'Other')



# Summarizing data by lon, lat
generator1940_county <- generator1940[, lapply( .SD, sum, na.rm = T),
                                      .SDcols = fueltype_names, by = .(`Latitude`, `Longitude`)]
#melt the table for production_type
generator1940_county.m <-
    as.data.table(generator1940_county) %>%
    melt( id.vars = c('Longitude', 'Latitude'),
          variable.name = 'production_type',
          value.name = 'max_capacity')

# Keep only rows where max_capacity > 0
generator1940_county.m <- generator1940_county.m[max_capacity > 0]

generator1940_county.sf <- 
    st_as_sf(generator1940_county.m %>% 
             filter(!is.na(Longitude) & !is.na(Latitude)),
             coords = c('Longitude', 'Latitude'),
             crs = 'WGS84') 
 
print(generator1940_county.sf) 
#generator1940_county[, COUNTY := tolower( COUNTY)]


#==============================================================================================================
#filter 2010

generator2010ret <- generator2010ret[`Operating Year` <= 2010 & `Retirement Year` > 2010, ..cols_keep]
generator2010act <- generator2010act[`Operating Year` <= 2010, ..cols_keep]
generator2010 <- rbind( generator2010ret, generator2010act)


energy_count_2010 <- generator2010 %>%
  group_by(`Energy Source Code`) %>%
  summarise(Count = n())

energy_count_2010 <- energy_count_2010 %>%
  mutate(fuel_type = case_when(
    `Energy Source Code` %in% c('SUB', 'RC', 'BIT', 'LIG', 'WC') ~ 'Coal',
    `Energy Source Code` %in% c('DFO', 'JF', 'KER', 'RFO', 'WO', 'PC') ~ 'Petroleum',
    `Energy Source Code` %in% c('BFG', 'NG', 'OG', 'SGP', 'SGC', 'LFG', 'OBG') ~ 'Gas',
    `Energy Source Code` %in% c('WND', 'WAT',  'NUC', 'BLQ', 'SUN', 'WDS', 'GEO', 'WDL') ~ 'Renewable',
    `Energy Source Code` %in% c( 'AB', 'MSW', 'OBS', 'LFG', 'OBL') ~ 'Biomass',
    TRUE ~ 'Other'
  ))
  
energy_count_2010$year <- "2010"

generator2010[ (`Energy Source Code` %in% c( 'SUB', 'RC', 'BIT', 'LIG', 'WC')),
               Coal := `Nameplate Capacity (MW)`]
generator2010[ (`Energy Source Code` %in% c( 'DFO', 'JF', 'KER', 'RFO', 'WO', 'PC')),
               Petroleum := `Nameplate Capacity (MW)`]
generator2010[ (`Energy Source Code` %in% c( 'BFG', 'NG', 'OG', 'SGP', 'SGC', 'LFG', 'OBG')),
               Gas := `Nameplate Capacity (MW)`]
generator2010[ (`Energy Source Code` %in% c( 'WND', 'WAT',  'NUC', 'BLQ', 'SUN', 'WDS', 'GEO', 'WDL' )),
               Renewable := `Nameplate Capacity (MW)`]
generator2010[ (`Energy Source Code` %in% c( 'AB', 'MSW', 'OBS', 'LFG', 'OBL')),
               Biomass := `Nameplate Capacity (MW)`]               
generator2010[ (`Energy Source Code` %in% c( 'MWH', 'PUR', 'WH', 'TDF', 'OTH' )),
               Other := `Nameplate Capacity (MW)`] 
            
               
# sum by fuel type and county
fueltype_names <- c( 'Coal', 'Petroleum', 'Gas', 'Renewable', 'Biomass', 'Other')

# Summarizing data by lon, lat
generator2010_county <- generator2010[, lapply( .SD, sum, na.rm = T),
                                      .SDcols = fueltype_names, by = .(`Latitude`, `Longitude`)]
#melt the table for production_type
generator2010_county.m <-
    as.data.table(generator2010_county) %>%
    melt( id.vars = c('Longitude', 'Latitude'),
          variable.name = 'production_type',
          value.name = 'max_capacity')

# Keep only rows where max_capacity > 0
generator2010_county.m <- generator2010_county.m[max_capacity > 0]

generator2010_county.sf <- 
    st_as_sf(generator2010_county.m %>% 
             filter(!is.na(Longitude) & !is.na(Latitude)),
             coords = c('Longitude', 'Latitude'),
             crs = 'WGS84')

print(generator2010_county.sf) 

#==============================================================================================================
#rbin energy_count_1940 & energy_count_2010

energy_count_all <- bind_rows(energy_count_1940, energy_count_2010)

# Filter data to include only the specified fuel types
energy_count_filtered <- energy_count_all %>%
  filter(fuel_type %in% c('Coal', 'Gas', 'Petroleum', 'Renewable'))

# Calculate percentage
energy_count_filtered <- energy_count_filtered %>%
  group_by(year, fuel_type) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup()

# Create the ggplot
ggplot(energy_count_filtered, aes(y = fuel_type, x = Percentage, fill = `Energy Source Code`)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = scales::hue_pal()(length(unique(energy_count_filtered$`Energy Source Code`)))) +
  labs(
    title = "Percentage of Counts for Different Fuel Types by Year",
    x = "Percentage",
    y = "Fuel Type",
    fill = "Energy Source Code"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )


ggsave("/scratch/xshan2/R_Code/powerplant/pp_energy_p.pdf", height = 7, width = 10)

#==============================================================================================================
#sulfur content for 1940 & 2010
# Define sulfur content for each coal type
sulfur_content <- c("LIG" = 0.01, "SUB" = 0.02, "BIT" = 0.03, "RC" = 0.01, "WC" = 0.01)

# Filter data for coal types and year 1940
coal_1940 <- energy_count_filtered %>%
  filter(fuel_type == "Coal" & year == "1940")

# Calculate sulfur content for 1940
total_count_1940 <- sum(coal_1940$Count)
total_sulfur_1940 <- sum(coal_1940$Count * sapply(coal_1940$`Energy Source Code`, function(x) sulfur_content[x]))

average_sulfur_1940 <- total_sulfur_1940 / total_count_1940


# Filter data for coal types and year 2010
coal_2010 <- energy_count_filtered %>%
  filter(fuel_type == "Coal" & year == "2010")

# Calculate sulfur content for 2010
total_count_2010 <- sum(coal_2010$Count)
total_sulfur_2010 <- sum(coal_2010$Count * sapply(coal_2010$`Energy Source Code`, function(x) sulfur_content[x]))

average_sulfur_2010 <- total_sulfur_2010 / total_count_2010

# Output results
list(
  average_sulfur_1940 = average_sulfur_1940,
  average_sulfur_2010 = average_sulfur_2010
)











