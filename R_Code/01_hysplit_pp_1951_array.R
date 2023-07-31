# srun -p test --mem 15g -t 0-01:00 -c 1 -N 1 --pty /bin/bash

library(disperseR) # our package
library(ncdf4)
library(data.table)
library(tidyverse)
library(parallel)
library(sf)
library(viridis)
library(ggplot2)
library(scales)
library(ggsn)
library(gridExtra)
library(ggmap)
library(ggrepel)
library(fst)
library( USAboundariesData)

disperseR::create_dirs('/projects/HAQ_LAB/xshan2/disperseR_Linux')

proc_dir <- '/scratch/xshan2/disperseR_Linux/main/process'

# define a coordinate reference system
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

years.run <- 1951

# download data
disperseR::get_data(data = "all",
                    start.year = "1951",
                    start.month = "01",
                    end.year = "1951",
                    end.month = "12")

#read the facilities' locations                    
unit.facility <- read.csv("/scratch/xshan2/R_Code/disperseR/generator1940_gen.csv")                    

unit.facility <- as.data.table(unit.facility)

unit.facility.c <- dcast( unit.facility,
              X + Y ~ variable,
              fun.aggregate = sum,
              value.var = 'value',
              na.rm = TRUE)

unit.facility.c$Height <- 374.17
unit.facility.c$year <- 1951
unit.facility.c$uID <- 1:nrow(unit.facility.c)
unit.facility.c$ID <- 1:nrow(unit.facility.c)
unit.facility.c$uID <- as.character(unit.facility.c$uID)
unit.facility.c$ID <- as.character(unit.facility.c$ID)

# X --> longitude, Y --> latitude
colnames(unit.facility.c)[1] <- "Longitude"
colnames(unit.facility.c)[2] <- "Latitude"

input_refs <- disperseR::define_inputs(units = unit.facility.c,
                                       startday = '1951-01-01',
                                       endday = '1951-12-31',
                                       start.hours =  12,
                                       duration = 24 * 7)

# select specific 
array_num <- as.numeric( Sys.getenv("SLURM_ARRAY_TASK_ID"))
array_num <- ifelse( array_num == '' | is.na( array_num), 1, array_num)

# total array numbers
numbers <- 100
# Calculate the number of tasks and determine the index for each task
total_tasks <- nrow(input_refs)
tasks_per_job <- ceiling(total_tasks / numbers)
start_index <- (array_num - 1) * tasks_per_job + 1
end_index <- min(array_num * tasks_per_job, total_tasks)

set.seed( array_num)

#print(start_index:end_index)

#print(input_refs[start_index:end_index])

hysp_raw <- disperseR::run_disperser_parallel(input.refs = input_refs[start_index:end_index],
                                              pbl.height = pblheight,
                                              species = 'so2',
                                              proc_dir = proc_dir,
                                              overwrite = TRUE, ## FALSE BY DEFAULT
                                              npart = 100,
                                              keep.hysplit.files = FALSE,
                                              mc.cores=1) #parallel::detectCores())


# hysp_raw <- disperseR::run_disperser_parallel(input.refs = input_refs[c( array_num, sample( 1:nrow( unitsuse)))],
#                                               pbl.height = pblheight,
#                                               species = 'so2',
#                                               proc_dir = proc_dir,
#                                               overwrite = TRUE, ## FALSE BY DEFAULT
#                                               npart = 100,
#                                               keep.hysplit.files = FALSE,
#                                               mc.cores=1) #parallel::detectCores())
                                              
                                              

# yearmons <- disperseR::get_yearmon(start.year = as( years.run[1], 'character'),
#                                     start.month = "01",
#                                     end.year = as( years.run[length( years.run)], 'character'),
#                                     end.month = '02')


# unitsuse <- units.CAMP[!duplicated(ID)]
# 
# linked_grid <- disperseR::link_all_units(units.run = unitsuse[c( array_num, sample( 1:nrow( unitsuse)))],
#                                          link.to = 'grids',
#                                          mc.cores = 1,
#                                          year.mons = yearmons,
#                                          pbl.height = pblheight,
#                                          crosswalk. = crosswalk,
#                                          duration.run.hours = 24*7,
#                                          res.link = 36000,
#                                          overwrite = TRUE,
#                                          pbl.trim = FALSE,
# 					                     crop.usa = TRUE,
#                                          return.linked.data = TRUE)
# 
# unique( linked_grid$month)
# 
# #Combine all results into RData file
# combined_gridlinks <- disperseR::combine_daily_links(
#   month_YYYYMMs = yearmons,
#   link.to = 'grids',
#   filename = 'hyads_vig_unwgted_grids.RData')
# 
# 
# 
# exp_day_unit_grids <- disperseR::calculate_daily_exposure(
#   year.E = 1951,
#   year.D = 1951,
#   link.to = 'grids',
#   pollutant = 'SO2.tons',
#   rda_file = file.path(rdata_dir, "hyads_vig_unwgted_grids.RData"),
#   exp_dir = exp_dir,
#   units.mo = PP.units,
#   source.agg = 'unit',
#   return.daily.data = T)
# 
# unique(exp_day_unit_grids$yearmonth)
# 
# exp_day_unit_grids.c <- dcast( exp_day_unit_grids,
#                         x + y ~ yearmonth, 
#                         fun.aggregate = sum,
#                         value.var = 'hyads',
#                         na.rm = TRUE)
# 
# # create raster object
# campfire_day.r <- rasterFromXYZ( exp_day_unit_grids.c,
#                                  crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
# 
# 
# 
# # create sf polygon object
# campfire_day.sp <- rasterToPolygons( campfire_day.r)
# campfire_day.sf <- st_as_sf( campfire_day.sp)
# 
# 
# 
# # melt back to long format
# campfire_day.sf.m <-
#   as.data.table( campfire_day.sf) %>%
#   melt( id.vars = 'geometry',
#         variable.name = 'month',
#         value.name = 'N')
# 
# 
# 
# # NA for disperseR means 0
# campfire_day.sf.m[is.na( N), N := 0]
# 
# # download some US data
# states <- us_states()
# 
# campfire_day.sf.m$geometry <-
#   st_transform( campfire_day.sf.m$geometry,
#                 crs = st_crs( states))
# 
# 




