library(ggplot2)
library(USAboundariesData)
library(pryr)
library(fst)
library(data.table)
library(sf)
library(viridis)
library(raster)

#read the summed 10-year hyads exposure, summed coal, petroleum, gas and reneable together for the same county
pp_exposure.m <- st_read("/home/xshan2/HAQ_LAB/xshan2/R_Code/powerplant/pp_hyads_shp/pp_exposure_m.shp")

setDT(pp_exposure.m)

#sum data by same NHGISNA and STATENA
summed_data <- pp_exposure.m[, .(total_hyds_nr = sum(hyds_nr, na.rm = TRUE)), by = .(NHGISNA, STATENA)]

