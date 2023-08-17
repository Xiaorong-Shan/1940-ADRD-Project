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

# Convert the geometry column to a character representation
pp_exposure.m[, geom_char := st_as_text(geometry)]

# Sum the hyads_normalize values for each unique geometry
summed_data <- pp_exposure.m[, .(total_hyads_normalize = sum(hyads_normalize, na.rm = TRUE)), by = .(geom_char)]
