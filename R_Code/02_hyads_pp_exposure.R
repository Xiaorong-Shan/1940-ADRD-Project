library(ncdf4)
library(data.table)
library(tidyverse)
library(parallel)
library(sf)
library(sp)
library(viridis)
library(ggplot2)
library(scales)
library(ggsn)
library(gridExtra)
library(ggmap)
library(ggrepel)
library(fst)
library(USAboundariesData)
library(raster)

# define a coordinate reference system
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

p4string <- p4s

#resolution
res.link. <- 12000

#read the input unit file
unit.facility <- read.csv("/scratch/xshan2/R_Code/disperseR/generator1940_gen.csv")                    

#merge with unit.facility.c later
#unit.uID <- unit.facility[, c('UTILITY_ID', 'X', 'Y')]

unit.facility <- as.data.table(unit.facility)

unit.facility.c <- dcast( unit.facility,
              X + Y ~ variable,
              fun.aggregate = sum,
              value.var = 'value',
              na.rm = TRUE)

#get the uID
#unit.facility.t <- merge(unit.uID, unit.facility.c, by=c('X', 'Y'))

unit.facility.c$Height <- 374.17
unit.facility.c$year <- 1951
unit.facility.c$uID <- 1:nrow(unit.facility.c)
unit.facility.c$ID <- 1:nrow(unit.facility.c)
unit.facility.c$uID <- as.character(unit.facility.c$uID)
unit.facility.c$ID <- as.character(unit.facility.c$ID)

# X --> longitude, Y --> latitude
colnames(unit.facility.c)[1] <- "Longitude"
colnames(unit.facility.c)[2] <- "Latitude"

#colnames(unit.facility.t)[3] <- "uID"

unit.facility.c <- as.data.table(unit.facility.c)

# find unique combos of Latitude, Longitude, and Height
unitslatlonh <- unique( unit.facility.c[ ,.( Latitude, Longitude, Height, year)] )
unitslatlonh[, unit_run_ref:=1:nrow( unitslatlonh)]
unitsrun_trim <- merge( unit.facility.c, unitslatlonh)[ !duplicated( unit_run_ref)]

years.run <- 1951

start.date <- '1951-01-01'
end.date <- '1951-12-31'

# define yearmons for link to zips/grids/counties
vec_filedates <-
      seq.Date(
        from = as.Date( start.date), #previously defines as: as.Date( start.date) - ceiling( duration.run.hours / 24),
        to = as.Date( end.date),
        by = '1 day'
      )
      
#hysplit file directory in yearmonth   
hysp_dir <- '/projects/HAQ_LAB/xshan2/disperseR_Linux/main/output/hysplit'

hysp_dir.path <-
      file.path( hysp_dir,
                 unique( paste( year( vec_filedates),
                                formatC( month( vec_filedates), width = 2, flag = '0'),
                                sep = '/')))

#keep the unit for each facility, we need to exposure*each facility's capacity in the end
#unit <- unitsrun_trim 
#for(i in 1:nrow(unitsrun_trim)){
function_date = function(unit,
                         unit_table) {
  
  unit.c <- unit_table[ID == unit]
    
    pattern.file <-
            paste0( '_',
            gsub( '[*]', '[*]', unit.c$ID),
            '_(',
            paste(vec_filedates, collapse = '|'),
            ').*\\.fst$'
                )
 
    #read the file by different ID                               
    files.read <-
      list.files( path = hysp_dir.path,
                  pattern = pattern.file,
                  recursive = F,
                  full.names = T)                            
                  
    ## read in the files
    l <- lapply( files.read, read.fst, as.data.table = TRUE)
    # Combine all parcels into single data table
    d <- rbindlist(l)
    
    d_trim <- d
    xy <- d[, .(lon, lat)]
    
    spdf.in <- SpatialPointsDataFrame( coords = xy,
                                    data = d,
                                    proj4string = CRS( "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
                                    
    spdf <- spTransform( spdf.in, p4string)

    # create raster with resolution res.link.
    e <- extent(spdf)
    e@xmin <- floor(  e@xmin / res.link.) * res.link.
    e@ymin <- floor(  e@ymin / res.link.) * res.link.
    e@xmax <- ceiling(e@xmax / res.link.) * res.link.
    e@ymax <- ceiling(e@ymax / res.link.) * res.link.
    r <- raster( ext = e, resolution = res.link., crs = CRS( proj4string( spdf)))
    values( r) <- NA

    # count number of particles in each cell,
    # find original raster cells, divide number by pbl
    cells <- cellFromXY( r, spdf)
    tab <- table( cells)

    r[as.numeric( names( tab))] <- tab

    # crop around point locations for faster extracting
    r2 <- crop( trim(r,
                 padding = 1),
                  e)

    xyz <- data.table( rasterToPoints(r2))
    names(xyz)[3] <- 'N' #number of parcel
    
    #calcualte exposure = N * facility capacity
    xyz$exp_coal <- xyz$N * unit.c$Coal
    xyz$exp_gas <- xyz$N * unit.c$Gas
    xyz$exp_petroleum <- xyz$N * unit.c$Petroleum
    xyz$exp_renewable <- xyz$N * unit.c$Renewable
    
    ##  convert to polygons for faster extracting
    #r3 <- rasterToPolygons(r2)
    #ID <- gsub("^_([0-9]+).*", "\\1", ID_number) 
    
    file_name <- paste0(unit, "_", years.run, "_hyads_exposure.fst")
    
    save_hyads_dir <- "/projects/HAQ_LAB/xshan2/disperseR_Linux/main/output/hyads"
    
    path_name <- paste0(save_hyads_dir, "/", file_name)
    
    write.fst(xyz, path_name)
    
    return(xyz)
            
        }

lapply(unitsrun_trim$ID, function_date, unit_table=as.data.table(unitsrun_trim))


#we have different capacity for different fuel type which include:Coal Gas Petroleum Renewable





                                
                                