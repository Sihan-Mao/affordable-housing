library(RCurl)
library(gdalUtils)
library(rgdal)
library(rts)
library(raster)
library(dplyr)
library(devtools)
library(purrr)
library(lubridate)
library(sp)
library(sf)
setwd("D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif_day")



MAIACDateFromName <- function(n) {
  
  #Get date from file name 
  date.substring <- strsplit(n, ".", fixed = T)[[1]][2]
  
  #Remove extra A
  date.substring <- gsub("A", "", date.substring)
  
  #Extract year from File Name
  year <- as.integer(substring(date.substring, first = 0, last = 4))
  
  #Use year to create origin for julian date
  origin <- paste(year, "-01-01", sep = "")
  
  #Extract julian date (Subtract 1 for correct date)
  julianDate <- as.integer(gsub(year, "", date.substring)) - 1
  
  date <- as.Date(julianDate, origin = origin)
  
  return(date)
}


chi.aod <- stack("MCD19A2_largebbox.tif")


#chi.aod.mean <- raster::mean(chi.aod, na.rm = T)
plot(chi.aod.mean)




aod.names <- read.csv("MCD19A2_Names.csv")

aod.names <- aod.names$x
aod.names <- as.character(aod.names)

aod.dates <- as.Date(unlist(map(aod.names, MAIACDateFromName)))

#Create Raster Time Series Object from aod 
aod.rts <- rts(chi.aod, aod.dates)
plot(aod.rts@raster[[141]])

aod.yearly <- rts::apply.yearly((aod.rts), mean, na.rm = T)


aod.quarterly <- apply.quarterly(aod.rts, mean, na.rm = T)
write.rts(aod.quarterly, "aod_quarterly")

aod.monthly <- apply.monthly(aod.rts, mean, na.rm = T)
write.rts(aod.monthly, "aod_monthly")
gdalinfo(chi.aod)


aod.monthly.names <- read.csv("aod.monthly.names.csv")

###############################################################
# eg 2017001
#File path to folder with tifs
aod.path <- "D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif1"


#List all AOD files
all.aod <- list.files(path = aod.path, full.names = TRUE, pattern = "MCD19A2")
aod.list <- as.list(all.aod)

# find files in the same date 
file_same_date <- lapply(aod.list, function(ls) grep("2017001", ls))
aod.list[file_same_date > 0]

# merge the same-date files together 

a20170011 <- raster(all.aod[1])
a20170012 <- raster(all.aod[2])
a20170013 <- raster(all.aod[3])
a20170014 <- raster(all.aod[4])
a2017001 <- raster::merge(a20170011, a20170012, a20170013, a20170014)
plot(a2017001)

#eg 20170002
aod.path2 <- "D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif2"
aod2 <- list.files(path = aod.path2, full.names = TRUE, pattern = "MCD19A2")

a20170021 <- raster(aod2[1])
a20170022 <- raster(aod2[2])
a20170023 <- raster(aod2[3])
a20170024 <- raster(aod2[4])
a2017002 <- raster::merge(a20170021, a20170022, a20170023, a20170024)
plot(a2017002)

# average day1 and day2, looks perfect! 
day12 <- stack(a2017001, a2017002)
day12 <- reclassify(day12, cbind(0, NA))
day12mean <- mean(day12, na.rm=TRUE)

# read tif in different days
aod.loc <- "D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif"
aod_days <- list.files(path = aod.loc, full.names = TRUE, pattern = "MCD19A2")

setwd("D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif_day")

for (i in c(1:2)) {
  today <- paste0("201700", as.character(i))
  today_tif <- aod_days[grep(today, aod_days)] 
  today_merge <- raster::merge(raster(today_tif[1]),raster(today_tif[2]),raster(today_tif[3]),raster(today_tif[4]))
  tif_name <- paste0(today, ".tif")
  writeRaster(today_merge, tif_name)
}

###############################################################
# formally merge tiles to days

# read tif in different days
aod.loc <- "D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tiff"
aod_days <- list.files(path = aod.loc, full.names = TRUE, pattern = "MCD19A2")

setwd("D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif_day")

nDigits <- function(x) nchar( trunc( abs(x) ) )


for (i in c(1:365)){
  
  if (nDigits(i) == 1){
    
    today <- paste0("201700", as.character(i)) 
    
  } else if (nDigits(i) == 2){
    
    today <- paste0("20170", as.character(i))
    
  } else if (nDigits(i) == 3){
    
    today <- paste0("2017", as.character(i))
    
  } else {NULL}
  
  
  today_tif <- aod_days[grep(today, aod_days)] 
  
  
  if (length(today_tif) == 4){
    
    today_merge <- raster::merge(raster(today_tif[1]),raster(today_tif[2]),raster(today_tif[3]),raster(today_tif[4]))
    
    tif_name <- paste0(today, ".tif")
    
    writeRaster(today_merge, tif_name)
    
  } else{print(paste0(today, "requires tiles check"))}
}


## !!!!!!!!!!!!!!!! convert tif for 2017059 2017082 (h26v4)

# stack tif of each day in 2017
path2017 <- "D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif_day"

aod2017 <- list.files(path = path2017, full.names = TRUE, pattern = "2017")

month <- c("jan", "feb", "mar","apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
setwd("D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif_month")

### calculate monthly average

for (m in month[1:12]) {
  pos <- base::match(m, month)
  start <- pos*30-29
  end <- pos*30
  name = paste0("bj2017", m, "_mean")
  tif_name <- paste0(name, ".tif")
  if (pos < 12) {
    assign(name, raster::mean(stack(aod2017[start:end]), na.rm=TRUE))
    writeRaster(get(name), tif_name)
    print(paste0(tif_name, " is saved"))
  } else {
    assign(name, raster::mean(stack(aod2017[start:length(aod2017)]), na.rm=TRUE))
    writeRaster(get(name), tif_name)
    print(paste0(tif_name, " is saved"))
  }
}

# stack monthly average
path2017month <- "D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif_month"

aod2017month <- list.files(path = path2017month, full.names = TRUE, pattern = "2017")

aod2017_monthstack <- stack(aod2017month)
aod2017_avg <- raster::mean(aod2017_monthstack, na.rm=TRUE)
##########################################

### change proj and crop to beijing shapefile

##############################################################
####reproject 

new.crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
aod.crs <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs ")

rect <- matrix(c(115.41203, 39.43470, 
                 117.51985, 39.43470, 
                 117.51985, 41.07256, 
                 115.41203, 41.07256,
                 115.41203, 39.43470),
               ncol = 2, byrow = TRUE)
p1 <- (Polygon(rect))
ps1 <- SpatialPolygons(list(Polygons(list(p1), ID = "a")), proj4string=new.crs)
ps1 <- spTransform(ps1, aod.crs)

new.bbox <- bbox(ps1)

# read in beijing shapefile

bj <- readOGR('D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/affordable-housing/data/beijing/beijing/bjdist.shp')
bj <- bj[0]


bj.map.transformed <- spTransform(bj, aod.crs)

# crop months stack with new.bbox
surrounding.crop <- crop(aod2017_monthstack, new.bbox)

reproj.surrounding.stack <- projectRaster(surrounding.crop, crs = new.crs)

writeRaster(reproj.surrounding.stack, "MCD19A2_largebbox.tif")

# crop 2017 average with new.bbox
surrounding.crop.avg <- crop(aod2017_avg, new.bbox)
reproj.surrounding.avg <- projectRaster(surrounding.crop.avg, crs = new.crs)

# crop 2017 average with bj.map
bjmap.crop.avg <- crop(aod2017_avg, bj.map.transformed)
reproj.bj.avg <- projectRaster(bjmap.crop.avg, crs = new.crs)

# mask 2017 average with bj.map
bj2017avg_mask <- raster::mask(reproj.bj.avg, bj)
writeRaster(bj2017avg_mask, "bj2017_monthly_avg.tif")



#############################################################
# plot the map 
# code source:
# https://stackoverflow.com/questions/33530055/add-raster-to-ggmap-base-map-set-alpha-transparency-and-fill-color-to-inset-r
library(raster)
library(ggplot2)
library(ggmap)

# r <- raster() # any raster you want to plot
rtp <- rasterToPolygons(bj2017avg_mask)
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

colnames(rtpFortMer)[8] <- "Optical_Depth_047"

## plot it out!
bjbasemap + geom_polygon(data = rtpFortMer, 
                  aes(x = long, y = lat, group = group, fill = Optical_Depth_047), 
                  alpha = 0.5, 
                  size = 0) +  ## size = 0 to remove the polygon outlines
  scale_fill_gradientn(colours = topo.colors(255)) + 
  #guides(fill=guide_legend(title="Optical_Depth_047 (1km)")) +
  ggtitle("Beijing Air Quality 2017 (MCD19A2)") + 
  theme(plot.title = element_text(hjust = 0.5))

#############################################################
# SETUP goole api for get_map
register_google(key = "AIzaSyB4qCvKU-dQKYBtUoVWW4z_RRhU3y6wPPI")
has_google_key()

bjbox <- c(left =115.4, bottom = 39.3, right = 117.7, top = 41.3)
bjbasemap <- ggmap(get_map(bjbox, maptype = "satellite", zoom = 9))

##############################################################


# extract monthly aod values for locations


##############################################################
setwd("D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/tif_month")
origin <- read_sf('D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/affordable-housing/data/working_data/origin.shp', quiet = FALSE)
orig_repr <- st_set_crs(origin, 4326)

### change proj and crop to beijing shapefile


new.crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
aod.crs <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs ")

rect <- matrix(c(115.41203, 39.43470, 
                 117.51985, 39.43470, 
                 117.51985, 41.07256, 
                 115.41203, 41.07256,
                 115.41203, 39.43470),
               ncol = 2, byrow = TRUE)
p1 <- (Polygon(rect))
ps1 <- SpatialPolygons(list(Polygons(list(p1), ID = "a")), proj4string=new.crs)
ps1 <- spTransform(ps1, aod.crs)

new.bbox <- bbox(ps1)

# read in beijing shapefile

bj <- readOGR('D:/Auguste/UChicago/2018-2019/2019winter/GIS2/project/affordable-housing/data/beijing/beijing/bjdist.shp')
bj <- bj[0]


bj.map.transformed <- spTransform(bj, aod.crs)

# crop monthly average with new.bbox

for (m in month) {
  name = paste0("bj2017", m, "_mean")
  
  ## reproject monthly average and crop it
  bjmap.crop.avg <- crop(get(name), bj.map.transformed)
  reproj.bj.avg <- projectRaster(bjmap.crop.avg, crs = new.crs)
  
  file <- paste0(name, "_mask", ".tif")
  
  ## mask raster to Beijing and export 
  
  month_mask <- raster::mask(reproj.bj.avg, bj)
  writeRaster(month_mask, file)
  print(paste0(file, " is saved"))
  
  ## extract aod values
  rt = raster(file)
  orig_repr[paste0(m, "_mean")] = raster::extract(rt, orig_repr)
  print(paste0("column", ": ", m, "_mean", " is added."))
}


origin_aod <- orig_repr %>%
  st_drop_geometry()

write.csv(origin_aod, file =  "D:/Auguste/Projects/soh-bj/data/dest/aod.csv")

##############################################################
## extract aod for neighborhood origins
origin_ct <- read_sf('D:/Auguste/Projects/soh-bj/data/orig/neigh_ct.shp', quiet = FALSE)
plot(origin_ct)

for (m in month) {
  name = paste0("bj2017", m, "_mean")
  
  file <- paste0(name, "_mask", ".tif")
  
  ## extract aod values
  rt = raster(file)
  origin_ct[paste0(m, "_mean")] = raster::extract(rt, origin_ct)
  print(paste0("column", ": ", m, "_mean", " is added."))
}


origin_ct <- origin_ct %>%
  st_drop_geometry()

write.csv(origin_ct, file =  "D:/Auguste/Projects/soh-bj/data/dest/aod_neigh.csv")
