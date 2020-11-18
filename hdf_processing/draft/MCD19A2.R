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
setwd("~/Desktop/AoT/MAIAC /Data Thru 9.19.18")


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

#File path to folder with tifs
aod.path <- "tif"

#List all AOD files
all.aod <- list.files(path = aod.path, full.names = TRUE, pattern = "MCD19A2")


aod.monthly.names <- aod.monthly.names$x

aod.monthly.names <- as.character(aod.monthly.names)
aod.monthly.names <- gsub(aod.monthly.names, pattern = "X", replacement = "")
head(aod.monthly.names)

date.names <- as.Date(aod.monthly.names, format = "%Y.%m.%d")

num.names <- as.numeric(date.names)

write.csv(num.names, "aod.monthly.names.csv")


aod.stack <- stack(all.aod)

new.crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
aod.crs <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs ")

rect <- matrix(c(-88.32937, 41.22154, 
                 -87.30472, 41.22154, 
                 -87.30472, 43.02008, 
                 -88.32937, 43.02008,
                 -88.32937, 41.22154),
                  ncol = 2, byrow = TRUE)
p1 <- (Polygon(rect))
ps1 <- SpatialPolygons(list(Polygons(list(p1), ID = "a")), proj4string=new.crs)
ps1 <- spTransform(ps1, aod.crs)

new.bbox <- bbox(ps1)



#Read in Chicago map with CAs
chi.map <- readOGR("Chicago")
chi.map.transformed <- spTransform(chi.map, aod.crs)

surrounding.crop <- crop(aod.stack, new.bbox)

reproj.surrounding.stack <- projectRaster(surrounding.crop, crs = new.crs)

writeRaster(reproj.surrounding.stack, "MCD19A2_largebbox.tif")

reproj.chi.aod.stack <- stack("MCD19A2_reproj.tif")

stack.names <- read.csv("MCD19A2_Names.csv")





#Rename MAIAC Files to original names
names(reproj.chi.aod.stack) <- stack.names$x

#Read in Chicago map with CAs
chi.map <- readOGR("chipoli")


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

#Convert file names to date and fix format
a <- ((unlist((map(names(reproj.chi.aod.stack), MAIACDateFromName)))))
a <- as.character(a)
names(reproj.chi.aod.stack) <- a

plot(reproj.chi.aod.stack[[1161]])
plot(chi.map, add = T)

year(as.Date(as.integer(a)))

### Create yearly average Aerosol Optical Depth for city 

#Create mean AOD from 2018 (note that missing Nov and Dec will bring down yearly average)
mean.2018 <- raster::mean(reproj.chi.aod.stack[[which(year(as.Date(as.integer(a))) == 2018)]], na.rm = TRUE)
plot(mean.2018)
summary(mean.2018)

#Create mean AOD from 2017
mean.2017 <- raster::mean(reproj.chi.aod.stack[[which(year(as.Date(as.integer(a))) == 2017)]], na.rm = TRUE)
plot(mean.2017)
summary(mean.2017)

#Create mean AOD from 2016
mean.2016 <- raster::mean(reproj.chi.aod.stack[[which(year(as.Date(as.integer(a))) == 2016)]], na.rm = TRUE)
plot(mean.2016)
#Change 0 value to NA (only 0 value likely a fluke)
mean.2016@data@values[which(mean.2016@data@values == 0)] <- NA
summary(mean.2016)

#Create mean AOD from 2015
mean.2015 <- raster::mean(reproj.chi.aod.stack[[which(year(as.Date(as.integer(a))) == 2015)]], na.rm = TRUE)
plot(mean.2015)
summary(mean.2015)

#Create mean AOD from 2014
mean.2014 <- raster::mean(reproj.chi.aod.stack[[which(year(as.Date(as.integer(a))) == 2014)]], na.rm = TRUE)
plot(mean.2014)
summary(mean.2014)

yearly.stack <- stack(mean.2014, mean.2015, mean.2016, mean.2017)
year.names <- c(2014, 2015, 2016, 2017)
names(yearly.stack) <- year.names

writeRaster(yearly.stack, "Yearly_Aod_Stack_Reproj.tif")

#breaks <- c(.1, .15, .2, .25, .3)

plot(chi.map)
plot(yearly.stack[[2]], 
     add = TRUE)


plot(yearly.stack)

#Create mean AOD for complete dataset
reproj.mean.all <- raster::mean(reproj.chi.aod.stack, na.rm = TRUE)

plot(mean.all)


summary(mean.all)

library(tmap)


breaks <- c(.14, .16, .18, .20, .22, .24)
pal <- colorRampPalette(c("green","red"))

aod.plot <- tm_shape(mean.all) +
  tm_raster(breaks = breaks, title = "AOD") +
  tm_shape(chi.map) +
  tm_borders() +
  tm_layout(main.title = "Aerosol Optical Depth in Chicago Jan. 1, 2014 - Sep. 18, 2018", legend.position = c("RIGHT", "BOTTOM"))

tm_shape(reproj.yearly.maiac) +
  tm_raster(breaks = breaks, title = year.names) +
  tm_shape(chi.map) +
  tm_borders()

aod.plot

summary(a)
plot(a,
     breaks = breaks,
     col = pal(5))


plot(chi.map, 
     add = T)

writeRaster(a, "AOD_Average_4_Year.tif")
writeRaster(reproj.mean.all, "AOD_Average_4_Year_Reproj.tif")

yearly.maiac <- stack("Yearly_Aod_Stack.tif")
projection(yearly.maiac)

reproj.yearly.maiac <- projectRaster(yearly.maiac, crs = new.crs)

reproj.yearly.maiac <- stack("Yearly_Aod_Stack_Reproj.tif")

summary(reproj.yearly.maiac)

sum(!is.na(getValues(reproj.yearly.maiac[[4]])))
gdalinfo("Yearly_Aod_Stack_Reproj.tif")
summary(reproj.yearly.maiac)


reproj.mean.other <- raster::mean(reproj.yearly.maiac, na.rm= TRUE)

aod.overall <- raster("AOD_Average_4_Year_Reproj.tif")
aod.overall <- readAll(aod.overall)
#Make 0 value NA
aod.overall@data@values[aod.overall@data@values == 0] <- NA
writeRaster(aod.overall, "AOD_Average_4_Year_Reproj.tif", overwrite = TRUE)


library(leaflet)

summary(reproj.mean.other)
summary(reproj.mean.all)

#Change 0 to NA
reproj.mean.all@data@values[(reproj.mean.all@data@values == 0)] = NA

summary(reproj.mean.other)

breaks <- seq(.1, .35, 0.02)
pal <- colorNumeric(c("green", "yellow", "red", "purple"), breaks, na.color = "transparent")

chi.bounds <- readOGR("chipoli")


leaflet() %>%
  addTiles() %>%
  addRasterImage(chi.aod.mean, opacity = 0.7, colors = pal) %>%
  addPolygons(data = chi.bounds, fill = F, color = "black") %>%
  addLegend(pal = pal, values = values(chi.aod.mean), title = "Aerosol Optical Depth") 






aod.stack <- stack("MCD19A2_reproj.tif")

stack.names <- read.csv("MCD19A2_Names.csv")
stack.names <- as.character(stack.names$x)

names(aod.stack) <- stack.names

aod.dates <- unlist(purrr::map(stack.names, MAIACDateFromName))
aod.dates
range.dates <- as.Date(unlist(map(aod.dates, as.Date)))
range.dates
month.year <- paste(month(range.dates), year(range.dates), sep = "-")
month.year



aod.monthly <- stack()
n <- 1
storage <- stack() #Holding stack
#Get monthly averages
unique.month.year <- (unique(month.year))
month.year

for (i in 2:length(month.year)) {
  
  if(month.year[i] == month.year[i-1]) {
    storage <- stack(storage, aod.stack[[i-1]])
  } else {
    storage <- stack(storage, aod.stack[[i-1]])
    storage.mean <- raster::mean(storage, na.rm = TRUE)
    aod.monthly <- stack(aod.monthly, storage.mean)
    names(aod.monthly[[n]]) <- unique.month.year[n]
    n <- n+1
    storage <- stack()
  }
  
  if(i == length(month.year)) {
    storage.mean <- raster::mean(storage, na.rm = TRUE)
    aod.monthly <- stack(aod.monthly, storage.mean)
    names(aod.monthly[[n]]) <- unique.month.year[n]
  }
}


writeRaster(aod.monthly, "AOD_Monthly_Avgs.tif")
write.csv(unique.month.year, "aod.monthly.names.csv")


monthly <- stack("AOD_Monthly_Avgs.tif")





