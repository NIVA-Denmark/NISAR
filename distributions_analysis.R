library(tidyverse)
library(stats)
library(jsonlite)
library(httr)
library(sf)
library(rgdal)

# ------------ Polygons to intersect with regions ------------ 
dfDK <- data.frame(id=c("1","2"),
                  x0=c(-28,0),
                  x1=c(43,30),
                  y0=c(26,49),
                  y1=c(69,67),
                  stringsAsFactors=F)
# use combinations of min/max lat and lon to create a df for each corner
df1 <- dfDK %>%
  dplyr::select(id,lon=x0,lat=y0)
df2 <- dfDK %>%
  dplyr::select(id,lon=x0,lat=y1)
df3 <- dfDK %>%
  dplyr::select(id,lon=x1,lat=y1)
df4 <- dfDK %>%
  dplyr::select(id,lon=x1,lat=y0)

# combine the 4 corner dataframes. We now have 1 df with 4 rows for each MRG region
dfDK <- bind_rows(df1,df2,df3,df4) %>%
  arrange(id)

# make a list
dfDK_list <- split(dfDK, dfDK$id)

# only want lon-lats in the list, not the names
dfDK_list <- lapply(dfDK_list, function(x) { x["id"] <- NULL; x })

psDK<-lapply(dfDK_list, Polygon)

# add id variable
psDK1 <- lapply(seq_along(psDK), function(i) Polygons(list(psDK[[i]]), 
                                                      ID = names(dfDK_list)[i]  ))

# create SpatialPolygons object
sp_DK <- SpatialPolygons(psDK1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

sp_df_DK <- SpatialPolygonsDataFrame(sp_DK, 
                                      data.frame(id = unique(dfDK$id), 
                                                 row.names = unique(dfDK$id)))

plot(sp_df_DK)

# ---------------------------  Polygons from regions --------------------------------------------- 


df <- read.table("output/ODA_Species_Distributions_LatLon.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8" )

dfGeo1<-df %>% 
  group_by(higherGeography) %>%
  summarise() %>%
  ungroup() %>%
  rename(Name=higherGeography) %>%
  mutate(Geo="higherGeography")

dfGeo2<-df %>% 
  filter(higherGeography=="") %>%
  group_by(MRGID,locality) %>%
  summarise() %>%
  ungroup() %>%
  rename(Name=locality) %>%
  mutate(Geo="locality")

dfGeo <- bind_rows(dfGeo1,dfGeo2)

#write.table(dfGeo,file="output/MRG_Geography.csv",col.names=T,row.names=F,sep=";",na="")
dfGeo<-read.table(file="output/MRG_Geography.csv",header=T,sep=";",stringsAsFactors=F,quote="")


df <- df %>% filter(!is.na(minLongitude))



dfType<-df %>% 
  group_by(placeType) %>%
  summarise(n=n()) %>%
  ungroup()


df <- df %>%
  filter(MRGID %in% c(1908,1910,4347)) #%>%
  #mutate(longitude=-179)

df <- df %>%
  mutate(minLatitude=ifelse(is.na(minLatitude),latitude-0.01,minLatitude),
         maxLatitude=ifelse(is.na(maxLatitude),latitude+0.01,maxLatitude),
         minLongitude=ifelse(is.na(minLongitude),longitude-0.01,minLongitude),
         maxLongitude=ifelse(is.na(maxLongitude),longitude+0.01,maxLongitude)) 


df <- df %>% 
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude)) %>%
  group_by(MRGID,longitude,latitude,minLatitude,maxLatitude,minLongitude,maxLongitude) %>%
  summarise(locality=paste0(locality,collapse="/")) %>%
  ungroup()

dfcheck <- df %>% 
  group_by(longitude,latitude,minLatitude,maxLatitude,minLongitude,maxLongitude) %>%
  summarise(n=n()) %>% 
  filter(n>1) %>%
  left_join(df)

# use combinations of min/max lat and lon to create a df for each corner
df1 <- df %>%
  dplyr::select(locality,MRGID,lon=minLongitude,lat=minLatitude) %>%
  mutate(pt=1)
df2 <- df %>%
  dplyr::select(locality,MRGID,lon=minLongitude,lat=latitude) %>%
  mutate(pt=2)
df3 <- df %>%
  dplyr::select(locality,MRGID,lon=minLongitude,lat=maxLatitude) %>%
  mutate(pt=3)
df4 <- df %>%
  dplyr::select(locality,MRGID,lon=longitude,lat=maxLatitude) %>%
  mutate(pt=4)
df5 <- df %>%
  dplyr::select(locality,MRGID,lon=maxLongitude,lat=maxLatitude) %>%
  mutate(pt=5)
df6 <- df %>%
  dplyr::select(locality,MRGID,lon=maxLongitude,lat=minLatitude) %>%
  mutate(pt=6)

# combine the 4 corner dataframes. We now have 1 df with 4 rows for each MRG region
mrg_df <- bind_rows(df1,df2,df3,df4,df5,df6) %>%
  arrange(MRGID,locality,pt) %>%
  dplyr::select(-pt)

# make a list
mrg_list <- split(dplyr::select(mrg_df,MRGID,lon,lat), mrg_df$MRGID)

# only want lon-lats in the list, not the names
mrg_list <- lapply(mrg_list, function(x) { x["MRGID"] <- NULL; x })

psmrg<-lapply(mrg_list, Polygon)

# add id variable
pmrg <- lapply(seq_along(psmrg), function(i) Polygons(list(psmrg[[i]]), 
                                                      ID = names(mrg_list)[i]  ))

# create SpatialPolygons object
sp_mrg <- SpatialPolygons(pmrg, proj4string = CRS("+proj=longlat +datum=WGS84")) 

sp_df_mrg <- SpatialPolygonsDataFrame(sp_mrg, 
                                      data.frame(id = unique(mrg_df$MRGID), 
                                                 row.names = unique(mrg_df$MRGID)))
# add area of shape to the data
# we will use this later to see if the polygon falls entirely within the EEZ - or conversely the EEZ is entirely within the polygon
sp_df_mrg@data$area<-sapply(slot(sp_df_mrg, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))



# ------------ load EEZ polygons ------------------------------------------------------------------------ 
sfEEZ <- readOGR(dsn="gis/shp", layer="EEZ")
# convert to data frame for plotting with ggplot - takes a while
sfEEZdf <- fortify(sfEEZ)

# generate fake data and add to data frame
sfEEZdf$count <- round(runif(nrow(sfEEZdf), 0, 100), 0)

sfEEZ$areaEEZ<-sapply(slot(sfEEZ, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))


#check the areas
# plot with ggplot
ggplot(sfEEZdf, aes(x = long, y = lat, group = id, fill = count)) +
  geom_polygon(colour = "black", size = 0.3, aes(group = id)) +
  theme() +coord_map()

# ------------ load countries polygons ------------------------------------------------------------------------ 

sfCountries <- readOGR(dsn="gis/shp", layer="countries_WGS84_singlepart")
sfCountries@data$id = rownames(sfCountries@data)
sfCountries.points = fortify(sfCountries, region="id")
sfCountriesdf = left_join(sfCountries.points, sfCountries@data, by="id")


# ------------  intersect Polygons from regions with two DK area polygons ------------------------ 
sp_df_mrg@proj4string<-sfEEZ@proj4string

library(raster)
spdfInt<-raster::intersect(x = sp_df_mrg, y = sfEEZ)
spdfInt@data$areaInt<-sapply(slot(spdfInt, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))



dfInt <- spdfInt@data
# for some intersects, the results have more than one part for each original shape. Take the sum of these areas
dfInt$areaInt<-sapply(dfInt$areaInt, function(x) sum(unlist(x)))

dfInt <- dfInt %>%
  group_by(id,area) %>%
  summarise(areaEEZ=sum(areaEEZ,na.rm=T),areaInt=sum(areaInt,na.rm=T))


dfInt <- dfInt %>%
  mutate(area=round(area,6),
         areaEEZ=round(areaEEZ,6),
         areaInt=round(areaInt,6))

dfInt <- dfInt %>%
  mutate(coversEEZ=areaEEZ==areaInt,
         enclosedEEZ=area==areaInt)


dfInt <- df %>%
  left_join(dfInt,by=c("MRGID"="id"))
# ------------  plot Polygons ------------------------------------------------------------------------ 

p<-ggplot(sp_df_mrg, aes(x=long,y=lat,group = id)) + 
  geom_polygon(data=sfCountriesdf,colour='#aaaaaa', fill='#cccccc',aes(x=long,y=lat,group=id)) +
  geom_path(colour='#999999') +
  geom_polygon(data=sp_df_DK,colour='red', fill=NA,aes(x=long,y=lat,group=id)) +coord_map() +
  geom_polygon(data=sfEEZdf,colour=NA, fill='red',aes(x=long,y=lat,group=id),alpha=0.5)

p
ggsave(p,filename="figures/polygon_map1.png",units="cm",height=15,width=20,dpi=300)

p<-ggplot(sp_df_mrg, aes(x=long,y=lat,group = id)) + 
  geom_polygon(data=sfCountriesdf,colour='#aaaaaa', fill='#cccccc',aes(x=long,y=lat,group=id)) +
  geom_path(colour='#999999') +
  geom_polygon(data=sp_df_DK,colour='red', fill=NA,aes(x=long,y=lat,group=id)) +coord_map(xlim=c(-30,45),ylim=c(20,80)) +
  geom_polygon(data=sfEEZdf,colour=NA, fill='red',aes(x=long,y=lat,group=id),alpha=0.5)

p
ggsave(p,filename="figures/polygon_map2.png",units="cm",height=20,width=15,dpi=300)


p<-ggplot(sp_df_mrg, aes(x=long,y=lat,group = id)) + 
  geom_polygon(data=sfCountriesdf,colour='#aaaaaa', fill='#cccccc',aes(x=long,y=lat,group=id)) +
  geom_path(colour='#999999') +
  geom_polygon(data=sp_df_DK,colour='red', fill=NA,aes(x=long,y=lat,group=id)) +coord_map() +
  geom_polygon(data=sfEEZdf,colour=NA, fill='red',aes(x=long,y=lat,group=id),alpha=0.5)

p

test <-subset(sp_df_mrg, id %in% c(1908,1910))


p<-ggplot(test, aes(x=long,y=lat,group = id)) + 
  geom_polygon(data=sfCountriesdf,colour='#aaaaaa', fill='#cccccc',aes(x=long,y=lat,group=id)) +
  geom_path(colour='#999999') +
  geom_polygon(data=sp_df_DK,colour='red', fill=NA,aes(x=long,y=lat,group=id)) +
  #coord_map(xlim=c(-30,45),ylim=c(20,80)) +
  geom_polygon(data=sfEEZdf,colour=NA, fill='red',aes(x=long,y=lat,group=id),alpha=0.5)

p

p<-ggplot(test, aes(x=long,y=lat,group = id)) + 
  geom_path()




test <- sp_df_intersect[63,]

p<-ggplot(sfEEZdf, aes(x=long,y=lat,group = id)) + 
  geom_polygon(colour=NA, fill='red',alpha=0.5) +
  geom_polygon(data=test,colour='red', fill=NA,aes(x=long,y=lat,group=id)) +coord_map() 

  p

writeOGR(sp_df_mrg, "gis/shp", "WoRMS", driver="ESRI Shapefile")
writeOGR(sfEEZ, "gis/shp", "EEZarea", driver="ESRI Shapefile")


library(rgeos)
df_intersect <- rgeos::gIntersection(spgeom1 = sp_df_mrg,
                                         spgeom2 = sfEEZ,byid=F)


df_intersect <- st_join(sp_df_mrg,sfEEZ)




sfdfi <- st_intersection(sfdf,sfdfx)


plot(sfdfi[, 'region'])

sfdfi1 <- sfdfi %>% 
  filter(polygons==1)
plot(sfdfi1[, 'region'])

sfdfi2 <- sfdfi %>% 
  filter(polygons==2)
plot(sfdfi2[, 'region'])

class(sfdf)

polys <- SpatialPolygonsDataFrame(sfdf, data.frame(id=ID, row.names=ID))

spdf <- SpatialPointsDataFrame(coords = XY, data = sfdf,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))








# Example data
square <- t(replicate(50, {
  o <- runif(2)
  c(o, o + c(0, 0.1), o + 0.1, o + c(0.1, 0), o)
}))
ID <- paste0('sq', seq_len(nrow(square)))

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

plot(polys.df, col=rainbow(50, alpha=0.5))




