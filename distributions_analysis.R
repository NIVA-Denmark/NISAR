library(stats)
library(tidyverse)
library(jsonlite)
library(httr)
library(sf)

# ------------ Polygons to intersect with regions ------------ 
dfx <- data.frame(shape=c("1","2"),
                  x0=c(-28,0),
                  x1=c(43,30),
                  y0=c(26,49),
                  y1=c(79,68),
                  stringsAsFactors=F)

lst <- lapply(1:nrow(dfx), function(x){
  ## create a matrix of coordinates that also 'close' the polygon
  res <- matrix(c(dfx[x, 'x0'], dfx[x, 'y0'],
                  dfx[x, 'x0'], dfx[x, 'y1'],
                  dfx[x, 'x1'], dfx[x, 'y1'],
                  dfx[x, 'x1'], dfx[x, 'y0'],
                  dfx[x, 'x0'], dfx[x, 'y0'])  ## need to close the polygon
                , ncol =2, byrow = T
  )
  ## create polygon objects
  st_polygon(list(res))
  
})

sfdf <- st_sf(polygons = dfx[, 'shape'], st_sfc(lst))

sfdf

plot(sfdf)

# ------------  Polygons from regions ------------ 

df <- read.table("output/ODA_Species_Distributions_LatLon.csv",sep=";",header=T,stringsAsFactors=F)


df <- df %>%
  mutate(minLatitude=ifelse(is.na(minLatitude),latitude-0.01,minLatitude),
         maxLatitude=ifelse(is.na(minLatitude),latitude+0.01,maxLatitude),
         minLongitude=ifelse(is.na(minLongitude),latitude-0.01,minLongitude),
         maxLongitude=ifelse(is.na(minLongitude),latitude+0.01,maxLongitude)) 

lst <- lapply(1:nrow(df), function(x){
  ## create a matrix of coordinates that also 'close' the polygon
  res <- matrix(c(df[x, 'minLongitude'], df[x, 'minLatitude'],
                  df[x, 'minLongitude'], df[x, 'maxLatitude'],
                  df[x, 'maxLongitude'], df[x, 'maxLatitude'],
                  df[x, 'maxLongitude'], df[x, 'minLatitude'],
                  df[x, 'minLongitude'], df[x, 'minLatitude'])  ## need to close the polygon
                , ncol =2, byrow = T
  )
  ## create polygon objects
  st_polygon(list(res))
  
})

sfdf <- st_sf(polygons = dfx[, 'shape'], st_sfc(lst))

sfdf

plot(sfdf)


# create shape files





