library(stats)
library(tidyverse)
library(jsonlite)
library(httr)
#library(rjson)

source("MRGinfo.R")


df <- read.table("output/ODA_Species_Distributions.csv",sep=";",header=T,stringsAsFactors=F)
df <- df %>% 
  filter(locality!="")

df1 <- distinct(df,locality,locationID,higherGeography)
df1 <- df1 %>% 
  filter(higherGeography != "")

df <- distinct(df,locality,locationID)

df <- df %>% 
  left_join(df1,by=c("locality","locationID"))


df <- df %>%
  mutate(MRGID=as.numeric(substr(locationID,32,nchar(locationID))))

df1 <- df
#df <- df[1:9,]
df <- df[c(32,462,868,892),]

df <- df %>%
  mutate(MRGinfo=lapply(MRGID, function(x) mrginfo(x)))

df <- df %>%
  mutate(placeType=sapply(MRGinfo, function(x) unlist(x)["placeType"]),
         latitude=sapply(MRGinfo, function(x) unlist(x)["latitude"]),
         longitude=sapply(MRGinfo, function(x) unlist(x)["longitude"]),
         minLongitude=sapply(MRGinfo, function(x) unlist(x)["minLongitude"]),
         maxLongitude=sapply(MRGinfo, function(x) unlist(x)["maxLongitude"]),
         minLatitude=sapply(MRGinfo, function(x) unlist(x)["minLatitude"]),
         maxLatitude=sapply(MRGinfo, function(x) unlist(x)["maxLatitude"])
         )

df <- df %>%
  select(-MRGinfo)

write.table(df,file="output/ODA_Species_Distributions_LatLon.csv",col.names=T,row.names=F,sep=";",na="")

