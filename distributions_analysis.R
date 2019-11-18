library(stats)
library(tidyverse)
library(jsonlite)
library(httr)

df <- read.table("output/ODA_Species_Distributions.csv",sep=";",header=T,stringsAsFactors=F)
df <- df %>% 
  filter(locality!="")

df1 <- distinct(df,locality,locationID,higherGeography)
df1 <- df1 %>% 
  filter(higherGeography != "")

df <- distinct(df,locality,locationID)

df <- df %>% 
  left_join(df1,by=c("locality","locationID"))


mrginfo<-function(MRGID){
  url<-sprintf("http://marineregions.org/rest/getGazetteerWMSes.json/%d/",MRGID)
  
  wms <- data.frame()
  
  x<-http_status(GET(url))
  if(x$reason!="OK"){
    cat(paste0(MRGID,": ",x$reason,"\n"))
    return(wms)
    }
  wms <- fromJSON(url)
  #wms<-list(wms$MRGID,wms$url,wms$namespace,wms$featureType,wms$featureName)
  return(wms)
  }

df <- df %>%
  mutate(MRGID=as.numeric(substr(locationID,32,nchar(locationID))))

#df <- df[1:9,]
df <- df[c(32,462,868,892),]


df <- df %>%
  mutate(MRGinfo=lapply(MRGID, function(x) mrginfo(x)))

df <- df %>%
  mutate(url=sapply(MRGinfo, function(x) unlist(x)[2]),
         namespace=sapply(MRGinfo, function(x) unlist(x)[3]),
         featureType=sapply(MRGinfo, function(x) unlist(x)[4]),
         featureName=sapply(MRGinfo, function(x) unlist(x)[5]),
         MRGID2=sapply(MRGinfo, function(x) unlist(x)[1])
         )


url<-"http://geo.vliz.be/geoserver/wms?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.3.0"
x<-http_status(GET(url))
dfx <- fromJSON(url)
#url<- "http://www.marineregions.org/downloads.php#ihoeez" 
