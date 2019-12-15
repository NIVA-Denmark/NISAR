library(tidyverse)

df <- read.table("output/ODA_Species_Distributions.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8")

dfGeo<-read.table(file="output/MRG_Geography.csv",header=T,sep=";",stringsAsFactors=F,quote="",fileEncoding="UTF-8")

dfRegions <- dfGeo %>%
  group_by(Region) %>%
  summarise()

dfGeoHigher <- dfGeo %>%
  filter(Geo=="higherGeography") %>%
  select(higherGeography=Name,Region) %>%
  group_by(higherGeography,Region) %>%
  summarise()

dfGeoLocality <- dfGeo %>%
  filter(Geo=="locality") %>%
  select(locality=Name,MRGID,Region) %>%
  group_by(MRGID,Region) %>%
  summarise()

# species data with no distributions
dfNoDistribution <- df %>%
  filter(locality=="")


df1 <- df %>%
  mutate(MRGID=as.numeric(substr(locationID,32,nchar(locationID))),
         higherMRGID=as.numeric(substr(locationID,32,nchar(higherGeographyID)))) %>%
  select(AphiaID,ScientificName,recordStatus,typeStatus,establishmentMeans,locality,locationID,MRGID,higherGeography,higherMRGID) %>%
  filter(!is.na(MRGID)) %>%
  left_join(dfGeoLocality,by="MRGID") %>%
  rename(RegionMRGID=Region) %>%
  left_join(dfGeoHigher,by="higherGeography") %>%
  rename(RegionHigher=Region) %>%
  mutate(Region=ifelse(is.na(RegionMRGID),RegionHigher,RegionMRGID)) %>%
  select(-c(RegionHigher,RegionMRGID))
 
# Get species where Region is not found

test<-df1 %>% filter(is.na(Region)) %>%
  group_by(locality,MRGID,higherGeography,higherMRGID,Region) %>% 
  summarise() %>%
  ungroup() %>%
  mutate(Geo="locality") %>%
  rename(Name=locality) %>%
  select(Name,Geo,MRGID,Region) 

# write.table(test,file="output/MRG_GeographyXXXX.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8",quote=F)

df2 <- df1 %>%
  group_by(AphiaID,ScientificName,recordStatus,typeStatus,establishmentMeans,Region) %>%
  summarise(n=n()) %>%
  ungroup()


dfDK <- df2 %>%
  filter(Region=="DK",establishmentMeans=="Alien") %>%
  select(AphiaID)

dfx <- dfDK %>%
  left_join(df,by="AphiaID")


dfBase<- read.table("output/Appendix2_Species.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8")
dfBase <- dfBase %>%
  filter(DK==T)

dfBase <- dfBase %>%
  left_join(mutate(dfDK,WoRMS=T),by="AphiaID")

dfDK %>% nrow()
dfBase %>% filter(WoRMS==T) %>% nrow()

# find out whic
