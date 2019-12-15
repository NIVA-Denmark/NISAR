library(tidyverse)
source("MRGinfo.R")

# Get listof Marine Region IDs which we consider relevant for Danish waters
dfMatch <- read.table("gis/MRG_matches.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8",quote="")

# Create a copy of Match table to link to the Higher Geography MRGID in distributions table
dfMatchHigher <- dfMatch %>% 
  select(MRGIDhigher=MRGID) %>% 
  mutate(matchHigher=TRUE)

# Create a copy of Match table to link to the locality MRGID in distributions table
dfMatchMRG <- dfMatch %>% 
  select(MRGID) %>% 
  mutate(match=TRUE)

# load the distributions table (43918 records)
dfDistributions <- read.table("output/ODA_Species_Distributions.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8")

df <- dfDistributions %>%
  filter(recordStatus=="valid") %>%
  mutate(MRGID=sapply(locationID,function(x) as.numeric(substr(x,32,nchar(x)))),
         MRGIDhigher=sapply(higherGeographyID,function(x) as.numeric(substr(x,32,nchar(x))))) %>%
  left_join(dfMatchMRG,by="MRGID") %>%
  left_join(dfMatchHigher,by="MRGIDhigher") %>%
  mutate(match=ifelse(is.na(match),matchHigher,match)) %>%
  filter(match==TRUE) %>%
  select(-c(match,matchHigher))

#filter(establishmentMeans=="Alien") %>%
  
df <- df %>%
  group_by(ScientificName,AphiaID,qualityStatus,establishmentMeans,locality) %>%
  summarise() %>%
  ungroup

dflocalities <- df %>%
  group_by(AphiaID,establishmentMeans) %>%
  summarise(localities=paste0(locality,collapse=" / ")) %>%
  ungroup()

df <- df %>%
  group_by(ScientificName,AphiaID,establishmentMeans,qualityStatus) %>%
  summarise(n=n()) %>%
  ungroup()

dfAlien <- df %>%
  filter(establishmentMeans=="Alien") %>%
  group_by(AphiaID) %>%
  summarise() %>%
  ungroup()

df <- dfAlien %>%
  left_join(df,by="AphiaID")

df <- df %>%
  mutate(Obs=paste0(establishmentMeans,"_",qualityStatus)) %>%
  select(ScientificName,AphiaID,Obs,n) %>%
  spread(key=Obs,value=n) %>%
  left_join(dflocalities,by=c("AphiaID","establishmentMeans")) %>%
  arrange(AphiaID,establishmentMeans)



