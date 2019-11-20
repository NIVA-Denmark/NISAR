# http://marineregions.org/mrgid/2401

source("worms.R")

df <- read.table("output/ODA_distinct_species.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8")

# match all Species names in the list from ODA against WoRMS data and return the AphiaID and correct scientific name

df <- df %>%
  mutate(searchfor=lapply(Species,function(x) FixNames(x))) %>%
  mutate(Aphia=lapply(searchfor,function(x) GetSpeciesID(x)))  %>%
  mutate(AphiaID=sapply(Aphia,function(x) unlist(x)[2]),
         ScientificName=sapply(Aphia,function(x) unlist(x)[3])) %>%
  select(-c(searchfor,Aphia)) %>%
  mutate(AphiaID=as.integer(AphiaID),Updated=as.Date(format(Sys.Date(), "%d-%m-%Y"),"%d-%m-%Y"))
         
write.table(df,file="output/ODA_Species_AphiaID.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8")
