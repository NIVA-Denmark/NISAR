library(tidyverse)
source("worms.R")


# The ODA unique species list has a many-to-one relationship between ODA species names and the valid AphiaID/Scientific Name
# Also the same AphiaID can be found within several tables (Bottom fauna, Bottom veg., Phytoplankton or Zooplankton)



dfODA <- read.table("output/ODA_Species_AphiaID.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8")

dfODAtypes <- dfODA %>%
  filter(!is.na(AphiaID)) %>%
  group_by(AphiaID,ScientificName,Rank,Table) %>%
  summarise() %>%
  ungroup() 

dfODA_mixed_types <- dfODAtypes %>%
  group_by(AphiaID,ScientificName,Rank) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(n>1) %>%
  select(-n) %>%
  left_join(dfODAtypes,by=c("AphiaID","ScientificName","Rank")) %>%
  mutate(Found=T,Table=gsub(" ","_",Table)) %>%
  spread(key=Table,value=Found)

dfODAspecies <- dfODA %>%
  group_by(AphiaID,Species) %>%
  summarise() %>%
  ungroup() %>%
  group_by(AphiaID) %>%
  summarise(SpeciesList = paste0(Species,collapse=" \\ ")) %>%
  ungroup()

dfODA_mixed_types <- dfODA_mixed_types %>%
  left_join(dfODAspecies,by="AphiaID")
  
dfODAphylum <- dfODA_mixed_types %>%
  group_by(AphiaID) %>%
  summarise() %>%
  mutate(Aphia=lapply(AphiaID,function(x) GetAphiaRecord(x)),
         Kingdom=sapply(Aphia,function(x) unlist(x)["kingdom"]),
         Phylum=sapply(Aphia,function(x) unlist(x)["phylum"])) %>%
  select(-Aphia)

namesOld <- names(dfODA_mixed_types)
namesNew <- c("AphiaID","ScientificName","Rank","Kingdom","Phylum")
namesOld <- namesOld[!namesOld %in% namesNew]
namesNew <- c(namesNew, namesOld)

dfODA_mixed_types <- dfODA_mixed_types %>%
  left_join(dfODAphylum,by="AphiaID")

dfODA_mixed_types <- dfODA_mixed_types[,namesNew] 

write.table(dfODA_mixed_types,file="output/ODA_species_multiple_types.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8")

dfODA_X <- dfODA_mixed_types %>%
  mutate(problem=ifelse(Bottom_Vegetation & (Bottom_Fauna | Zooplankton),T,ifelse(Phytoplankton & (Bottom_Fauna | Zooplankton) ,T,F))) %>%
  filter(problem==T) %>%
  select(-problem)

