# http://marineregions.org/mrgid/2401

source("worms.R")

df <- read.table("output/ODA_distinct_species.csv",sep=";",header=T,stringsAsFactors=F)
#rm("dfResults")
#rm("dfNotFound")

# match all Species names in the list from ODA against WoRMS data and return the AphiaID and correct scientific name
df <- df %>%
  mutate(Aphia=lapply(Species,function(x) GetSpeciesID(gsub(" sp\\.","",x)))) %>%
  mutate(Aphia=lapply(Species,function(x) GetSpeciesID(gsub(" indet\\.","",x)))) %>%
  mutate(Aphia=lapply(Species,function(x) GetSpeciesID(gsub(" s\\.l\\.","",x)))) %>%
  mutate(AphiaID=sapply(Aphia,function(x) unlist(x)[2]),
         ScientificName=sapply(Aphia,function(x) unlist(x)[3])) %>%
  select(-Aphia)
         
write.table(df,file="output/ODA_Species_AphiaID.csv",col.names=T,row.names=F,sep=";",na="")



###############################################
# need to replace sp. and indet. in ODA data!
###############################################

# https://www.nobanis.org/marine-identification-key/

# http://algaebase.org/search/species/detail/?species_id=133224

# http://nordicmicroalgae.org/taxon/Quadricoccus%20ellipticus
