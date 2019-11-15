# http://marineregions.org/mrgid/2401

source("worms.R")

rm("dfResults")

df <- read.table("output/ODA_Species_AphiaID.csv",sep=";",header=T,stringsAsFactors=F)

dfNotFound <- df %>%
  filter(is.na(AphiaID)) %>%
  select(-c(AphiaID,ScientificName))



dflist <- df %>%
  filter(!is.na(AphiaID)) %>%
  group_by(Table,AphiaID,ScientificName) %>%
  summarise(n=n()) %>%
  ungroup()


for(i in 1:nrow(dflist)){
  
  AphiaID<-dflist$AphiaID[i]
  Table<-dflist$Table[i]
  ScientificName<-dflist$ScientificName[i]
  cat(paste0(i," Checking ",ScientificName," [",Table,"]\n"))
  
  df <- GetSpeciesInfo(AphiaID)
  
  df$ODAtable <- Table 
    if(exists("dfResults")){
      dfResults<-bind_rows(dfResults,df)
    }else{
      dfResults<-df
    }
  }

write.table(dfNotFound,file="output/ODA_Species_Missing_AphiaID.csv",col.names=T,row.names=F,sep=";",na="")
write.table(dfResults,file="output/ODA_Species_Distributions.csv",col.names=T,row.names=F,sep=";",na="")


