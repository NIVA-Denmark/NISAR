# http://marineregions.org/mrgid/2401

source("worms.R")

# do not replace existing results 
# only find distributions for new AphiaID which are not in th existing results

rm("dfResults")

df <- read.table("output/ODA_Species_AphiaID.csv",sep=";",header=T,stringsAsFactors=F)

dfPrevious <- read.table("output/ODA_Species_Distributions.csv",sep=";",header=T,stringsAsFactors=F)
dfDropList <- distinct(dfPrevious,AphiaID)
dfDropList$DROP<-TRUE
df <- df %>%
    left_join(dfDropList,by="AphiaID") %>%
    filter(is.na(DROP)) %>%
    select(-DROP)

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
  
  df <- GetSpeciesDistributions(AphiaID)
  
  df$ODAtable <- Table 
    if(exists("dfResults")){
      dfResults<-bind_rows(dfResults,df)
    }else{
      dfResults<-df
    }
}

dfResults <- bind_rows(dfPrevious,dfResults)

write.table(dfNotFound,file="output/ODA_Species_Missing_AphiaID.csv",col.names=T,row.names=F,
            sep=";",na="",fileEncoding="UTF-8")
write.table(dfResults,file="output/ODA_Species_Distributions.csv",col.names=T,row.names=F,
            sep=";",na="",fileEncoding="UTF-8")


