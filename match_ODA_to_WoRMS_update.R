# http://marineregions.org/mrgid/2401

source("worms.R")

df <- read.table("output/ODA_distinct_Species.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8")
df_old <- read.table("output/ODA_Species_AphiaID.csv",sep=";",header=T,stringsAsFactors=F,fileEncoding="UTF-8")

df$ID<-1:nrow(df)

df_matched <- df %>%
  left_join(df_old,by=c("Species","n","Table","Phylum","Note")) 

df_unmatched <- df_matched %>%
  filter(is.na(AphiaID)) 

df_matched <- df_matched %>%
  filter(!is.na(AphiaID)) 

# match all Species names in the list from ODA against WoRMS data and return the AphiaID and correct scientific name

df_unmatched <- df_unmatched %>%
  mutate(searchfor=lapply(Species,function(x) FixNames(x))) %>%
  mutate(Aphia=lapply(searchfor,function(x) GetSpeciesID(x)))  %>%
  mutate(AphiaID=sapply(Aphia,function(x) unlist(x)[2]),
         ScientificName=sapply(Aphia,function(x) unlist(x)[3]),
         Rank=sapply(Aphia,function(x) unlist(x)[4]),
         ParentID=sapply(Aphia,function(x) unlist(x)[5])) %>%
  select(-c(searchfor,Aphia)) %>%
  mutate(AphiaID=as.integer(AphiaID),Updated=as.Date(format(Sys.Date(), "%d-%m-%Y"),"%d-%m-%Y"))
       
df <- bind_rows(df_matched,df_unmatched) %>%
  arrange(ID) %>%
  select(-ID)
  
write.table(df,file="output/ODA_Species_AphiaID.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8")

# ----------- see number of entries -----------------------------------
dfcount <- df %>% 
  mutate(matched=ifelse(is.na(AphiaID),"unmatched","matched")) %>%
  group_by(Table,matched) %>%
  summarise(species=n(),data=sum(n,na.rm=T)) %>%
  gather(key="count",value="n",c("species","data")) %>%
  spread(key="matched",value="n") %>%
  ungroup()

dfcount_total <- dfcount %>%
  ungroup() %>%
  mutate(Table="Total") %>%
  group_by(Table,count) %>%
  summarise(matched=sum(matched,na.rm=T),
            unmatched=sum(unmatched,na.rm=T))

dfcount <- dfcount %>%
  bind_rows(dfcount_total) %>%
  mutate(pct=round(100*unmatched/(matched+unmatched),1)) 
  
