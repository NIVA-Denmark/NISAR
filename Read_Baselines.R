library(tidyverse)
library(readxl)

rm(list=ls())

source("input_tidying_functions.R")
source("worms.R")

folder <- "../7-Data - 190183 NISAR/nst/"

# split species name from author names, depending on postions of spaces and brackets
# some special cases - we caqnt distinguish between a third species name component and the author:
# e.g. Sebastes schlegelii Hilgendorf, 1880
#      Diopatra hupferiana hupferiana (Augener, 1918)
# Diopatra hupferiana hupferiana
# Chondrus giganteus f. flabellatus

Species<- c("Amphistegina cf. papillosa",
            "Chondrus giganteus f. flabellatus",
            "Chaetoceros cf. lorenzianus",
            "Diopatra hupferiana hupferiana",
            "Diopatra hupferiana monroi",
            "Mesanthura cfr. romulea")


# ---------- Read Appendix 1 -----------------
df1<-read_excel(paste0(folder,"GES 22-2019-06 D2 Baselines Appendix 1.xlsx"),sheet = "all NIS")

names(df1)[1]<-"Species"
df1 <- df1 %>%
  mutate(Species = gsub(intToUtf8(160)," ",Species)) 

df1<-SplitSpeciesName(df1,Species)

df1 <- df1 %>%
  group_by(Species,Species2) %>%
  summarise(n=n())

df1s <- df1

df1 <- df1  %>%
  ungroup() %>%
  select(SpeciesOriginal=Species,Species=Species2,-n)

# lookup species names in WoRMS database to get correct AphiaID
df1 <- df1 %>%
  mutate(searchfor=lapply(Species,function(x) FixNames(x))) %>%
  mutate(Aphia=lapply(searchfor,function(x) GetSpeciesID(x)))  %>%
  mutate(AphiaID=sapply(Aphia,function(x) unlist(x)[2]),
         ScientificName=sapply(Aphia,function(x) unlist(x)[3])) %>%
  select(-c(searchfor,Aphia)) %>%
  mutate(AphiaID=as.integer(AphiaID),Updated=as.Date(format(Sys.Date(), "%d-%m-%Y"),"%d-%m-%Y"))

# write results Appendix 1
write.table(df1,file="output/Appendix1_Species.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8")

# ---------- Read Appendix 2 -----------------
rm("df2")

sheets<-excel_sheets(paste0(folder,"GES 22-2019-06 D2 Baselines Appendix 2.xlsx"))
sheets<-sheets[2:length(sheets)]

for(s in sheets){
  df<-read_excel(paste0(folder,"GES 22-2019-06 D2 Baselines Appendix 2.xlsx"),sheet = s)
  names(df)[1]<-"Species"
  df <- df %>%
    mutate(Country=s,Species=trimws(Species)) %>%
    mutate(Species = gsub(intToUtf8(160)," ",Species)) %>% # replace non-breaking space character
    select(Country,Species,Status)
  if(exists("df2")){
    df2<-df2 %>%
      bind_rows(df)
  }else{
    df2<-df
  }
}

df2long <- df2

df2 <- df2 %>%
  filter(!is.na(Status)) 

df2 <- df2 %>%
  group_by(Species,Status) %>%
  summarise(Countries=paste0(Country, collapse=",")) %>%
  ungroup()

df2spr <- df2 %>%
  spread(value=Countries,key=Status) 

# Species which appear in more than one category (cryptogenic/non-indegenous/data-deficient)
df2multiple <- df2 %>%
  group_by(Species) %>%
  summarise(n=n()) %>%
  ungroup() %>% 
  filter(n>1) %>%
  select(-n)

df2multiple <- df2multiple %>%
  left_join(df2spr,by="Species")

df2 <- df2 %>%
  group_by(Species) %>%
  summarise(Status=paste0(Status, collapse="/")) %>%
  ungroup()

df2 <- SplitSpeciesName(df2,Species)

df2 <- df2 %>%
  select(SpeciesOriginal=Species,Species=Species2,Status) %>%
  left_join(df2spr,by=c("SpeciesOriginal"="Species"))


df2 <- df2 %>% left_join(mutate(df1,DK=TRUE),by=c("SpeciesOriginal","Species"))

df2x <- df2 %>% filter(is.na(AphiaID)) %>%
  select(-c(AphiaID,ScientificName,Updated))

df2 <- df2 %>% filter(!is.na(AphiaID))

df2x <- df2x %>%
  mutate(searchfor=lapply(Species,function(x) FixNames(x))) %>%
  mutate(Aphia=lapply(searchfor,function(x) GetSpeciesID(x)))  %>%
  mutate(AphiaID=sapply(Aphia,function(x) unlist(x)[2]),
         ScientificName=sapply(Aphia,function(x) unlist(x)[3])) %>%
  select(-c(searchfor,Aphia)) %>%
  mutate(AphiaID=as.integer(AphiaID),Updated=as.Date(format(Sys.Date(), "%d-%m-%Y"),"%d-%m-%Y"))

df2 <- bind_rows(df2,df2x) %>%
  arrange(SpeciesOriginal)

df2 <- df2 %>%
  mutate(ParentID=sapply(AphiaID,function(x) GetParentAphiaID(x)))


# write results Appendix 2
write.table(df2,file="output/Appendix2_Species.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8")
write.table(df2multiple,file="output/Appendix2_multiple_categories.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8")

