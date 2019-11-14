library(tidyverse)
library(readxl)

rm(list=ls())

folder <- "../7-Data - 190183 NISAR/nst/"

# ---------- Read Appendix 1 -----------------
df1<-read_excel(paste0(folder,"GES 22-2019-06 D2 Baselines Appendix 1.xlsx"),sheet = "all NIS")

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
            "Diopatra hupferiana monroi")

dfSpecies<-data.frame(Species,stringsAsFactors=F)
dfSpecies <- dfSpecies %>%
  mutate(n=nchar(Species))

names(df1)[1]<-"Species"
df1 <- df1 %>%
  mutate(Species = gsub(intToUtf8(160)," ",Species)) %>%
  mutate(ns=str_locate_all(Species," ")) %>%
  mutate(nb=str_locate_all(Species,"\\(")) %>%
  mutate(nvar=str_locate_all(Species,"var\\."),
         nsub=str_locate_all(Species,"subsp\\.")) %>%
  mutate(nvar=sapply(nvar,function(x) unlist(x)[1]),
         nsub=sapply(nsub,function(x) unlist(x)[1])) %>%
  mutate(ns2=sapply(ns,function(x) unlist(x)[2]),
         ns3=sapply(ns,function(x) unlist(x)[3]),
         ns4=sapply(ns,function(x) unlist(x)[4]),
         nb1=sapply(nb,function(x) unlist(x)[1])) %>%
  mutate(nsplit=ifelse(is.na(nsub) & is.na(nvar),
                       ifelse(is.na(nb1),ns2,ifelse(nb1<ns2,ns3,ns2)),
                       ns4)-1,
         n=NA)

  for(i in 1:nrow(dfSpecies)){
    sx <- dfSpecies[i,"Species"]
    nx <- dfSpecies[i,"n"]
    df1 <- df1 %>%
      mutate(n=ifelse(is.na(n),
                      ifelse(substr(Species,1,nx)==sx,nx,NA),
                      n))
  }

  df1 <- df1 %>%
    mutate(nsplit=ifelse(is.na(n),nsplit,n)) %>%
    mutate(Species2=substr(Species,1,nsplit)) 
  
  df1 <- df1 %>%
  select(-c(n,ns,nb,nvar,nsub,ns2,ns3,ns4,nb1,nsplit))

df1 <- df1 %>%
  group_by(Species,Species2) %>%
  summarise(n=n())

df1s <- df1

df1 <- df1  %>%
  ungroup() %>%
  select(SpeciesOriginal=Species,Species=Species2,-n)



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


df2 <- df2 %>%
  mutate(ns=str_locate_all(Species," ")) %>%
  mutate(nb=str_locate_all(Species,"\\(")) %>%
  mutate(nvar=str_locate_all(Species,"var\\."),
         nsub=str_locate_all(Species,"subsp\\.")) %>%
  mutate(nvar=sapply(nvar,function(x) unlist(x)[1]),
         nsub=sapply(nsub,function(x) unlist(x)[1])) %>%
  mutate(ns2=sapply(ns,function(x) unlist(x)[2]),
         ns3=sapply(ns,function(x) unlist(x)[3]),
         ns4=sapply(ns,function(x) unlist(x)[4]),
         nb1=sapply(nb,function(x) unlist(x)[1])) %>%
  mutate(nsplit=ifelse(is.na(nsub) & is.na(nvar),
                       ifelse(is.na(nb1),ns2,ifelse(nb1<ns2,ns3,ns2)),
                       ns4)-1,
         n=NA) 

for(i in 1:nrow(dfSpecies)){
  sx <- dfSpecies[i,"Species"]
  nx <- dfSpecies[i,"n"]
  df2 <- df2 %>%
    mutate(n=ifelse(is.na(n),
                    ifelse(substr(Species,1,nx)==sx,nx,NA),
                    n))
}

df2 <- df2 %>%
  mutate(nsplit=ifelse(is.na(n),nsplit,n)) %>%
  mutate(Species2=substr(Species,1,nsplit)) 

df2 <- df2 %>%
  select(-c(n,ns,nb,nvar,nsub,ns2,ns3,ns4,nb1,nsplit)) %>%
  select(SpeciesOriginal=Species,Species=Species2,Status) %>%
  left_join(df2spr,by=c("SpeciesOriginal"="Species"))


# ------------------ write results --------------------------------------------------------
write.table(df1,file="output/Appendix1_Species.csv",col.names=T,row.names=F,sep=";",na="")
write.table(df2,file="output/Appendix2_Species.csv",col.names=T,row.names=F,sep=";",na="")
write.table(df2multiple,file="output/Appendix2_multiple_categories.csv",col.names=T,row.names=F,sep=";",na="")

