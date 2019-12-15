library(tidyverse)
library(readxl)

folder <- "../7-Data - 190183 NISAR/nst/"

degreesODA<-function(degstr,commadec=T){
  degstr<-as.character(degstr)
  if(commadec==T){
    n <- regexpr(",",degstr)[1]
    deg<-as.numeric(substr(degstr,n-4,n-3))
    min<-as.numeric(gsub(",",".",substr(degstr,n-2,99)))
  }else{
    n <- regexpr("\\.",degstr)[1]
    deg<-as.numeric(substr(degstr,n-4,n-3))
    min<-as.numeric(substr(degstr,n-2,99))
  }
  deg <- deg + (min/60)
  return(deg)
}

# -------------- STOQ -------------- 

# read the fixed-column-width text files
if(FALSE){ # need to remove this if(FALSE) container when all the processing steps are finished
col_end <- c(17,58,99,111,123,134,145,156,164,185,266,287)
col_end <- c(17,58,99,111,123,134,145,164,185,266,287)
colw <- col_end - c(1,col_end[1:length(col_end)-1])

file<-"Marin_FYTO_fra_STOQ.txt"
dfFyto1 <- read.fwf(paste0(folder,file),widths=colw,header=F,stringsAsFactors=F)
names(dfFyto1)<-dfFyto1[1,]
dfFyto1<-dfFyto1[2:nrow(dfFyto1),]

file<-"Marin_ZOO_fra_STOQ.txt"
dfZoo1 <- read.fwf(paste0(folder,file),widths=colw,header=F,stringsAsFactors=F)
names(dfZoo1)<-dfZoo1[1,]
dfZoo1<-dfZoo1[2:nrow(dfZoo1),]

# read xlsx files

file<-"STOQ_Artsfund_fytoplankton.xlsx"
dfFyto2 <- read_xlsx(paste0(folder,file))  #,header=T,sep="\t",stringsAsFactors=F,quote="")

file<-"STOQ_Artsfund_zooplankton.xlsx"
dfZoo2 <- read_xlsx(paste0(folder,file))

file<-"T_Marine_Arter_PlSys_MBL.xlsx"
dfMBL <- read_xlsx(paste0(folder,file))

file<-"T_Marine_Arter_PlSys_Orb.xlsx"
dfOrb <- read_xlsx(paste0(folder,file))

file<-"T_Marine_Arter_PlSys_Rkb.xlsx"
dfRkb <- read_xlsx(paste0(folder,file))

file<-"T_Marine_Arter_PlSys_AAA.xlsx"
dfAAA <- read_xlsx(paste0(folder,file))

file<-"Haardbundsdata_ODA.xlsx"
dfHaardbund <- read_xlsx(paste0(folder,file))

save(dfFyto1,dfFyto2,dfZoo1,dfZoo2,dfMBL,dfOrb,dfRkb,dfAAA,dfHaardbund,file="STOQ.Rda")
}

load("STOQ.Rda")

dfODAstn<-read.table(file="ODA_station_lat_lon.txt",header=T,sep=";",stringsAsFactors=F)


# dfOrb - NO lat, lon given. These need to be match to Station

dfHaardbundx <- dfHaardbund %>%
  mutate(Date=as.Date(as.character("dato...12"),format = "%Y-%m-%d")) %>%
  mutate(Lat1=degreesODA(bredde/100),Lon1=degreesODA(laengde/100)) %>%
  mutate(Table="Haardbundsdata_ODA") %>%
  select(Table,Species=dbo_t_Arter_Navn,Lat,Lon,Date)


dfMBLx <- dfMBL %>%
  filter(!is.na(Tælletal)) %>%
  mutate(Date=as.Date(as.character(Dato),format = "%Y-%m-%d")) %>%
  mutate(Lat=degreesODA(Breddegrad),Lon=degreesODA(Længdegrad)) %>%
  mutate(Table="T_Marine_Arter_PlSys_MBB") %>%
  select(Table,Species=dbo_t_Arter_Navn,Lat,Lon,Date)


dfOrb <- dfOrb %>%
  filter(!is.na(COUNT_TOTAL)) %>%
  mutate(Date=as.Date(as.character(DATE_TIME),format = "%Y-%m-%d")) %>%
  mutate(Table="T_Marine_Arter_PlSys_Orb") %>%
  select(Table,Species=NAME_LATIN,Lat,Lon,Date)

  
dfAAA <- dfAAA %>% 
  filter(!is.na(COUNT_TOTAL)) %>%
  mutate(Date=as.Date(as.character(DATE_TIME),format = "%Y-%m-%d")) %>%
  mutate(Lat=degreesODA(Udtryk6,F),Lon=degreesODA(Udtryk4,F)) %>%
  mutate(Table="T_Marine_Arter_PlSys_AAA") %>%
  select(Table,Species=NAME_LATIN,Lat,Lon,Date)

dfRkb <- dfRkb %>% 
  filter(!is.na(COUNT_TOTAL)) %>%
  mutate(Date=as.Date(as.character(DATE_TIME),format = "%Y-%m-%d")) %>%
  mutate(Lat=degreesODA(Udtryk6,F),Lon=degreesODA(Udtryk4,F)) %>%
  mutate(Table="T_Marine_Arter_PlSys_Rkb") %>%
  select(Table,Species=NAME_LATIN,Lat,Lon,Date)


dfRkbDistinct


write.table(df,file="output/STOQ_distinct_Species.csv",col.names=T,row.names=F,sep=";",na="",fileEncoding="UTF-8")
#   
#   
# paste0(list.files(folder),collapse='"+file<-"')
# 
# file<-"Capture.PNG"
# file<-"GES 22-2019-06 D2 Baselines Appendix 1.xlsx"
# file<-"GES 22-2019-06 D2 Baselines Appendix 2.xlsx"
# file<-"wetransfer-152b11.zip"
# file<-"wetransfer-7407bd.zip"