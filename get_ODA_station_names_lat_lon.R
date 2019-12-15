

dfODA <- read.table("C:/Users/CJM/OneDrive - NIVA/github/ODA/HentData_VGZGDKOLCT_naeringsstoffer.csv",sep=";",header=T,stringsAsFactors=F)

dfODAx <-dfODA %>%
  mutate(Station=MC.stationsnr,Lat=degreesODA(ObsSted_bredde),Lon=degreesODA(ObsSted_længde)) %>%
  select(Station,Lat,Lon)


dfODAstn<- dfODAx %>%
  group_by(Station,Lat,Lon) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  select(-n)

dfODAcount <- dfODAstn %>% 
  group_by(Station) %>%
  summarise(n=n()) %>%  
  filter(n>1)


dfODAstn<- dfODAx %>%
  group_by(Station,Lat,Lon) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  group_by(Station) %>%
  summarise(n=n())  


dfODAcount %>% filter(n>1)


write.table(dfODAstn,file="ODA_station_lat_lon.txt",row.names=F,col.names=T,sep=";",quote=F)


dfODAstn<-read.table(file="ODA_station_lat_lon.txt",header=T,sep=";",stringsAsFactors=F)


dfODAstn <- dfODAstn %>%
  mutate(StationCode=Station) %>%
  mutate(n=sapply(Station, function(x) regexpr("[0-9]",x)[1])) %>%
  mutate(Amt=ifelse(n>0,substr(Station,1,n-1),""),
         Station=ifelse(n>0,substr(Station,n,nchar(Station)),Station)) %>%
  select(StationCode,Amt,Station,Lat,Lon)

dfDouble <- dfODAstn %>%
  group_by(Station) %>%
  summarise(n=n()) %>%
  filter(n>1) %>%
  left_join(dfODAstn,by="Station") %>%
  mutate(Note=paste0(StationCode," [",round(Lat,4),",",round(Lon,4),"]"))

dfDuplicates <- dfODAstn %>%
  group_by(Station,Lat,Lon) %>%
  summarise(n=n(),Alternatives=paste0(StationCode,collapse="/")) %>%
  filter(n>1) %>%
  select(-n)

dfDouble <- dfDouble %>%
  left_join(dfDuplicates,by=c("Station","Lat","Lon"))

dfDouble2 <- dfDouble %>%
  ungroup() %>%
  mutate(Alternatives=ifelse(is.na(Alternatives),"",Alternatives)) %>%
  group_by(Station,Lat,Lon,Alternatives) %>%
  summarise(Note=paste0(Note,collapse=",")) %>%
  ungroup() 

dfDouble2 <- dfDouble2 %>%
  mutate(n1=sapply(Note,function(x) regexpr("\\[",x)[1]),
         n2=sapply(Note,function(x) regexpr("\\]",x)[1])) %>%
  mutate(Note=ifelse(Alternatives=="",Note,paste0(Alternatives," ",substr(Note,n1,n2)))) %>%
  select(Station,Lat,Lon,Note)

dfDouble3 <- dfDouble2 %>%
 group_by(Station) %>%
  summarise(n=n(),Positions=paste0(Note,collapse=",")) %>%
  mutate(Note=paste0(n," posn",ifelse(n>1,"s",""),": {",Positions,"}")) %>%
  select(Station,Note)
  


dfODAstn <- dfODAstn %>% 
  group_by(Station,Lat,Lon) %>%
  summarise() %>%
  ungroup() %>%
  left_join(dfDouble3,by="Station")

