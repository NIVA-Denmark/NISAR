# COnvert ODA lat/lon degree string format to decimal degrees.
# ODA gives degrees as DDMM.MM - degrees concatenated with decimal minutes

degreesODA<-function(degstr){
  n <- regexpr(",",degstr)[1]
  deg<-as.numeric(substr(degstr,n-4,n-3))
  min<-as.numeric(gsub(",",".",substr(degstr,n-2,99)))
  deg <- deg + (min/60)
  return(deg)
}


# 
SplitAlternativeSpeciesODA<-function(df){
  # where species name in a record is given with "/" separating two possible alternatives
  # then split to give two records
  # 
  dfsplit <- df %>%
    mutate(Species=gsub(" \\/ ","\\/",Species),
           Species=gsub("\\/ ","\\/",Species),
           Species=gsub(" \\/","\\/",Species)) %>%
    mutate(n1=str_locate_all(Species,"\\/"),n2=str_locate_all(Species," ")) %>%
    mutate(n1=sapply(n1,function(x) unlist(x)[1]),
           n3=sapply(n2,function(x) unlist(x)[2]),
           n2=sapply(n2,function(x) unlist(x)[1])) %>%
    mutate(n2=ifelse(is.na(n2),nchar(Species)+1,n2),
           n3=ifelse(is.na(n3),n2,n3))
  
  
  dfsplit <- dfsplit %>% 
    filter(!is.na(n1)) 
  
  dfsplit <- dfsplit %>%
    mutate(s1=ifelse(n2==n3,
                     ifelse(n1<n2,
                            paste0(substr(Species,1,n1-1)," ",substr(Species,n2,nchar(Species))),
                            substr(Species,1,n1-1)),
                     substr(Species,1,n1-1)),
           s2=ifelse(n2==n3,
                     ifelse(n1<n2,
                            substr(Species,n1+1,nchar(Species)),
                            paste0(substr(Species,1,n2-1)," ",substr(Species,n1+1,nchar(Species)))),
                     substr(Species,n1+1,nchar(Species))))
  
  dfsplit <- dfsplit %>%
    select(-c(n1,n2,n3))
  
  dfsplit <- dfsplit %>% 
    gather(key="Column",value="SpeciesNew",c(s1,s2)) %>%
    select(Species,Table,SpeciesNew)
  
  df <- df %>%
    left_join(dfsplit,by=c("Species","Table")) %>%
    mutate(Note=ifelse(is.na(SpeciesNew),NA,paste0("Split ODA entry: '",Species,"'")),
           Species=ifelse(is.na(SpeciesNew),Species,SpeciesNew)) %>%
    select(-SpeciesNew)
  
  return(df)
}

# Function for separating species names from author name(s) in Appendices 1 and 2
# e.g. Acipenser gueldenstaedtii Brandt & Ratzeburg, 1833 
SplitSpeciesName<-function(df,Species=NULL){
  
  # Species list - Species names which don't fit the other rules
  if(!is.null(Species)){
    dfSpecies<-data.frame(Species,stringsAsFactors=F)
    dfSpecies <- dfSpecies %>%
      mutate(n=nchar(Species))
  }else{
    dfSpecies<-data.frame()
  }
  
  df<-df %>%
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
    df <- df %>%
      mutate(n=ifelse(is.na(n),
                      ifelse(substr(Species,1,nx)==sx,nx,NA),
                      n))
  }
  
  df <- df %>%
    mutate(nsplit=ifelse(is.na(n),nsplit,n)) %>%
    mutate(Species2=substr(Species,1,nsplit)) 
  
  df <- df %>%
    select(-c(n,ns,nb,nvar,nsub,ns2,ns3,ns4,nb1,nsplit))  
  return(df)
}
