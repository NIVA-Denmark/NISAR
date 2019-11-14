# http://marineregions.org/mrgid/2401


dflist <- read.table("output/ODA_distinct_species.csv",sep=";",header=T,stringsAsFactors=F)
rm("dfResults")
rm("dfNotFound")

#for(i in 2339:2342){
for(i in 1:nrow(dflist)){

  ODAname<-dflist[i,"Species"]
  Table<-dflist[i,"Table"]
  cat(paste0("Checking ",ODAname," [",Table,"]\n"))
  df <- GetSpeciesInfo(ODAname)
  
  if(nrow(df)>0){
    df$ODAname <- ODAname
    df$ODAtable <- Table 
    if(exists("dfResults")){
      dfResults<-bind_rows(dfResults,df)
    }else{
      dfResults<-df
    }
    
  }else{
    df<-dflist[i,]
    if(exists("dfNotFound")){
      dfNotFound<-bind_rows(dfNotFound,df)
    }else{
      dfNotFound<-df
    }
  }
}

save(dfResults,file="output/results.Rda")
save(dfNotFound,file="output/notfound.Rda")


