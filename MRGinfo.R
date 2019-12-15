
# Function mrginfo(MRGID)
# to get lat,lon extent information from marineregions.org 

mrginfo<-function(MRGID){
  url<-sprintf("http://marineregions.org/rest/getGazetteerRecordByMRGID.json/%d/",MRGID)
  df <- data.frame()
  
  x<-http_status(GET(url))
  if(x$reason!="OK"){
    cat(paste0(MRGID,": ",x$reason,"\n"))
    return(df)
  }
  
  attempt=0
  repeat{
    attempt=attempt+1
    df <- try(fromJSON(url),silent=TRUE)
    
    if(class(df)=="try-error"){
      cat(paste0("MRGID=",MRGID,": ",class(df)," (attempt=",attempt,")\n"))
    }else{
      #if(attempt>1){
      cat(paste0("MRGID=",MRGID,": succeeded (attempt=",attempt,")\n"))
      #}
      break
    }
    }
  return(df)
  }


mrgRelation<-function(MRGID,direction="upper",type="all"){
  # direction = "upper" / "lower" / "both"
  # type = "partof" / "partlypartof" / "adjecentto" / "similarto" / "administrativepartof" / "influencedby" / "all"
  
  url<-sprintf("http://www.marineregions.org/rest/getGazetteerRelationsByMRGID.json/%d/upper/all/",MRGID)
  df <- data.frame()
  
  x<-http_status(GET(url))
  if(x$reason!="OK"){
    cat(paste0(MRGID,": ",x$reason,"\n"))
    return(df)
  }
  
  attempt=0
  repeat{
    attempt=attempt+1
    df <- try(fromJSON(url),silent=TRUE)
    
    if(class(df)=="try-error"){
      cat(paste0("MRGID=",MRGID,": ",class(df)," (attempt=",attempt,")\n"))
    }else{
      #if(attempt>1){
      cat(paste0("MRGID=",MRGID,": succeeded (attempt=",attempt,")\n"))
      #}
      break
    }
  }
   return(df)
}

