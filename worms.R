#Use the libraries
library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
library(httr)
library(tidyverse)


GetSpeciesID<-function(searchtext){
  # ---------- get the AphiaID from the search text -----------------------------------------------
  #Build the URL to get the data from
  
  df <- list(Species=searchtext,
                   AphiaID=NA,
                   ScientificName=NA)

  if(grepl("\\/", searchtext)==TRUE){
    # invalid character
    return(df)
  }
  
  searchtext2 <- gsub(" ","%20",searchtext)
  url<-sprintf("http://marinespecies.org/rest/AphiaIDByName/%s?marine_only=true",searchtext2)
  
  x<-http_status(GET(url))
  if(x$reason!="OK"){
    cat(paste0(searchtext,": ",x$reason,"\n"))
    return(df)
  }
  
  #Get the AphiaID
  AphiaID <- fromJSON(url)
  cat(paste0(searchtext,": AphiaID=",AphiaID))
  
  # ---------- get the Aphia record from the AphiaID -----------------------------------------------
  
  
  url<-sprintf("http://marinespecies.org/rest/AphiaRecordByAphiaID/%d",AphiaID)
  url
  AphiaRecord <- fromJSON(url)
  
  
  validID<-AphiaRecord$valid_AphiaID
  if(is.null(validID)){
    cat(paste0(" (No validID in record)\n"))
  }else{
    if(validID != AphiaID){
      cat(paste0(" (Using AphiaID=",validID,")\n"))
      
      AphiaIDorig <- AphiaID
      AphiaID <- validID
      AphiaRecordOrig<-AphiaRecord
      
      # get the correct record
      url<-sprintf("http://marinespecies.org/rest/AphiaRecordByAphiaID/%d",AphiaID)
      AphiaRecord <- fromJSON(url)
    }else{
      cat(paste0(" (Valid ID)\n"))
    }
  }
  
  df$AphiaID<-AphiaID
  df$ScientificName<-AphiaRecord$scientificname
  
  return(df)
}


GetSpeciesInfo<-function(AphiaID){
  
  url<-sprintf("http://marinespecies.org/rest/AphiaRecordByAphiaID/%d",AphiaID)
  AphiaRecord <- fromJSON(url)
  cat(paste0(AphiaRecord$scientificname," [",AphiaID,"]\n"))
  
  # ---------- get the Aphia synonyms -----------------------------------------------
  
  url<-sprintf("http://marinespecies.org/rest/AphiaSynonymsByAphiaID/%d?offset=1",AphiaID)

  x<-http_status(GET(url))
  if(x$reason=="OK"){
    dfSynonyms <- fromJSON(url)
    synonyms<-paste0(dfSynonyms$scientificname,collapse=", ")
    cat(paste0("  Synonyms: ",synonyms,"\n"))
    bSynonyms = TRUE
  }else{
    cat(paste0("  No synonyms found\n"))
    bSynonyms = FALSE
  }
  
  # ---------- Get all distributions for a given AphiaID -----------------------------------------------
  
  # loop through AphiaID for synonyms
  cat("  Get distributions:\n")
  nc <- nchar(AphiaRecord$scientificname)
  if(bSynonyms==TRUE){
    ncs <- max(nchar(dfSynonyms$scientificname),na.rm=T)
    if(ncs>nc){
      nc<-ncs
    }
  }
  
  url <- sprintf("http://marinespecies.org/rest/AphiaDistributionsByAphiaID/%d", AphiaID);
  x<-http_status(GET(url))
  if(x$reason=="OK"){
    Synonym<-AphiaRecord$scientificname
    spaces<-paste(replicate(nc-nchar(Synonym), " "), collapse = "")
    
    bFoundDistribution=TRUE
    distribution <- fromJSON(url)
    distribution$AphiaID <- AphiaID
    distribution$SynonymID <- AphiaID
    distribution$Synonym <- Synonym
    
    cat(paste0("   ",Synonym," [",AphiaID,"]: ",spaces, nrow(distribution)," records \n"))
  }else{
    bFoundDistribution=FALSE
  }
  
  if(bSynonyms==TRUE){
    
    for(id in dfSynonyms$AphiaID){
      Synonym<-dfSynonyms$scientificname[dfSynonyms$AphiaID==id]
      spaces<-paste(replicate(nc-nchar(Synonym), " "), collapse = "")
      cat(paste0("   ",Synonym," [",id,"]: ",spaces))
      url <- sprintf("http://marinespecies.org/rest/AphiaDistributionsByAphiaID/%d", id);
      x<-http_status(GET(url))
      if(x$reason=="OK"){
        distributionSynonym <- fromJSON(url)
        
        cat(paste0(nrow(distributionSynonym)," records \n"))
        
        distributionSynonym$AphiaID <- AphiaID
        distributionSynonym$SynonymID <- id
        distributionSynonym$Synonym <- Synonym
        if(bFoundDistribution==TRUE){
          distribution <- bind_rows(distribution,distributionSynonym)
        }else{
          distribution <- distributionSynonym
          bFoundDistribution=TRUE
        }
        
      }else{
        cat(paste0("no records \n"))
      }
    }
  }
  
  if(!exists("distribution")){
    
    # check for null values in the record
    if(is.null(AphiaRecord$kingdom)){
      kingdom<-NA
    }else{
      kingdom<-AphiaRecord$kingdom
    }
    if(is.null(AphiaRecord$phylum)){
      phylum<-NA
    }else{
      phylum<-AphiaRecord$phylum
    }
    if(is.null(AphiaRecord$class)){
      class<-NA
    }else{
      class<-AphiaRecord$class
    }
    if(is.null(AphiaRecord$order)){
      order<-NA
    }else{
      order<-AphiaRecord$order
    }
    if(is.null(AphiaRecord$family)){
      family<-NA
    }else{
      family<-AphiaRecord$family
    }
    if(is.null(AphiaRecord$genus)){
      genus<-NA
    }else{
      genus<-AphiaRecord$genus
    }
    
    distribution <- data.frame(
      AphiaID=AphiaID,
      SynonymID=AphiaID,
      ScientificName=AphiaRecord$scientificname,
      Synonym=AphiaRecord$scientificname,
      Kingdom=kingdom,
      Phylum=phylum,
      Class=class,
      Order=order,
      Family=family,
      Genus=genus,
      stringsAsFactors=FALSE)
    
  }else{
    distribution$ScientificName <- AphiaRecord$scientificname
    distribution$Kingdom <- AphiaRecord$kingdom
    distribution$Phylum <- AphiaRecord$phylum
    distribution$Class <- AphiaRecord$class
    distribution$Order <- AphiaRecord$order
    distribution$Family <- AphiaRecord$family
    distribution$Genus <- AphiaRecord$genus
  }
  
  
  nameslist <- c("ScientificName","AphiaID","Synonym","SynonymID","Kingdom","Phylum","Class","Order","Family","Genus")
  
  nameslist2 <- names(distribution)[!names(distribution) %in% nameslist ]
  nameslist <- c(nameslist,nameslist2)
  nameslist<-nameslist[nameslist %in% names(distribution)]
  distribution <- distribution[,nameslist]
  
  return(distribution)
  
  
  #distribution <- distribution %>%
  #  filter(!is.na(establishmentMeans))
  
  # http://marinespecies.org/aphia.php?p=manual#topic22
  #
  # Images, specimens, vernaculars, notes, distributions, taxa, … carry quality indicator icons.
  # 
  # Checked: verified by a taxonomic editor
  # Trusted: edited by a thematic editor
  # Unreviewed: has not been verified by a taxonomic editor
  # 
  
}

GetSpeciesInfoFromName<-function(searchtext){
  # ---------- get the AphiaID from the search text -----------------------------------------------
  #Build the URL to get the data from
  
  if(grepl("\\/", searchtext)==TRUE){
    # invalid character
    return(data.frame())
  }
  
  searchtext <- gsub(" ","%20",searchtext)
  url<-sprintf("http://marinespecies.org/rest/AphiaIDByName/%s?marine_only=true",searchtext)
  
  x<-http_status(GET(url))
  if(x$reason!="OK"){
    return(data.frame())
  }
  
  #Get the AphiaID
  AphiaID <- fromJSON(url)
  cat(paste0("AphiaID=",AphiaID,"\n"))
  
  # ---------- get the Aphia record from the AphiaID -----------------------------------------------
  
  
  url<-sprintf("http://marinespecies.org/rest/AphiaRecordByAphiaID/%d",AphiaID)
  url
  AphiaRecord <- fromJSON(url)
  
  
  validID<-AphiaRecord$valid_AphiaID
  
  
  if(validID != AphiaID){
    cat(paste0("AphiaID ",AphiaID," not valid. Using AphiaID=",validID,"\n"))
    
    AphiaIDorig <- AphiaID
    AphiaID <- validID
    AphiaRecordOrig<-AphiaRecord
    
    # get the correct record
    url<-sprintf("http://marinespecies.org/rest/AphiaRecordByAphiaID/%d",AphiaID)
    AphiaRecord <- fromJSON(url)
  }else{
    cat(paste0("AphiaID ",AphiaID," is valid.\n"))
  }
  
  
  # ---------- get the Aphia synonyms -----------------------------------------------
  
  url<-sprintf("http://marinespecies.org/rest/AphiaSynonymsByAphiaID/%d?offset=1",AphiaID)
  url
  x<-http_status(GET(url))
  if(x$reason=="OK"){
    dfSynonyms <- fromJSON(url)
    synonyms<-paste0(dfSynonyms$scientificname,collapse="\n")
    cat(paste0("Synonyms:\n",synonyms,"\n"))
    bSynonyms = TRUE
  }else{
    cat(paste0("No synonyms found\n"))
    bSynonyms = FALSE
  }
  
  # ---------- get the structure for kingdom, family, etc. -----------------------------------------------
  
  
  url <- sprintf("http://marinespecies.org/rest/AphiaClassificationByAphiaID/%d", AphiaID);
  
  #Get the actual data from the URL
  classificationTree <- fromJSON(url)
  
  #Walk the classification tree
  currentTreeItem = classificationTree
  while (!is.null(currentTreeItem )) {
    print(sprintf("ID: %d, RANK: %s, ScientificName: %s",
                  currentTreeItem$AphiaID,
                  currentTreeItem$rank,
                  currentTreeItem$scientificname
    ));
    #You can access the variables individually
    #print(currentTreeItem$AphiaID);
    #print(currentTreeItem$scientificname);
    #print(currentTreeItem$rank);
    
    #Get next item in the tree
    currentTreeItem <- currentTreeItem$child;
  }
  
  # /AphiaVernacularsByAphiaID/{ID}
  # Get all vernaculars for a given AphiaID
  
  # ---------- Get all distributions for a given AphiaID -----------------------------------------------
  
  # loop through AphiaID for synonyms
  
  url <- sprintf("http://marinespecies.org/rest/AphiaDistributionsByAphiaID/%d", AphiaID);
  x<-http_status(GET(url))
  if(x$reason=="OK"){
    bFoundDistribution=TRUE
    distribution <- fromJSON(url)
    distribution$AphiaID <- AphiaID
    distribution$Synonym <- AphiaRecord$scientificname
  }else{
    bFoundDistribution=FALSE
  }
  
  if(bSynonyms==TRUE){
    for(id in dfSynonyms$AphiaID){
      cat("  getting distribution for ",id,"\n")
      url <- sprintf("http://marinespecies.org/rest/AphiaDistributionsByAphiaID/%d", id);
      x<-http_status(GET(url))
      if(x$reason=="OK"){
        distributionSynonym <- fromJSON(url)
        
        cat("  found ",nrow(distributionSynonym)," records \n")
        
        distributionSynonym$AphiaID <- id
        distributionSynonym$Synonym <- dfSynonyms$scientificname[dfSynonyms$AphiaID==id]
        if(bFoundDistribution==TRUE){
          distribution <- bind_rows(distribution,distributionSynonym)
        }else{
          distribution <- distributionSynonym
          bFoundDistribution=TRUE
        }
        
      }
    }
  }
  
  if(!exists("distribution")){
    
    # check for null values in the record
    if(is.null(AphiaRecord$kingdom)){
      kingdom<-NA
    }else{
      kingdom<-AphiaRecord$kingdom
    }
    if(is.null(AphiaRecord$phylum)){
      phylum<-NA
    }else{
      phylum<-AphiaRecord$phylum
    }
    if(is.null(AphiaRecord$class)){
      class<-NA
    }else{
      class<-AphiaRecord$class
    }
    if(is.null(AphiaRecord$order)){
      order<-NA
    }else{
      order<-AphiaRecord$order
    }
    if(is.null(AphiaRecord$family)){
      family<-NA
    }else{
      family<-AphiaRecord$family
    }
    if(is.null(AphiaRecord$genus)){
      genus<-NA
    }else{
      genus<-AphiaRecord$genus
    }
    
    distribution <- data.frame(
             AphiaID=AphiaID,
             ScientificName=AphiaRecord$scientificname,
             Synonym=AphiaRecord$scientificname,
             Kingdom=kingdom,
             Phylum=phylum,
             Class=class,
             Order=order,
             Family=family,
             Genus=genus,
             stringsAsFactors=FALSE)
    
  }else{
    distribution$ScientificName <- AphiaRecord$scientificname
    distribution$Kingdom <- AphiaRecord$kingdom
    distribution$Phylum <- AphiaRecord$phylum
    distribution$Class <- AphiaRecord$class
    distribution$Order <- AphiaRecord$order
    distribution$Family <- AphiaRecord$family
    distribution$Genus <- AphiaRecord$genus
  }
  
  
  nameslist <- c("ScientificName","Synonym","AphiaID","Kingdom","Phylum","Class","Order","Family","Genus")
  
  nameslist2 <- names(distribution)[!names(distribution) %in% nameslist ]
  nameslist <- c(nameslist,nameslist2)
  nameslist<-nameslist[nameslist %in% names(distribution)]
  distribution <- distribution[,nameslist]
  
  return(distribution)
  
  
  #distribution <- distribution %>%
  #  filter(!is.na(establishmentMeans))
  
  # http://marinespecies.org/aphia.php?p=manual#topic22
  #
  # Images, specimens, vernaculars, notes, distributions, taxa, … carry quality indicator icons.
  # 
  # Checked: verified by a taxonomic editor
  # Trusted: edited by a thematic editor
  # Unreviewed: has not been verified by a taxonomic editor
  # 
  
}
