#Use the libraries
library(stats)
library(tidyverse)
library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
library(httr)

ErrorList <- read.table("badnames.txt",sep=";",header=F,stringsAsFactors=F,encoding="Windows-1251")
ErrorList <- ErrorList[,1]

BadName<-function(name){

  ErrorListX <- c("Andre grønalger",
                 "Blomsterplanter, uspec.",
                 "Blågrøn",
                 "Blågrønalger og bakterier",
                 "Brown bush",
                 "Brown crust",
                 "Brunskorper",
                 "Enårige trådformede grøn-",
                 "brunalger",
                 "fasthæftede oprette alger",
                 "Fedtmøg - Pilayella, Ectocarpus",
                 "Fingrenet grøn busk",
                 "Green endophyte",
                 "Green endozoic",
                 "Grønalger kalkborende",
                 "Gulgrøn",
                 "Ingen arter registreret",
                 "Kiselalger",
                 "løstliggende alger",
                 "muslinge ånderør",
                 "Red bush",
                 "Red calcified crust",
                 "Red crust",
                 "Rødalger på lavt vand",
                 "Rødkødskorp, tyk",
                 "Rødkødskorp, tynd",
                 "Små makrofytter",
                 "svovlbakterier",
                 "Terebellidae",
                 "Trådalger-rø,br,gr - generelt",
                 "Trådform.enår.rødalg",
                 "Trådformede enårige brunalger",
                 "Trådformede enårige grønalger",
                 "Trådformede enårige rødalger",
                 "Bakterier",
                 "Ciliater (fra fytoplanktonundersøgelser)",
                 "Diverse (fra fytoplanktonundersøgelser)",
                 "Flagellate",
                 "Flagellate x",
                 "Monader",
                 "Nanoflagellates, unidentified",
                 "Nanoplankton, unidentified",
                 "Picoplankton, unidentified",
                 "Ultrananoplankton, unidentified",
                 "Unidentified",
                 "Unidentified flagellates",
                 "Unidentified flagellates",
                 "Unidentifred ciliates",
                 "Unidentifred oligotrich ciliates",
                 "Unidentifred tintinnids",
                 "Nauplie",
                 "Parasitisk copepod",
                 "Unidentified",
                 "Unidentifred ciliates")

  if(name %in% ErrorList){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

FixNames<-function(name){
  # fixes to name used before searching in WoRMS
  name<-gsub("  "," ",name) # replace two spaces with one
  name<-gsub(" spp\\.","",name)
  name<-gsub(" sp\\.","",name)
  name<-gsub(" gr\\.","",name)
  name<-gsub(" indet\\.","",name)
  name<-gsub(" indet","",name)
  name<-gsub(" s\\.l\\.","",name)
  name<-gsub("Acrchaetium","Acrochaetium",name)
  name<-gsub(" tetrasporophyte","",name)
  name<-gsub(" \\(æg\\)","",name)
  name<-gsub(" \\(egg\\)","",name)
  name<-gsub(" eggs","",name)
  name<-gsub(" nauplier","",name)
  name<-gsub(", cysts","",name)
  name<-gsub(" cysts","",name)
  name<-gsub(" cysts","",name)
  name<-gsub("pungitus","pungitius",name)
  name<-gsub("spioniidae","spionidae",name)
  name<-gsub("f. scrubsolei","",name)
  name<-gsub("f. semispina","",name)
  name<-gsub("f. truncata","",name)
  name<-gsub("v. variabile","",name)
  name<-gsub("-group","",name)
  name<-gsub(" group","",name)
  name<-gsub(" <celle>","",name)
  name<-gsub(", Børsteorme","",name)
  name<-gsub("HYDRACARINA I, Vandmider","Hydracarina",name)
  name<-gsub("MYSIDACEA, Mysider, Kårer","Mysidacea",name)
  name<-gsub("OSTRACODA, Muslingekrebs","Ostracoda",name)
  name<-gsub("ANTHOZOA, Koraldyr","Anthozoa",name)
  name<-gsub("ECHINODERMATA II, Ophiuroidea Slangestje","Echinodermata",name)
  name<-gsub("GASTROPODA, PROSOBRANCHIA, NUDIBRANCHIA","Nudibranchia",name)
  name<-gsub("HYDROZOA, athecata","Athecata",name)
  name<-gsub("anguste-tabulatum","angustitabulatum",name)
  name<-gsub("flos-aquae","flosaquae",name)
  name<-gsub("BIVALVIA, muslinger","Bivalvia",name)
  name<-gsub("NEMATODA, Rundorme","Nematoda",name)
  name<-gsub("POLYCHAETA, Havbørsteorme","Polychaeta",name)
  name<-gsub("NEMERTINI Slimbændler","Nemertini",name)
  name<-gsub("HYDROZOA, Polypdyr","Hydrozoa",name)
  name<-gsub("Chaetoceros, phaeoceros-group","Chaetoceros (Phaeoceros)",name)
  name<-gsub(", delicatissima"," delicatissima",name)
  name<-gsub(", seriata"," seriata",name)
  
  #name<-gsub("","",name)
  return(name)
}


GetSpeciesIDFuzzy<-function(searchtext){
  # ---------- get the AphiaID from the search text using fuzzy match  -----------------------------------------------
  #Build the URL to get the data from
  searchtext2 <- gsub(" ","%20",searchtext)
  url<-sprintf("http://marinespecies.org/rest/AphiaRecordsByMatchNames?scientificnames[]=%s&marine_only=false",searchtext2)
  x<-http_status(GET(url))
  ID<-""
  if(x$reason!="OK"){
    cat(paste0(searchtext,": ",x$reason,"\n"))
    return(ID)
  }
  AphiaRecord <- fromJSON(url) 
  if(length(AphiaRecord)>0){
    AphiaRecord <- as.data.frame(AphiaRecord[[1]])
    if(nrow(filter(AphiaRecord,status=="Accepted"))>0){
      AphiaRecord <- AphiaRecord %>% 
        filter(status=="Accepted")
    }
    ID<-AphiaRecord$AphiaID[1]
    cat(paste0(searchtext,": AphiaID=",ID,"\n"))
  }
  return(ID)

}

GetSpeciesID<-function(searchtext){
  # ---------- get the AphiaID from the search text -----------------------------------------------
  #Build the URL to get the data from
  
  df <- list(Species=searchtext,
                   AphiaID=NA,
                   ScientificName=NA)

  if(BadName(searchtext)==TRUE){
    # Species is in a list of names that won't return anything useful
    # e.g. "Blågrønalger og bakterier","Brown bush",
    cat(paste0("'",searchtext,"' is on the list of bad names. No search made\n"))
    return(df)
  }
  
  if(grepl("\\/", searchtext)==TRUE){
    # invalid character
    return(df)
  }
  
  searchtext2 <- gsub(" ","%20",searchtext)
  url<-sprintf("http://marinespecies.org/rest/AphiaIDByName/%s?marine_only=false",searchtext2)
  
  x<-http_status(GET(url))
  if(x$reason!="OK"){
    cat(paste0(searchtext,": ",x$reason," (trying fuzzy search instead)\n"))
    
    AphiaID <- GetSpeciesIDFuzzy(searchtext)
    if(AphiaID==""){
      return(df)
    }
  }else{
    #Get the AphiaID
    AphiaID <- fromJSON(url)
    cat(paste0(searchtext,": AphiaID=",AphiaID))
  }
  
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
