

#################################################################################
#
# Code developed during NISAR project (2019-2020) by Steen W. Knudsen at NIVA-DK
# R code for analysing filtered and extracted eDNA samples collected by the
# Danish environmental agency (Miløstyrelsen) (MST) over 2017-2018.
# 
# The water samples collected by MST are to be analysed for eDNA levels from 20
# non indeginuous marine species in Danish seas.
#
# The 20 non indeginuous marine species targeted for analysis are the same 20 
# species that the MONIS2, MONIS3 and MONIS4 project, carried out at NIVA-DK
#
# This code is prepared for analysis of the qPCR data obtained in the MONIS5 
# project by laboratorial work carried out in 2018-2019 by SWK
#
# The overall purpose of the present code is to analyse the eDNA levels inferred
# in the qPCR setups performed over 2018-2019.


# This code contains the following sections:
# 1 - Preparation of standard curves with qPCR eDNA levels plotted on to curves. 
#   These plots are prepared as copy numbers versus cycle treshold (Ct) values
# 2 - Plot of sampling locations on maps, for each species,
#   with indication of eDNA intensity for each location monitored
# 3 - Plot of sampling locations on maps, for each species,
#   with indication of eDNA intensity for each location monitored, 
#   and with interpolation of eDNA levels between sampled locations


#################################################################################

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Use ipdw package to interpolate between marine sampling locations
# interpolate between sampling locations using coastlines as barriers 
#-  as the fish swims, not as the crow flies!
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#code is prepared in 2019-Aug by Steen W. Knudsen
# and is able to run in :
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# > R.Version()
# $platform
# [1] "x86_64-apple-darwin13.4.0"
# $arch
# [1] "x86_64"
# $os
# [1] "darwin13.4.0"
# $system
# [1] "x86_64, darwin13.4.0"
# $status
# [1] ""
# $major
# [1] "3"
# $minor
# [1] "3.3"
# $year
# [1] "2017"
# $month
# [1] "03"
# $day
# [1] "06"
# $`svn rev`
# [1] "72310"
# $language
# [1] "R"
# $version.string
# [1] "R version 3.3.3 (2017-03-06)"
# $nickname
# [1] "Another Canoe"
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#remove everything in the working environment, without a warning!!
#rm(list=ls())

#libr.path <- "/home/sknu003/uoa00029_runs/Rplot_tryout" 
#.libPaths( c( libr.path, .libPaths()) ) 

#libr.path <- "/home/sknu003/uoa00029_runs/Rplot_tryout"
#libr.path <- "/scale_wlg_persistent/filesets/home/sknu003/R/x86_64-pc-linux-gnu-library/3.5"

#libr.path <- "/scale_wlg_persistent/filesets/home/sknu003/R/x86_64-pc-linux-gnu-library/3.6"
#.libPaths( c( .libPaths(), libr.path) )

#.libPaths()

#.libPaths( c( libr.path , .libPaths() ) )
#.libPaths()
#.libPaths(libr.path)
#.libPaths()
#chooseCRANmirror(graphics=FALSE)
#chooseCRANmirror(4)
#'chooseCRANmirror(graphics=FALSE, ind=4)'


#________________________________________________________________________________
# Install packages for making standard curve plots and exporting html tables
#________________________________________________________________________________
#see this
#website
#on how to only install required packages
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  scales, 
  fields, 
  gplots,
  plyr,
  ReporteRs)



## install the package 'scales', which will allow you to make points on your plot more transparent
#install.packages("scales")
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
library(scales)

#install.packages("fields")
if(!require(fields)){
  install.packages("fields")
  library(fields)
}
library(fields)

## install the package 'gplots', to be able to translate colors to hex - function: col2hex
#install.packages("gplots")
if(!require(gplots)){
  install.packages("gplots")
  library(gplots)
}
library(gplots)

## install the package 'glad', to be able to color using the function 'myPalette'
#install.packages("glad")
#library(glad)

require(graphics)

## install the package 'marmap', which will allow you to plot bathymetric maps
#install.packages("marmap")
#library(marmap)

#get the package that enables the function 'subplot'
#install.packages("TeachingDemos")
#library(TeachingDemos)

#get package to make maps
#install.packages("rworldmap")
#require (rworldmap)

#install.packages("rworldxtra")
#require(rworldxtra)

#get package to read excel files
#install.packages("readxl")
#library(readxl)

#get package to do count number of observations that have the same value at earlier records:
# see this website: https://stackoverflow.com/questions/11957205/how-can-i-derive-a-variable-in-r-showing-the-number-of-observations-that-have-th
#install.packages("plyr")
if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}
library(plyr)

#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("mapdata")
#library(mapdata)

#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("maps")
#library(maps)
# #get package for shapefiles see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
# install.packages(maptools)
# library(maptools)  #for shapefiles

# #get package for adding pies on the map
#install.packages("mapplots")
#library(mapplots)

#get the packages required for exporting to a table to word
#install.packages("ReporteRs")
if(!require(ReporteRs)){
  install.packages("ReporteRs")
  library(ReporteRs)
}

devtools::install_github("davidgohel/ReporteRs")
devtools::install_github("davidgohel/officer")

if(!require(officer)){
  install.packages("officer")
  library(officer)
}
library(ReporteRs)
library(officer)


#install.packages("tableHTML")
#https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
if(!require(tableHTML)){
  install.packages("tableHTML")
  library(tableHTML)
}
require(tableHTML)





#####################################################################################

# #get package for adding pies and bars on the map
if(!require(mapplots)){
  install.packages("mapplots")
  library(mapplots)
}
#install.packages("mapplots")
library(mapplots)
#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
if(!require(mapdata)){
  install.packages("mapdata")
  library(mapdata)
}
#install.packages("mapdata")
library(mapdata)
#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("maps")
if(!require(maps)){
  install.packages("maps")
  library(maps)
}
library(maps)
#get the package that enables the function 'subplot'
#install.packages("TeachingDemos")
if(!require(TeachingDemos)){
  install.packages("TeachingDemos")
  library(TeachingDemos)
}
library(TeachingDemos)
#get package to make maps
#install.packages("rworldmap")
if(!require(rworldmap)){
  install.packages("rworldmap")
  library(rworldmap)
}
require (rworldmap)
#get another package to make maps
#install.packages("rworldxtra")
if(!require(rworldxtra)){
  install.packages("rworldxtra")
  library(rworldxtra)
}
require(rworldxtra)
## install the package 'scales', 
#which will allow you to make points on your plot more transparent
#install.packages("scales")
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
library(scales)
#install.packages("fields")
if(!require(fields)){
  install.packages("fields")
  library(fields)
}
library(fields)
## install the package 'marmap', which will allow you to plot bathymetric maps
#install.packages("marmap")
if(!require(marmap)){
  install.packages("marmap")
  library(marmap)
}
library(marmap)



#________________________________________________________________________________
# Install packages for making maps with eDNA levels mapped and interpolation
# between points
#________________________________________________________________________________

#________________________________________________________________________________
# - use the two spatial dataframes as in this example    https://jsta.github.io/ipdw/articles/ipdw2.html

# also check out this website: https://globalfishingwatch.org/data-blog/working-with-our-downloadable-public-data-in-r/
# and this website: https://www.molecularecologist.com/2015/07/marmap/
#________________________________________________________________________________
# get the rgeos package
if(!require(rgeos)){
  install.packages("rgeos", repos='http://cran.us.r-project.org')
}  
library(rgeos)

# get the ipdw package
if(!require(ipdw)){
  install.packages("ipdw", repos='http://cran.us.r-project.org')
}  
library(ipdw)

# get the scales package
if (!requireNamespace("scales", quietly=TRUE))
  install.packages("scales", repos='http://cran.us.r-project.org')
library(scales)


# get the sf package
if (!requireNamespace("sf", quietly=TRUE))
  install.packages("sf", repos='http://cran.us.r-project.org')
library(sf)

# get the rnaturalearth package
if (!requireNamespace("rnaturalearth", quietly=TRUE))
  install.packages("rnaturalearth", repos='http://cran.us.r-project.org')
library(rnaturalearth)

#Read in the rgdal library
if (!requireNamespace("rgdal", quietly=TRUE))
  install.packages("rgdal", repos='http://cran.us.r-project.org')
library(rgdal)

# get the sp package
if (!requireNamespace("sp", quietly=TRUE))
  install.packages("sp", repos='http://cran.us.r-project.org')
library(sp)

#https://www.rdocumentation.org/packages/biogeo/versions/1.0/topics/dms2dd
# get biogeo package to be able to use 'dms2dd' function
if(!require(biogeo)){
  install.packages("biogeo", repos='http://cran.us.r-project.org')
}
library(biogeo)
#https://www.rdocumentation.org/packages/rgdal/versions/1.3-6/topics/readOGR

#:::::::::::::::::example data set below to try out - start :::::::::::::
# #some values for a dataframe 
# X_UTM <- c(494687.0, 575538.9, 609334.6, 605383.9, 642761.1, 467847.1, 541688.4, 594828.5, 598755.3, 599125.8, 746990.6, 613889.7, 612131.3)
# Y_UTM <- c(6393366, 6452774, 6409207, 6361790, 6336866, 6374410, 6381022, 6354321, 6332203, 6294628, 6085877, 6053287, 6177412)
# zval <- c(1.5, 2.3, 5.6, 8.1, 7.3, 4.6, 3.7, 6.1, 1.8, 2.3, 2.7, 2.8, 7.6)
# #assemble to a dataframe
# loc_DK <- data.frame(X_UTM,Y_UTM,zval)
# #convert from UTM to decimal lat long # https://stackoverflow.com/questions/45230760/convert-utm-to-decimal-degree-in-r
# utmcoor<-SpatialPoints(cbind(loc_DK$X_UTM,loc_DK$Y_UTM), proj4string=CRS("+proj=utm +zone=32"))
# longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
# #extract from spatial points datafram and append back
# loc_DK$declon2 <- coordinates(longlatcoor)[,1]
# loc_DK$declat2 <- coordinates(longlatcoor)[,2]
#:::::::::::::::::example data set below to try out - end :::::::::::::

###########################################################################
# Set working directory and read in csv files

# set working directory to data folder
# setwd("pathToDirHere")
#setwd("/Users/steenknudsen/R_koder")
wd <- "/Users/steenknudsen/Documents/Documents/NIVA_Ansaettelse_2019sep/NISAR_analysis"
wd<-"../7-Data - 190183 NISAR/eDNA/"

setwd(wd)
getwd()
#read in the qPCR data with the eDNA reads 
MONIS5eDNA01_df <- 
  read.csv("outfile02_merged_mxpro_txtrepfls_MONIS5.csv", sep = ";",
           stringsAsFactors = FALSE)
MONIS5eDNA02_df <- MONIS5eDNA01_df
#read in the species specific assays used
MONIS3.ls.assays01_df <- 
  read.csv("MONIS3_lst_of_spcfc_eDNA_assays.csv", sep = ",",
           stringsAsFactors = FALSE)
#read in the collected and filtered and extracted water samples
MST_smpls01_df <- 
  read.csv("MST_samples_2017_2018.csv", sep = ",",
           stringsAsFactors = FALSE)

###########################################################################
#keep only selected columns
colnames(MONIS3.ls.assays01_df)
keeps <- c("Assay_ID",
           "Common_name_Danish",
           "Lat_Species",
           "Genus",
           "species")
#keep only selected columns
MONIS3.ls.assays02_df <- MONIS3.ls.assays01_df[keeps]
#get number of columns
n.col.MO3 <- length(MONIS3.ls.assays02_df)
#get only unique columns
MONIS3.ls.assays03_df <- unique(MONIS3.ls.assays02_df[,1:n.col.MO3])
#replace the Danish letters
com.nm.Dan01 <- gsub("\xbf","oe",MONIS3.ls.assays03_df$Common_name_Danish)
com.nm.Dan02 <- gsub("\x8c","aa",com.nm.Dan01)
com.nm.Dan03 <- gsub(" ","_",com.nm.Dan02)
#append back to df
MONIS3.ls.assays03_df$Common_name_Danish <- com.nm.Dan03
# replace "Magallana" with "Crassostrea" # the previous genus name used to be "Crassostrea"
MONIS3.ls.assays03_df$Genus <- gsub("Magallana","Crassostrea",MONIS3.ls.assays03_df$Genus)
MONIS3.ls.assays03_df$Lat_Species <- gsub("Magallana","Crassostrea",MONIS3.ls.assays03_df$Lat_Species)
# replace "serruculata" with "verruculosa" ## the previous species name used to be "serruculata"
MONIS3.ls.assays03_df$species <- gsub("serruculata","verruculosa",MONIS3.ls.assays03_df$species)
MONIS3.ls.assays03_df$Lat_Species <- gsub("serruculata","verruculosa",MONIS3.ls.assays03_df$Lat_Species)

# split text - see: https://stevencarlislewalker.wordpress.com/2013/02/13/remove-or-replace-everything-before-or-after-a-specified-character-in-r-strings/
# and concatenate text - see: https://stackoverflow.com/questions/7201341/how-can-2-strings-be-concatenated 
# to get 6 letter abbr of latin speciesnames
ls.abbr.spcnm <-  paste(
  substr(sub('\\_.*', '', MONIS3.ls.assays03_df$Genus), 1, 3),
  substr(sub('.*\\_', '', MONIS3.ls.assays03_df$species), 1, 3),
  sep="."
)
#remove point with gsub
ls.abbr.spcnm <- gsub("\\.","",ls.abbr.spcnm)
#add back on to latin name dataframe
MONIS3.ls.assays03_df$abbr.nm <- ls.abbr.spcnm
#remove blanks #NOTE!! This will remove all NTC's with "No Ct"
MONIS5eDNA02_df<-na.omit(MONIS5eDNA02_df)
#remove "No Ct"
MONIS5eDNA02_df<-MONIS5eDNA02_df[!grepl("NoCt", MONIS5eDNA02_df$Quantitycopies),]
#change x into numeric variable
MONIS5eDNA02_df$CtdRn=as.numeric(as.character(MONIS5eDNA02_df$CtdRn))
MONIS5eDNA02_df$Quantitycopies=as.numeric(as.character(MONIS5eDNA02_df$Quantitycopies))
#head(MONIS5eDNA02_df,5)
#match between dataframes to add latin species names and DK common names
MONIS5eDNA02_df$gen_specnm <- MONIS3.ls.assays03_df$Lat_Species[match(MONIS5eDNA02_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
MONIS5eDNA02_df$Genus <- MONIS3.ls.assays03_df$Genus[match(MONIS5eDNA02_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
MONIS5eDNA02_df$species <- MONIS3.ls.assays03_df$species[match(MONIS5eDNA02_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
MONIS5eDNA02_df$dk_comnm <- MONIS3.ls.assays03_df$Common_name_Danish[match(MONIS5eDNA02_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
#check if both lists have the same species - and identify the missing species
sp.A <- unique(MONIS5eDNA02_df$gen_specnm)
sp.B <- MONIS3.ls.assays03_df$Lat_Species
unique(sp.B[! sp.B %in% sp.A])
#match between dataframes
MONIS5eDNA02_df$Lok_omr01 <- MST_smpls01_df$Lok_omr01[match(MONIS5eDNA02_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA02_df$Dato_inds <- MST_smpls01_df$Dato_inds[match(MONIS5eDNA02_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA02_df$Vwf_mL <- MST_smpls01_df$Vwf_mL[match(MONIS5eDNA02_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA02_df$lok_pos_lat <- MST_smpls01_df$lok_pos_lat[match(MONIS5eDNA02_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA02_df$lok_pos_lon <- MST_smpls01_df$lok_pos_lon[match(MONIS5eDNA02_df$smpltp, MST_smpls01_df$U_Pr_Nr)]


#unique(MONIS5eDNA02_df$smpltp)
#paste a new column based on variables separated by point
MONIS5eDNA02_df$Lok_omr01.Welltype <- paste(MONIS5eDNA02_df$Lok_omr01, MONIS5eDNA02_df$WellType,  sep=".")
#get the unique smpl names for locations and WellTypes
#this will allow you to assign a fixed colour to the sampling locations
unHaWT <- unique(MONIS5eDNA02_df$Lok_omr01.Welltype)
# make a transparent color
transp_col <- rgb(0, 0, 0, 0)
#transp_col <- as.character("#FFFFFF")
HaWTnoNA <- addNA(unHaWT)
col.01<-as.numeric(as.factor(unHaWT))
#make a small dataframe w harbours and standards and numbers assigned, 
#use the col2hex in gplot pacakge to convert the 'red' color name to hex-color
col.02 <- col2hex(palette(rainbow(length(col.01))))
lok.cols <- cbind(unHaWT,col.01, col.02)
length(unHaWT)
length(col.01)
length(col.02)
#replace the colour for the standard dilution sample type with the transparent colour
col.03<-replace(col.02, unHaWT=="NA.Standard", transp_col)
col.04 <- cbind(lok.cols,col.03)
#this dataframe now has unique standardized colors for each sampling locality
colfor.lok <- as.data.frame(col.04)
#match to main data frame and add as new color
MONIS5eDNA02_df$col.06 <- colfor.lok$col.03[match(MONIS5eDNA02_df$Lok_omr01.Welltype, colfor.lok$unHaWT)]
#insert the transparent color for all matches with "NA.Standard"
MONIS5eDNA02_df$col.06[MONIS5eDNA02_df$Lok_omr01.Welltype=="NA.Standard"] <- transp_col


######################################################################################
#   Appendix A - start
######################################################################################
####################################################################################
#
# prepare std dilution curve plots for each for species
#
####################################################################################
#first get unique species names 
#get the unique species names
latspecnm <- unique(MONIS5eDNA02_df$gen_specnm)
#match the assay number to the data frame with species
AIfps <- MONIS3.ls.assays03_df$Assay_ID[match(latspecnm, MONIS3.ls.assays03_df$Lat_Species)]
#pad with zeros to two characters
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
AIfps <-stringr::str_pad(AIfps, 2, pad = "0")
#make a new data frame with assay Id No and species
nlspnm <- data.frame(AIfps,latspecnm)
#reorder by the column 'AssayIDNo'
nlspnm<- nlspnm[order(nlspnm$AIfps),]
#make a list of numbers for the unique species
no.latspc <- seq(1:length(latspecnm))
#add a new column with no to use for appendix numbering
nlspnm <- cbind(nlspnm, no.latspc) 
#use the new order of latin species names for producing plots
latspecnm <- unique(nlspnm$latspecnm)




######################################################################################
#   make standard curve plots for each species for each season 
######################################################################################
#Use a copy of the data frame to iterate over
amp <- MONIS5eDNA02_df
########################################################
# for loop start here
########################################################
#latspecnm  <- "Mnemiopsis leidyi"
#spec.lat  <- "Mnemiopsis leidyi"
# loop over all species names in the unique list of species, and make plots. 
#Notice that the curly bracket ends after the pdf file is closed
for (spec.lat in latspecnm){
  #print(spec.lat)
  #}
  #get the Danish commom name
  #first split the string by the dot
  #https://stackoverflow.com/questions/33683862/first-entry-from-string-split
  #and escape the dot w two backslashes
  latnm <- sapply(strsplit(spec.lat,"\\."), `[`, 1)
  #get Danish common name
  sbs.dknm <- MONIS3.ls.assays03_df$Common_name_Danish[match(latnm, MONIS3.ls.assays03_df$Lat_Species)]
  #get AssIDNo
  sbs.AssIDNo <- MONIS3.ls.assays03_df$Assay_ID[match(latnm, MONIS3.ls.assays03_df$Lat_Species)]
  #get the number for the appendix plot number
  no.spc.app.plot <- nlspnm$no.latspc[match(spec.lat, nlspnm$latspecnm)]
  #pad with zeros to two characters
  #see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
  no.spc.app.plot <-stringr::str_pad(no.spc.app.plot, 2, pad = "0")
  #get the latin species nam without underscore
  spec.lat.no_undersc <- paste(sub('_', ' ', spec.lat))
  # Exporting PFD files via postscript()           
  pdf(c(paste("App_A",no.spc.app.plot,"_plot_qpcr_MONIS5_AssID",sbs.AssIDNo,"_",spec.lat,"_std_dilution_series.pdf",  sep = ""))
      ,width=(1.6*8.2677),height=(2*1.6*2*2.9232))
  #op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
  op <- par(mfrow=c(1,1), # set number of panes inside the plot - i.e. c(2,2) would make four panes for plots
            oma=c(1,1,0,0), # set outer margin (the margin around the combined plot area) - higher numbers increase the number of lines
            mar=c(5,5,5,5) # set the margin around each individual plot 
  )
    #subset based on variable values, subset by species name 
    sbs.amp <- amp[ which(amp$gen_specnm==spec.lat), ]
    #identify LOD
    lod.id.df<-sbs.amp[(sbs.amp$WellType=='Standard'),]
    lod.val<-min(lod.id.df$Quantitycopies)
    #identify LOQ
    #limit the dataframe to only well type that equals standard
    zc<-sbs.amp[(sbs.amp$WellType=='Standard'),]
    #count the occurences of dilution steps - i.e. the number of succesful replicates
    #see this webpage: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
    #zd<-count(zc, "WellName")
    zd<-count(zc, "Quantitycopies")
    #turn this into a dataframe
    ze<-as.data.frame(zd)
    #match the dilution step to the number of occurences -i.e. match between the two dataframes
    no.occ <- ze$freq[match(zc$Quantitycopies,ze$Quantitycopies)]
    #add this column with counted occurences to the limited dataframe
    zg <- cbind.data.frame(zc,no.occ)
    #exlude all observations where less than 3 replicates amplified
    zh<-zg[(zg$no.occ>=3),]
    #get the lowest dilution step that succesfully ampllified on all 3 repliactes
    loq.val=min(zh$Quantitycopies)
    #Conditionally Remove Dataframe Rows with R
    #https://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
    sbs.pamp<-sbs.amp[!(sbs.amp$WellType=='Standard' & sbs.amp$Quantitycopies<=5),]
    
    #__________________# plot1   - triangles________________________________________
    # Exporting EPS files via postscript()
    # postscript(c(paste("plot_qpcr_MONIS3_",sbs.AssIDNo,"_",spec.lat,"_std_dilution_series.eps", sep = "")),
    #             width=(1.6*8.2677),height=(2*1.6*2.9232),
    #             #family = "Arial", 
    #             paper = "special", onefile = FALSE,
    #             horizontal = FALSE)
    ##  Create a data frame with eDNA
    y.sbs.amp <- sbs.amp$CtdRn
    x.sbs.amp <- sbs.amp$Quantitycopies
    d.sbs.famp <- data.frame( x.sbs.amp = x.sbs.amp, y.sbs.amp = y.sbs.amp )
    #get( getOption( "device" ) )()
    plot(
      y.sbs.amp ~ x.sbs.amp,
      data = d.sbs.famp,
      type = "n",
      log  = "x",
      las=1, # arrange all labels horizontal
      xaxt='n', #surpress tick labels on x-axis
      yaxt='n', #surpress tick labels on y-axis
      #main=c(paste("qPCR standard curve - for ",sbs.AssIDNo,"\n-",spec.lat,seas,"(",sbs.dknm,")"),  sep = ""), 
      
      #add a title with bquote
      main=c(bquote('qPCR standard curve for'~italic(.(spec.lat.no_undersc))
                    ~'('~.(sbs.dknm)~'), '
                    ~'AssayNo'~.(sbs.AssIDNo) #~', '
                    #~.(eng.seas)
      )),
      #offset = 2,
      #sub="sub-title",
      xlab="target-eDNA in extract. (copy/qPCR-reaction)",
      ylab="Ct",
      #xlim = c( 0.1, 1000000000 ),
      #ylim = c( 10, 50 )
      xlim = c( 0.234, 0.428*1000000000 ),
      ylim = c( 9.55, 48.446 )
    )
    #add labels to the points
    pos_vector <- rep(3, length(sbs.amp$Lok_omr01))
    #pos_vector[sbs.amp$Harbour %in% c("Roedby", "Aalborgportland", "KalundborgStatiolHavn")] <- 4
    #pos_vector[sbs.amp$Harbour %in% c("AalborgHavn")] <- 2
    text(x.sbs.amp, y.sbs.amp, labels=sbs.amp$Lok_omr01, cex= 0.8, pos=pos_vector, las=3)
    ##  Put grid lines on the plot, using a light blue color ("lightsteelblue2").
    # add horizontal lines in grid
    abline(
      h   = c( seq( 8, 48, 2 )),
      lty = 1, lwd =0.6,
      col = colors()[ 225 ]
    )
    
    # add vertical lines in grid
    abline(
      v   = c( 
        seq( 0.1, 1, 0.1 ),
        seq( 1e+0, 1e+1, 1e+0 ),
        seq( 1e+1, 1e+2, 1e+1 ),
        seq( 1e+2, 1e+3, 1e+2 ),
        seq( 1e+3, 1e+4, 1e+3 ),
        seq( 1e+4, 1e+5, 1e+4 ), 
        seq( 1e+5, 1e+6, 1e+5 ),
        seq( 1e+6, 1e+7, 1e+6 ),
        seq( 1e+7, 1e+8, 1e+7 ),
        seq( 1e+8, 1e+9, 1e+8 )),
      lty = 1, lwd =0.6,
      col = colors()[ 225 ]
    )
    # add line for LOQ
    abline(v=loq.val, lty=2, lwd=1, col="black")
    text(loq.val*0.7,15,"LOQ",col="black",srt=90,pos=1, font=1)
    
    # add line for LOD 
    abline(v=lod.val, lty=1, lwd=1, col="red")
    text(lod.val*0.7,22,"LOD",col="red",srt=90,pos=1, font=1)
    
    # add line for Ct-cut-off
    abline(h=seq(41,100,1000), lty=1, lwd=3, col="darkgray")
    text(10,40.6,"cut-off",col="darkgray",srt=0,pos=3, font=2, cex=1.2)
    
    # make a transparent color
    #transp_col <- rgb(0, 0, 0, 0)
    #make numbers for the sample type
    #convert NAs to a number 
    # https://stackoverflow.com/questions/27195956/convert-na-into-a-factor-level
    #sbs.amp.stndnm <- addNA(sbs.amp$Harbour)
    #col.01<-as.numeric(as.factor(sbs.amp.stndnm))
    #make a small dataframe w harbours and standards and numbers assigned, 
    #check that the standard is matched up with the transparent color - currently no 17 or 16 ?
    #harbourcols <- cbind(sbs.amp.stndnm,col.01,sbs.amp$Harbour)
    #replace the colour for the standard dilution sample type with the transparent colour
    #col.02<-replace(col.01, col.01==16, transp_col)
    #col.04 <- colforharb$col.02[match(sbs.amp$Harbour.Welltype, colforharb$unHaWT)]
    
    ##  Draw the points over the grid lines.
    points( y.sbs.amp ~ x.sbs.amp, data = d.sbs.famp, 
            pch=c(24), lwd=1, cex=1.8,
            bg=as.character(sbs.amp$col.06)
    )
    #edit labels on the x-axis
    ticks <- seq(-1, 9, by=1)
    labels <- sapply(ticks, function(i) as.expression(bquote(10^ .(i))))
    axis(1, at=c(0.1, 1, 10, 1e+2, 1e+3, 1e+4, 1e+5, 1e+6, 1e+7, 1e+8, 1e+9), pos=8, labels=labels)
    #edit labels on the y-axis
    axis(side=2, at=seq(8, 50, by = 2), las=1, pos=0.1)
    
    #estimate a model for each STD subset incl below LOQ
    sbs.amp$x <- sbs.amp$Quantitycopies
    sbs.amp$y<- sbs.amp$CtdRn
    logEst.amp_STD <- lm(y~log(x),sbs.amp)
    
    #add log regresion lines to the plot
    with(as.list(coef(logEst.amp_STD)),
         curve(`(Intercept)`+`log(x)`*log(x),add=TRUE,
               lty=1))
    
    #estimate a model for each STD subset for dilution steps above LOQ
    ab.loq.sbs.amp<-zh # get the previously limited dataframe from identifying LOQ
    ab.loq.sbs.amp$x <- ab.loq.sbs.amp$Quantitycopies
    ab.loq.sbs.amp$y<- ab.loq.sbs.amp$CtdRn
    logEst.abloqamp_STD <- lm(y~log(x),ab.loq.sbs.amp) #make a linear model
    
    #add log regresion lines to the plot
    with(as.list(coef(logEst.abloqamp_STD)),
         curve(`(Intercept)`+`log(x)`*log(x),add=TRUE,
               lty=1, col="red"))
    
    #add 95% confidence intervals around each fitted line
    #inspired from this webpage
    #https://stat.ethz.ch/pipermail/r-help/2007-November/146285.html
    #for the first line - with below LOQ
    newx<-seq(lod.val,1e+6,1000)
    prdlogEst.amp_STD<-predict(logEst.amp_STD,newdata=data.frame(x=newx),interval = c("confidence"), 
                               level = 0.95, type="response")
    prd2logEst.amp_STD<- prdlogEst.amp_STD
    #polygon(c(rev(newx), newx), c(rev(prd2[ ,3]), prd2[ ,2]), col = 'grey80', border = NA)
    lines(newx,prd2logEst.amp_STD[,2],col="black",lty=2)
    lines(newx,prd2logEst.amp_STD[,3],col="black",lty=2)
    #add 95% conf. intervals for the second line - only above LOQ
    newx<-seq(loq.val,1e+6,100)
    prdlogEst.abloqamp_STD<-predict(logEst.abloqamp_STD,newdata=data.frame(x=newx),interval = c("confidence"), 
                                    level = 0.95 , type="response")
    prd2logEst.abloqamp_STD<- prdlogEst.abloqamp_STD
    #polygon(c(rev(newx), newx), c(rev(prd2[ ,3]), prd2[ ,2]), col = 'grey80', border = NA)
    lines(newx,prd2logEst.abloqamp_STD[,2],col="red",lty=2)
    lines(newx,prd2logEst.abloqamp_STD[,3],col="red",lty=2)
    # add a legend for colors on points
    legend(1e+7*0.5,49,
           unique(sbs.amp$Lok_omr01.Welltype),
           pch=c(24),
           bg="white",
           #NOTE!! the hex color numbers must be read as characters to translate into hex colors
           pt.bg = as.character(unique(sbs.amp$col.06)),
           y.intersp= 0.7, cex=0.9)
    # add a second legend for types of regression lines
    legend(1000,49,
           c("incl below LOQ","excl below LOQ"),
           #pch=c(24), #uncomment to get triangles on the line in the legend
           cex=0.8,
           bg="white",
           lty=c(1), col=c("black","red"),
           y.intersp= 0.7)
    #title(main=c(paste("qPCR standard curve - for ",spec.lat,"\n-(",sbs.dknm,")"),  sep = ""), 
    #        col.main="red",
    #    sub="My Sub-title", col.sub="blue",
    #    xlab="My X label", ylab="My Y label",
    #    col.lab="green", cex.lab=0.75)

  # add title for the pdf-page
  mtext(c(paste("Appendix A",no.spc.app.plot,"."),  sep = ""), outer=TRUE, 
        #use at , adj and padj to adjust the positioning
        at=par("usr")[1]+0.15*diff(par("usr")[1:2]),
        adj=3.4,
        padj=2,
        #use side to place it in te top
        side=3, cex=1.6, line=-1.15)
  
  #apply the par settings for the plot as defined above.
  par(op)
  # end pdf file to save as
  dev.off()  
  ########################################################
  # for loop on species end here
  ########################################################
  
}
######################################################################################
######################################################################################
#   Appendix A - end
######################################################################################












#################################################################################
#  plot eDNA copies per L on map for season per species
#################################################################################

# #get package for adding pies and bars on the map
if(!require(mapplots)){
  install.packages("mapplots")
  library(mapplots)
}
#install.packages("mapplots")
library(mapplots)
#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
if(!require(mapdata)){
  install.packages("mapdata")
  library(mapdata)
}
#install.packages("mapdata")
library(mapdata)
#get package to make maps - see this website: http://www.molecularecologist.com/2012/09/making-maps-with-r/
#install.packages("maps")
if(!require(maps)){
  install.packages("maps")
  library(maps)
}
library(maps)
#get the package that enables the function 'subplot'
#install.packages("TeachingDemos")
if(!require(TeachingDemos)){
  install.packages("TeachingDemos")
  library(TeachingDemos)
}
library(TeachingDemos)
#get package to make maps
#install.packages("rworldmap")
if(!require(rworldmap)){
  install.packages("rworldmap")
  library(rworldmap)
}
require (rworldmap)
#get another package to make maps
#install.packages("rworldxtra")
if(!require(rworldxtra)){
  install.packages("rworldxtra")
  library(rworldxtra)
}
require(rworldxtra)
## install the package 'scales', 
#which will allow you to make points on your plot more transparent
#install.packages("scales")
if(!require(scales)){
  install.packages("scales")
  library(scales)
}
library(scales)
#install.packages("fields")
if(!require(fields)){
  install.packages("fields")
  library(fields)
}
library(fields)
## install the package 'marmap', which will allow you to plot bathymetric maps
#install.packages("marmap")
if(!require(marmap)){
  install.packages("marmap")
  library(marmap)
}
library(marmap)



#################################################################################
#  prepare tables for  concentration in copies per Liter of seawater
#################################################################################
#copy the data frame
MONIS5eDNA03_df <- MONIS5eDNA02_df
#set NA blanks to zero
MONIS5eDNA03_df$CtdRn[is.na(MONIS5eDNA03_df$CtdRn)] <- 0
MONIS5eDNA03_df$Quantitycopies[is.na(MONIS5eDNA03_df$Quantitycopies)] <- 0
#make sure numbers are numbers
MONIS5eDNA03_df$Quantitycopies <- as.numeric(as.character(MONIS5eDNA03_df$Quantitycopies))
MONIS5eDNA03_df$Vwf_mL <- as.numeric(as.character(MONIS5eDNA03_df$Vwf_mL))
#add column with copies per Liter of filtered water
#Ae = (Cqpcr /Fe) /Vwf. 
#’Ae’ number of  eDNA-copies per volumen filtered water, 
#’Cqpcr’ number of copies detected in the qPCR-well, #smpls02.1$meanQuantitycopies 
#’Fe’ the ratio of the eluted extrated filtrate used in a qPCR-well #5/350
# For the qPCR 5 uL of extracted eDNA from the filter sample was used
# For the extraction of eDNA from the filters the extracted eDNA was eluated in 350 uL
#’Vwf’ is volumen of seawater filtered.
rt <- 5/350
# get the template volume used for qPCR setups
MONIS5eDNA03_df$templvol2 <- as.numeric(substr(MONIS5eDNA03_df$templvol, 1, 1))
# relate the volume of template used in qPCR to recalculate the ratio of
#extracted eluate used
rt <- (MONIS5eDNA03_df$templvol2/350)
#per mL
MONIS5eDNA03_df$copies_per_mLwater <- (MONIS5eDNA03_df$Quantitycopies/(rt))/MONIS5eDNA03_df$Vwf_mL
#per Liter
MONIS5eDNA03_df$copies_per_Lwater <- MONIS5eDNA03_df$copies_per_mLwater*1000
#replace nas with zeros
MONIS5eDNA03_df$copies_per_Lwater[is.na(MONIS5eDNA03_df$copies_per_Lwater)]<-0
#add one to be able to do logarithmic scales
MONIS5eDNA03_df$copies_per_Lwater_plone<- MONIS5eDNA03_df$copies_per_Lwater+1
#take log10 to all copies
MONIS5eDNA03_df$log.10_copies_L <- log10(MONIS5eDNA03_df$copies_per_Lwater_plone)
# following this example: https://stackoverflow.com/questions/41336606/accessing-element-of-a-split-string-in-r
#split the collection date, and get the third element, to get the year
MONIS5eDNA03_df$year_inds <- MONIS5eDNA03_df$Dato_inds %>%
  strsplit( "/" ) %>%
  sapply( "[", 3 )
#check what years the samples have been collected
unique(MONIS5eDNA03_df$year_inds)
#paste latin species names and year for collection together
MONIS5eDNA03_df$gen_specnm.year <- paste(MONIS5eDNA03_df$gen_specnm,MONIS5eDNA03_df$year_inds, sep=".")
#get the LOD (Limit of detection) for each set of qPCR run
#use the function aggregate to get the minimum value for a group
lodtable1 <- aggregate(MONIS5eDNA03_df[, "Quantitycopies"], list(MONIS5eDNA03_df$gen_specnm.year, MONIS5eDNA03_df$WellType), min)
#subset this table by group
lodtable2 <- lodtable1[ which(lodtable1$Group.2=="Standard"), ]
#rename the column names
colnames(lodtable2) <- c("spc.year","WellT","LOD")
#identify LOQ for each qPCR run
#limit the dataframe to only well type that equals standard
oc<-MONIS5eDNA03_df[(MONIS5eDNA03_df$WellType=='Standard'),]
#add a new column that merges two columns
oc$Quan.spc.years <- paste(oc$Quantitycopies, oc$gen_specnm.year,  sep=".")
#count the occurences of dilution steps - i.e. the number of succesful replicates
#see this webpage: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
#and this webpage: https://stackoverflow.com/questions/9809166/count-number-of-rows-within-each-group
od<-count(oc, c("Quantitycopies","gen_specnm.year"))
#turn this into a dataframe
oe<-as.data.frame(od)
#add a new column that merges two columns
oe$Quan.spc.years <- paste(as.character(oe$Quantitycopies), oe$gen_specnm.year,  sep=".")
#match the dilution step to the number of occurences -i.e. match between the two dataframes
no.occ <- oe$freq[match(oc$Quan.spc.years,oe$Quan.spc.years)]
#add this column with counted occurences to the limited dataframe
og <- cbind.data.frame(oc,no.occ)
#exlude all observations where less than 3*3 replicates amplified
oh<-og[(og$no.occ>=9),]
#get the lowest dilution step that succesfully ampllified on all 3*3 repliactes
# there were 3 qPCR runs per species, and each run included 
#3 replicates of each standard dilution step
#use aggregate to get the minimum for each
loqtable1 <- aggregate(oh[, "Quantitycopies"], list(oh$gen_specnm.year), min)
#change the column names
colnames(loqtable1) <- c("spc.year","LOQ")
#copy the LOD table and add the corresponding LOQ values
loq.lod.table <- lodtable2
#match back to data frame
loq.lod.table$LOQ <- loqtable1$LOQ[match(lodtable2$spc.year,loqtable1$spc.year)]

#make a copy of the original data frame
# The previous copy of the original data frame had all wells with zero
# qPCR reads removed -  you will need to include the zero detections
MONIS5eDNA04_df <- MONIS5eDNA01_df
#Replace all NoCts with zero
MONIS5eDNA04_df$Quantitycopies[MONIS5eDNA04_df$Quantitycopies=="NoCt"] <- 0
#delete rows that have "NoCtforFAMStandards" in the column 'smpls03$Quantitycopies'
#this error is caused by the wrong qpcr plate setup for  'Pseudochattonella_farcimen'
MONIS5eDNA04_df<-MONIS5eDNA04_df[!(MONIS5eDNA04_df$Quantitycopies=="NoCtforFAMStandards"),]
#change variable into numeric variable
MONIS5eDNA04_df$CtdRn=as.numeric(as.character(MONIS5eDNA04_df$CtdRn))
# replace NAs with zeros
MONIS5eDNA04_df$CtdRn[is.na(MONIS5eDNA04_df$CtdRn)] <- 0
MONIS5eDNA04_df$Quantitycopies=as.numeric(as.character(MONIS5eDNA04_df$Quantitycopies))
#match between dataframes to add latin species names and DK common names
MONIS5eDNA04_df$gen_specnm <- MONIS3.ls.assays03_df$Lat_Species[match(MONIS5eDNA04_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
MONIS5eDNA04_df$Genus <- MONIS3.ls.assays03_df$Genus[match(MONIS5eDNA04_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
MONIS5eDNA04_df$species <- MONIS3.ls.assays03_df$species[match(MONIS5eDNA04_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
MONIS5eDNA04_df$dk_comnm <- MONIS3.ls.assays03_df$Common_name_Danish[match(MONIS5eDNA04_df$speciesabbr, MONIS3.ls.assays03_df$abbr.nm)]
#match between data frames
MONIS5eDNA04_df$Lok_omr01 <-    MST_smpls01_df$Lok_omr01[match(MONIS5eDNA04_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA04_df$Dato_inds <-    MST_smpls01_df$Dato_inds[match(MONIS5eDNA04_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA04_df$Vwf_mL <-       MST_smpls01_df$Vwf_mL[match(MONIS5eDNA04_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA04_df$lok_pos_lat <-  MST_smpls01_df$lok_pos_lat[match(MONIS5eDNA04_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA04_df$lok_pos_lon <-  MST_smpls01_df$lok_pos_lon[match(MONIS5eDNA04_df$smpltp, MST_smpls01_df$U_Pr_Nr)]
# make variables numeric
MONIS5eDNA04_df$Quantitycopies <- as.numeric(as.character(MONIS5eDNA04_df$Quantitycopies))
MONIS5eDNA04_df$Vwf_mL <- as.numeric(as.character(MONIS5eDNA04_df$Vwf_mL))
# following this example: https://stackoverflow.com/questions/41336606/accessing-element-of-a-split-string-in-r
#split the collection date, and get the third element, to get the year
MONIS5eDNA04_df$year_inds <- MONIS5eDNA04_df$Dato_inds %>%
  strsplit( "/" ) %>%
  sapply( "[", 3 )
#paste a new column based on variables separated by point
MONIS5eDNA04_df$WellType.year_inds.1 <- paste(MONIS5eDNA04_df$WellType, MONIS5eDNA04_df$year_inds, "1",  sep=".")
#paste a new column based on variables separated by point
MONIS5eDNA04_df$gen_specnm.year_inds <- paste(MONIS5eDNA04_df$gen_specnm, MONIS5eDNA04_df$year_inds,  sep=".")
#paste a new column based on variables separated by point
MONIS5eDNA04_df$Lok_omr01.Welltype <- paste(MONIS5eDNA04_df$Lok_omr01, MONIS5eDNA04_df$WellType,  sep=".")
#get the unique smpl names for Harbours and WellTypes
unHaWT <- unique(MONIS5eDNA04_df$Lok_omr01.Welltype)
#transp_col <- as.character("#FFFFFF")
HaWTnoNA <- addNA(unHaWT)
col.01<-as.numeric(as.factor(HaWTnoNA))
#make a small dataframe w harbours and standards and numbers assigned, 
#use the col2hex in gplot pacakge to convert the 'red' color name to hex-color
col.02 <- col2hex(palette(rainbow(length(col.01)+1)))

lokomrcols <- cbind(unHaWT,col.01, col.02)
length(unHaWT)
length(col.01) 
length(col.02)
#replace the colour for the standard dilution sample type with the transparent colour
col.03<-replace(col.02, unHaWT=="NA.Standard", transp_col)
col.04 <- cbind(lokomrcols,col.03)
colforlokomr <- as.data.frame(col.04)
#match to main data frame and add as new color
MONIS5eDNA04_df$col.06 <- colforlokomr$col.03[match(MONIS5eDNA04_df$Lok_omr01.Welltype, colforlokomr$unHaWT)]
#insert the transparent color for all matches with "NA.Standard"
MONIS5eDNA04_df$col.06[MONIS5eDNA04_df$Lok_omr01.Welltype=="NA.Standard"] <- transp_col
# following this example: https://stackoverflow.com/questions/41336606/accessing-element-of-a-split-string-in-r
#split the collection date, and get the third element, to get the year
loq.lod.table$gen_spcnm <- loq.lod.table$spc.year %>%
  strsplit( "\\." ) %>%
  sapply( "[", 1 )
#match between the smpls03 dataframe and the LOD and LOQ table
MONIS5eDNA04_df$LOD <- loq.lod.table$LOD[match(MONIS5eDNA04_df$gen_specnm, loq.lod.table$gen_spcnm)]
MONIS5eDNA04_df$LOQ <- loq.lod.table$LOQ[match(MONIS5eDNA04_df$gen_specnm, loq.lod.table$gen_spcnm)]

#add column with copies per Liter of filtered water
#Ae = (Cqpcr /Fe) /Vwf. 
#’Ae’ number of  eDNA-copies per volumen filtered water, 
#’Cqpcr’ number of copies detected in the qPCR-well, #smpls02.1$meanQuantitycopies 
#’Fe’ the ratio of the eluted extrated filtrate used in a qPCR-well #5/350
# For the qPCR 5 uL of extracted eDNA from the filter sample was used
# For the extraction of eDNA from the filters the extracted eDNA was eluated in 350 uL
#’Vwf’ is volumen of seawater filtered.
rt <- 5/350
# get the template volume used for qPCR setups
MONIS5eDNA04_df$templvol2 <- as.numeric(substr(MONIS5eDNA04_df$templvol, 1, 1))
# relate the volume of template used in qPCR to recalculate the ratio of
#extracted eluate used
rt <- (MONIS5eDNA04_df$templvol2/350)
#per mL
MONIS5eDNA04_df$copies_per_mLwater <- (MONIS5eDNA04_df$Quantitycopies/(rt))/MONIS5eDNA04_df$Vwf_mL
#per Liter
MONIS5eDNA04_df$copies_per_Lwater <- MONIS5eDNA04_df$copies_per_mLwater*1000
#replace nas with zeros
MONIS5eDNA04_df$copies_per_Lwater[is.na(MONIS5eDNA04_df$copies_per_Lwater)]<-0
#add one to be able to do logarithmic scales
MONIS5eDNA04_df$copies_per_Lwater_plone<- MONIS5eDNA04_df$copies_per_Lwater+1
#take log10 to all copies
MONIS5eDNA04_df$log.10_copies_L <- log10(MONIS5eDNA04_df$copies_per_Lwater_plone)
#make a subset 
MONIS5eDNA05_df <- MONIS5eDNA04_df[ which(MONIS5eDNA04_df$WellType=="Unknown"), ]
# exclude the rows that have NAs for years
MONIS5eDNA06_df <- MONIS5eDNA05_df[which(!is.na(MONIS5eDNA05_df$year_inds)), ] 

######################################################################################################
# Get mean for each set of 3 technical qPCR replicates per species per season per harbour
######################################################################################################
#get the mean quantity for each species per season per port
MONIS5eDNA07_df <- aggregate(MONIS5eDNA06_df[, "Quantitycopies"], list(MONIS5eDNA06_df$gen_specnm.year_inds, MONIS5eDNA06_df$smpltp), mean)
#change the column names
colnames(MONIS5eDNA07_df) <- c("spc.year","MSTsmpl","meanQuantitycopies")
# following this example: https://stackoverflow.com/questions/41336606/accessing-element-of-a-split-string-in-r
#split the collection date, and get the third element, to get the year
MONIS5eDNA07_df$gen_spcnm <- MONIS5eDNA07_df$spc.year %>%
  strsplit( "\\." ) %>%
  sapply( "[", 1 )
#match with LOD and LOQ
MONIS5eDNA07_df$LOD <- loq.lod.table$LOD[match(MONIS5eDNA07_df$gen_spcnm, loq.lod.table$gen_spcnm)]
MONIS5eDNA07_df$LOQ <- loq.lod.table$LOQ[match(MONIS5eDNA07_df$gen_spcnm, loq.lod.table$gen_spcnm)]
#add an empty column with just NAs
MONIS5eDNA07_df[,"eDNA_eval_mean"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
MONIS5eDNA07_df$eDNA_eval_mean[MONIS5eDNA07_df$meanQuantitycopies>MONIS5eDNA07_df$LOQ] <- "aboveLOQ"
MONIS5eDNA07_df$eDNA_eval_mean[MONIS5eDNA07_df$meanQuantitycopies<MONIS5eDNA07_df$LOQ] <- "AbLOD_BeLOQ"
MONIS5eDNA07_df$eDNA_eval_mean[MONIS5eDNA07_df$meanQuantitycopies<MONIS5eDNA07_df$LOD & !MONIS5eDNA07_df$meanQuantitycopies==0] <- "belowLOD"
MONIS5eDNA07_df$eDNA_eval_mean[MONIS5eDNA07_df$meanQuantitycopies==0] <- "NoCt"

#add an empty column with just NAs to fil with color codings
MONIS5eDNA07_df[,"eDNA_col_eval_mean"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
MONIS5eDNA07_df$eDNA_col_eval_mean[MONIS5eDNA07_df$meanQuantitycopies>MONIS5eDNA07_df$LOQ] <- "black" # "green"
MONIS5eDNA07_df$eDNA_col_eval_mean[MONIS5eDNA07_df$meanQuantitycopies<MONIS5eDNA07_df$LOQ] <- "red" #"orange" 
MONIS5eDNA07_df$eDNA_col_eval_mean[MONIS5eDNA07_df$meanQuantitycopies<MONIS5eDNA07_df$LOD & !MONIS5eDNA07_df$meanQuantitycopies==0] <-"yellow" #"yellow"
MONIS5eDNA07_df$eDNA_col_eval_mean[MONIS5eDNA07_df$meanQuantitycopies==0] <- "white" #,"red"
#reorder the dataframe, to make it easier to look at
MONIS5eDNA08_df <- MONIS5eDNA07_df[ order(MONIS5eDNA07_df$spc.year, MONIS5eDNA07_df$MSTsmpl), ]
# following this example: https://stackoverflow.com/questions/41336606/accessing-element-of-a-split-string-in-r
#split the collection date, and get the third element, to get the year
MONIS5eDNA08_df$year.inds <- MONIS5eDNA08_df$spc.year %>%
  strsplit( "\\." ) %>%
  sapply( "[", 2 )
#head(MONIS5eDNA08_df,4)
#match between data frames
MONIS5eDNA08_df$Lok_omr01 <-    MST_smpls01_df$Lok_omr01[match(MONIS5eDNA08_df$MSTsmpl, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA08_df$Dato_inds <-    MST_smpls01_df$Dato_inds[match(MONIS5eDNA08_df$MSTsmpl, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA08_df$Vwf_mL <-       MST_smpls01_df$Vwf_mL[match(MONIS5eDNA08_df$MSTsmpl, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA08_df$lok_pos_lat <-  MST_smpls01_df$lok_pos_lat[match(MONIS5eDNA08_df$MSTsmpl, MST_smpls01_df$U_Pr_Nr)]
MONIS5eDNA08_df$lok_pos_lon <-  MST_smpls01_df$lok_pos_lon[match(MONIS5eDNA08_df$MSTsmpl, MST_smpls01_df$U_Pr_Nr)]

#get unique rows for more than one variable
tmp.vol <- unique(MONIS5eDNA06_df[c("gen_specnm", "templvol2")])
colnames(MONIS5eDNA06_df)
#get unique rows for more than one variable
smpltp.Vwf_mL <- unique(MONIS5eDNA06_df[c("smpltp", "Vwf_mL")])
# add template volume column to data frame
MONIS5eDNA08_df$templvol2 <-  tmp.vol$templvol2[match(MONIS5eDNA08_df$gen_spcnm, tmp.vol$gen_specnm)]
MONIS5eDNA08_df$Vwf_mL <-  smpltp.Vwf_mL$Vwf_mL[match(MONIS5eDNA08_df$MSTsmpl, smpltp.Vwf_mL$smpltp)]
#Ae = (Cqpcr /Fe) /Vwf. 
#’Ae’ number of  eDNA-copies per volumen filtered water, 
#’Cqpcr’ number of copies detected in the qPCR-well, #smpls20$meanQuantitycopies 
#’Fe’ the ratio of the eluted extrated filtrate used in a qPCR-well #5/350
#’Vwf’ is volumen of seawater filtered. #smpls20$volfilt_mL
# get the template volume used for qPCR setups
# relate the volume of template used in qPCR to recalculate the ratio of
#extracted eluate used
rt <- (MONIS5eDNA08_df$templvol2/350)
#per mL
MONIS5eDNA08_df$copies_per_mLwater <- (MONIS5eDNA08_df$meanQuantitycopies/(5/350))/MONIS5eDNA08_df$Vwf_mL
#per Liter
MONIS5eDNA08_df$copies_per_Lwater <- MONIS5eDNA08_df$copies_per_mLwater*1000
#replace nas with zeros
MONIS5eDNA08_df$copies_per_Lwater[is.na(MONIS5eDNA08_df$copies_per_Lwater)]<-0
#add one to be able to do logarithmic scales
MONIS5eDNA08_df$copies_per_Lwater_plone<- MONIS5eDNA08_df$copies_per_Lwater+1
#take log10 to all copies
MONIS5eDNA08_df$log.10_copies_L <- log10(MONIS5eDNA08_df$copies_per_Lwater_plone)

#add an empty column with just NAs to fil with evaluations
MONIS5eDNA08_df[,"no_for_log.10_eDNAlvls"] <- NA

#replace in the empty column, the order is important, 
#as you otherwise will end up with the last evaluations
MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L==0 ] <- 1 #if No Ct

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L<=
                                 log10((MONIS5eDNA08_df$LOD/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) 
                               & MONIS5eDNA08_df$log.10_copies_L>0] <- 2 #if below LOD but above zero

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOD/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) ] <- 3 #if above LOD

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) ] <- 4 #if above LOQ

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=1] <- 5 #if above LOQ, and within 1-10 copies per L

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=10] <- 6 #if above LOQ, and within 10-100 copies per L

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=100] <- 7 #if above LOQ, and within 100-1000 copies per L

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=1000] <- 8 #if above LOQ, and within 1e3-1e4 copies per L

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=1E4] <- 9 #if above LOQ, and within 1e4-1e5 copies per L

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=1E5] <- 10 #if above LOQ, and within 1e5-1e6 copies per L

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=1E6] <- 11 #if above LOQ, and within 1e6-1e7 copies per L

MONIS5eDNA08_df$no_for_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                                 log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                   MONIS5eDNA08_df$copies_per_Lwater>=1E7] <- 12 #if above LOQ, and within 1e7-1e8 copies per L

unique(MONIS5eDNA08_df$no_for_log.10_eDNAlvls)

#add an empty column with just NAs to fil with evaluations
MONIS5eDNA08_df[,"col_log.10_eDNAlvls"] <- NA

#replace in the empty column, the order is important, 
#as you otherwise will end up with the last evaluations
MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L==0 ] <- 0 #if No Ct

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L<=
                              log10((MONIS5eDNA08_df$LOD/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) 
                            & MONIS5eDNA08_df$log.10_copies_L>0] <- 0 #if below LOD but above zero

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOD/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) ] <- 0 #if above LOD

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) ] <- 1 #if above LOQ

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=1] <- 2 #if above LOQ, and within 1-10 copies per

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=10] <- 3 #if above LOQ, and within 10-100 copies per L

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=100] <- 4 #if above LOQ, and within 100-1000 copies per L

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=1000] <- 5 #if above LOQ, and within 1e3-1e4 copies per L

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=1E4] <- 6 #if above LOQ, and within 1e4-1e5 copies per L

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=1E5] <- 7 #if above LOQ, and within 1e5-1e6 copies per L

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=1E6] <- 8 #if above LOQ, and within 1e6-1e7 copies per L

MONIS5eDNA08_df$col_log.10_eDNAlvls[MONIS5eDNA08_df$log.10_copies_L>=
                              log10((MONIS5eDNA08_df$LOQ/(rt))/MONIS5eDNA08_df$Vwf_mL*1000) &
                                MONIS5eDNA08_df$copies_per_Lwater>=1E7] <- 9 #if above LOQ, and within 1e7-1e8 copies per L

#check unique values
maxcolval <- max(unique(MONIS5eDNA08_df$col_log.10_eDNAlvls))










## draw a map of the world, but limit the x-axis to between 8 to 14 ° E lon 
## and the y-axis to between 54 and 58 N° lat , 
## colour the land and sea in hexadecimal colors
## import bathymetric data for the region, 
## with lon1 and lon2 specifying the western end eastern 
## boundaries of the bathymetric plot 
## set the resolution to be 2, a higher resulotion takes longer to download
# All maps are to be based on the same bathymetric map , so this can be reused
dk.sea <- getNOAA.bathy(lon1 = 6, lon2 = 14,
                        lat1 = 54, lat2 = 58, resolution = 2)
# All maps are to be based on the same bathymetric map , so this can be reused
########################################################################################
#
# prepare maps with eDNA categories mapped on harbours
# notice the section in the middle, that allows for plotting bars on the harbours
# to reflect the intensity of the eDNA levels. This middle section is currently
# commented out!
#
########################################################################################
#first get the species names, and Assay No's and assign numbers to each, to use 
# for numbering appendices
#get the unique species names
latspecnm <- unique(MONIS5eDNA08_df$gen_spcnm)
#match the assay number to the data frame with species
AIfps <- MONIS3.ls.assays03_df$Assay_ID[match(latspecnm, MONIS3.ls.assays03_df$Lat_Species)]
#make a new data frame with assay Id No and species
nlspnm <- data.frame(AIfps,latspecnm)
#reorder by the column 'AssayIDNo'
nlspnm<- nlspnm[order(nlspnm$AIfps),]
#make a list of numbers for the unique species
no.latspc <- seq(1:length(latspecnm))
#add a new column with no to use for appendix numbering
nlspnm <- cbind(nlspnm, no.latspc) 
#use the new order of latin species names for producing plots
latspecnm <- unique(nlspnm$latspecnm)
#remove NA from the factors
#see this webpage: https://www.r-bloggers.com/r-drop-factor-levels-in-a-dataset/
latspecnm <- latspecnm[latspecnm!="NA"]
# loop over all species names in the unique list of species, and make plots. 
#Notice that the curly bracket ends after the pdf file is closed
latspecnm <- "Mnemiopsis leidyi"
spec.lat <- "Mnemiopsis leidyi"
for (spec.lat in latspecnm){
  print(spec.lat)
  }
#get the Danish commom name
sbs.dk.nm <- MONIS3.ls.assays03_df$Common_name_Danish[match(spec.lat, MONIS3.ls.assays03_df$Lat_Species)]
#make the species name in italics
#ital.spec.lat <- bquote(''~italic(.(spec.lat)))
#get the AssayIDNo
sbs.AssIDNo <- MONIS3.ls.assays03_df$Assay_ID[match(spec.lat, MONIS3.ls.assays03_df$Lat_Species)]
#get the number for the appendix plot number
no.spc.app.plot <- nlspnm$no.latspc[match(spec.lat, nlspnm$latspecnm)]
#subset based on variable values, subset by species name
sbs.MONIS5eDNA08_df <- MONIS5eDNA08_df[ which(MONIS5eDNA08_df$gen_spcnm==spec.lat), ]
#colnames(MONIS5eDNA02_df)
#count using the plyr-package - see: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-r/
sbs.tot_smpl <- count(sbs.MONIS5eDNA08_df, c("Lok_omr01", "lok_pos_lon", "lok_pos_lat","year.inds"))
#head(sbs.MONIS5eDNA08_df,6)

#add an empty column with just NAs to fil with color codings
MONIS5eDNA08_df[,"eDNA_eval_t_repl_col"] <- NA
#replace in the empty column, the order is important, as you otherwise will end up with the last evaluations
MONIS5eDNA08_df$eDNA_eval_t_repl_col[MONIS5eDNA08_df$eDNA_eval_p_repl_col==1] <- "white" #"white" #"NoCt" #0  
MONIS5eDNA08_df$eDNA_eval_t_repl_col[MONIS5eDNA08_df$eDNA_eval_p_repl_col==2] <- "yellow" #"yellow" #"belowLOD" #1  
MONIS5eDNA08_df$eDNA_eval_t_repl_col[MONIS5eDNA08_df$eDNA_eval_p_repl_col==3] <- "orange" # "azure3" #"AbLOD_BeLOQ" #2 
MONIS5eDNA08_df$eDNA_eval_t_repl_col[MONIS5eDNA08_df$eDNA_eval_p_repl_col==4] <- "red" #"azure4" #"one aboveLOQ" #3 
MONIS5eDNA08_df$eDNA_eval_t_repl_col[MONIS5eDNA08_df$eDNA_eval_p_repl_col==5] <- "black" #"black" #"all 3 above LOQ" #4




















##########################################################################################
#
# Make a table that looks somewhat similar to Table 5 presented by :
#  Li, J, Hatton‐Ellis, TW, Lawson Handley, L‐J, et al. Ground‐truthing of a fish‐based environmental DNA metabarcoding method for assessing the quality of lakes. J Appl Ecol. 2019; 56: 1232– 1244. https://doi.org/10.1111/1365-2664.13352 
#
##########################################################################################

################################################################################################
# 01 - make a table that compares conventional monitoring and eDNA levels - start
################################################################################################
#delimit one dataframe for the season, to only include species and harbour
#keep only selected columns for coloring
keeps <- c("spc.season", "Harbour")
smpls26 <- smpls12foraar[keeps]
#split by point, bind as a dataframe
dat2 <- data.frame(do.call(rbind, strsplit(as.vector(smpls26$spc.season), split = "[.]")))
#append back to data frame
smpls26$spc <- dat2$X1
#delete column not needed
smpls26$spc.season <- NULL
# get the greatest value of the two columns for spring and autumn
#https://stackoverflow.com/questions/28531809/i-want-to-select-the-greater-of-the-two-values-from-two-columns-in-r
#use the 'pmax' function
smpls26$freq_NoCt <- pmax(smpls12foraar$freq_NoCt, smpls12efteraar$freq_NoCt)
smpls26$freq_belowLOD <- pmax(smpls12foraar$freq_belowLOD, smpls12efteraar$freq_belowLOD)
smpls26$freq_AbLOD_BeLOQ <- pmax(smpls12foraar$freq_AbLOD_BeLOQ, smpls12efteraar$freq_AbLOD_BeLOQ)
smpls26$freq_aboveLOQ <- pmax(smpls12foraar$freq_aboveLOQ, smpls12efteraar$freq_aboveLOQ)
smpls26$eDNA_eval_p_repl_col <- pmax(smpls12foraar$eDNA_eval_p_repl_col, smpls12efteraar$eDNA_eval_p_repl_col)
# paste two columns together
smpls26$spc.Harbour <- as.character(paste(smpls26$spc, smpls26$Harbour, sep="."))
#check unique elements
l1 <- unique(smpls26$Harbour)
l2 <- unique(srtsurv02$Harbour)
#check if the lists are identical
identical(l1,l2)
#check unique elements
l1 <- sort(as.character(unique(smpls26$spc)))
l2 <- sort(as.character(unique(srtsurv02$Species_name)))
#remove the two extra Acipenser species from the list
l3 <- l2[l2 != "Acipenser_gueldenstaedtii"]; # without elements that are "Acipenser_gueldenstaedtii"
l4 <- l3[l3 != "Acipenser_ruthenus"];
#check if the lists are identical
identical(l1,l2)
identical(l1,l4)
#match the species and harbour name and get the conventional record
smpls26$conv.rec <- srtsurv02$conv_rec_val[match(smpls26$spc.Harbour,srtsurv02$Species_name.Harbour)]
#head(smpls26)
#get the column number 
which( colnames(smpls26)=="freq_belowLOD" )
which( colnames(smpls26)=="freq_aboveLOQ" )
#sum the rows in the selected columns
smpls26$sum.ab.LOD <- rowSums(smpls26[4:6])
#evaluate as presence-absence if eDNA level is higher than LOD in just a single case
smpls26$eDNA.pr.ab <- 1*(smpls26$sum.ab.LOD>=1)
#evaluate as presence-absence if the conventional recording found the species
smpls26$conv.rec.pr.ab <- 1*(smpls26$conv.rec>=1)
#paste together
smpls26$eDNA.conv.rec <- as.character(paste(smpls26$eDNA_eval_p_repl_col, smpls26$conv.rec, sep="."))
#add an empty column with just NAs to fil with  codings
smpls26[,"eDNA.eval.code"] <- NA
smpls26[,"eDNA.eval.code2"] <- NA
#assign letter code values to eDNA level scores
smpls26$eDNA.eval.code[smpls26$freq_NoCt>=0] <- "NoCq" #"white" #"NoCt" #0  
smpls26$eDNA.eval.code2[smpls26$freq_NoCt>=0] <- "NC" #"white" #"NoCt" #0  
smpls26$eDNA.eval.code[smpls26$freq_belowLOD>=1] <- "beLOD" #"yellow" #"belowLOD" #1  
smpls26$eDNA.eval.code2[smpls26$freq_belowLOD>=1] <- "bL" #"yellow" #"belowLOD" #1  
smpls26$eDNA.eval.code[smpls26$freq_AbLOD_BeLOQ>=1] <- "abLODbeLOQ" # "azure3" #"AbLOD_BeLOQ" #2 
smpls26$eDNA.eval.code2[smpls26$freq_AbLOD_BeLOQ>=1] <- "aLbL" # "azure3" #"AbLOD_BeLOQ" #2 
smpls26$eDNA.eval.code[smpls26$freq_aboveLOQ>=1] <- "1abLOQ" #"azure4" #"one aboveLOQ" #3 
smpls26$eDNA.eval.code2[smpls26$freq_aboveLOQ>=1] <- "1aL" #"azure4" #"one aboveLOQ" #3 
smpls26$eDNA.eval.code[smpls26$freq_aboveLOQ>=3] <- "3abLOQ" #"black" #"all 3 above LOQ" #4
smpls26$eDNA.eval.code2[smpls26$freq_aboveLOQ>=3] <- "3aL" #"black" #"all 3 above LOQ" #4
#add an empty column with just NAs to fil with  codings
smpls26[,"conv.rec.eval.code"] <- NA
smpls26[,"conv.rec.eval.code2"] <- NA
#assign letter code values to conventional record level scores
smpls26$conv.rec.eval.code[smpls26$conv.rec>=0] <- "nevrec" #"white" #"Never recorded" #0  
smpls26$conv.rec.eval.code2[smpls26$conv.rec>=0] <- "NR" #"white" #"Never recorded" #0  
smpls26$conv.rec.eval.code[smpls26$conv.rec>=1] <- "prevrec" #"red" #"previously recorded" #1  
smpls26$conv.rec.eval.code2[smpls26$conv.rec>=1] <- "PR" #"red" #"previously recorded" #1  
smpls26$conv.rec.eval.code[smpls26$conv.rec>=2] <- "survrec" #"black" #"recorded in survey" #2  
smpls26$conv.rec.eval.code2[smpls26$conv.rec>=2] <- "SR" #"black" #"recorded in survey" #2  
#paste together
smpls26$eDNA.conv.rec2 <- as.character(paste(smpls26$eDNA.eval.code, smpls26$conv.rec.eval.code, sep="_"))
smpls26$eDNA.conv.rec3 <- as.character(paste(smpls26$eDNA.eval.code2, smpls26$conv.rec.eval.code2, sep="_"))
#keep only selected columns
keeps <- c("spc.Harbour",
           "eDNA.conv.rec3")
#put the kept columns into a new dataframe
smpls27 <- smpls26[keeps]
#split by point, bind as a dataframe
dat3 <- data.frame(do.call(rbind, strsplit(as.vector(smpls27$spc.Harbour), split = "[.]")))
#append back to data frame
smpls27$spc <- dat3$X1
smpls27$Harbour <- dat3$X2
smpls27$spc.Harbour <- NULL
#replace underscore in species name
#smpls27$spc <- gsub("_", " ", smpls27$spc)
#use substring to shorten species names
dat4 <- data.frame(do.call(rbind, strsplit(as.vector(smpls27$spc), split = "[_]")))
genusnm <- dat4$X1
spcnm <- dat4$X2
smpls27$spc <- paste((substr(dat4$X1, 1, 1)),". ",spcnm,sep="")
#head(smpls27)
unique(smpls27$Harbour)

#make a data frame with three letter code abbreviations for ports
port00 <- c("harbour","L3Code")
port01 <- c("AalborgHavn","ALH")
port02 <- c("Aalborgportland","ALP")
port03 <- c("Aarhus","ARH")
port04 <- c("Esbjerg","ESB")
port05 <- c("Fredericia","FRC")
port06 <- c("Frederikshavn","FRH")
port07 <- c("Gedser","GED")
port08 <- c("Grenaa","GRE")
port09 <- c("Helsingoer","HEL")
port10 <- c("Hirtshals","HIR")
port11 <- c("Kalundborg","KAB")
port12 <- c("KalundborgStatiolHavn","KSH")
port13 <- c("Koebenhavn","KBH")
port14 <- c("Koege","KGE")
port15 <- c("Odense","ODE")
port16 <- c("Roedby","ROD")
#bind columns and transpose
port <- t(cbind(port00,port01,port02,port03,port04,port05,
                port06,port07,port08,port09,port10,
                port11,port12,port13, port14, port15, port16))
#assign col names based on first row
colnames(port) = port[1, ]
#remove the first row
port = port[-1, ] 
#turn into data frame
ports <- as.data.frame(port)
#match back to data frame
smpls27$Harbour.3L <- ports$L3Code[match(smpls27$Harbour,ports$harbour)]
smpls27$Harbour <- smpls27$Harbour.3L
smpls27$Harbour.3L <- NULL
#colnames(smpls27)
#reshape data from long to wide format
smpls28 <- reshape(smpls27, idvar = "spc", timevar = "Harbour", direction = "wide")
#R: substitute multiple column names in a data frame and keep their numerical value: see: https://stackoverflow.com/questions/45535505/r-substitute-multiple-column-names-in-a-data-frame-and-keep-their-numerical-val
#get the numeber of columns in the dataframe
l.s.28 <- length(smpls28)
#get the column names in the data frame and put in a object 
nameVec <- names(smpls28)
#replace in the column names in the previous prepared object 
nameVec[2:l.s.28] <- gsub("eDNA.conv.rec3.", "", nameVec[2:l.s.28])
nameVec[1] <- gsub("spc", "species", nameVec[1])
#use the object with new column names to replace the column namwes
names(smpls28) <- nameVec

#install.packages("tableHTML")
#https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
if(!require(tableHTML)){
  install.packages("tableHTML")
  library(tableHTML)
}
require(tableHTML)
#try the tableHTML with no border
tableHTML <- smpls28 %>% 
  tableHTML(border = 0) 
#count the number of columns in the dataframe
l.s.28 <- length(smpls28)
#get unique cell values in dataframe : see : http://r.789695.n4.nabble.com/Retrieve-distinct-values-within-a-whole-data-frame-td1460205.html
#apart from the first column
unique(unlist(smpls28[2:l.s.28]))
#make lists of the words in the cells to color using the 'add_css_conditional_column' function
#agree1 # both neg or both positive #white
#agr01 <- c("NoCq_nevrec", "1abLOQ_survrec" , "abLODbeLOQ_survrec" ,"3abLOQ_survrec")
agr01 <- c("NC_NR", "1aL_SR" , "aLbL_SR" ,"3aL_SR")
#disagree2 # eDNA pos, conv neg #red "#FF6666"
#dgr02 <- c("abLODbeLOQ_nevrec", "1abLOQ_nevrec" ,"3abLOQ_nevrec")
dgr02 <- c("aLbL_NR", "1aL_NR" ,"3aL_NR")
#disagree3 # eDNA neg, conv pos #blue "#3333FF"
#dgr03 <- c("NoCq_survrec")
dgr03 <- c("NC_SR")
#disagree4 # eDNA pos, conv almost neg #red-light "#FF9999"
#dgr04 <- c("3abLOQ_prevrec", "abLODbeLOQ_prevrec", "1abLOQ_prevrec")
dgr04 <- c("3aL_PR", "aLbL_PR", "1aL_PR")
#disagree5 # eDNA almost neg, conv neg #red-light "#FF9999"
#dgr05 <- c("beLOD_nevrec")
dgr05 <- c("bL_NR")
#disagree6 # eDNA almost neg, conv neg #blue-light "#99CCFF"
#dgr06 <- c("beLOD_prevrec", "beLOD_survrec", "NoCq_prevrec")
dgr06 <- c("bL_PR", "bL_SR", "NC_PR")

tableHTML <- smpls28 %>% 
  tableHTML()
#tableHTML(smpls28, border = 0)
# for dgr02 "#FF6666"
words <- dgr02
col.f.cell <- "#FF6666"
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 2:l.s.28, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}
# for dgr03 "#3333FF"
words <- dgr03
col.f.cell <- "#3333FF"
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 2:l.s.28, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}
# for dgr04 "#FF9999"
words <- dgr04
col.f.cell <- "#FF9999"
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 2:l.s.28, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}
# for dgr05 "#FF9999"
words <- dgr05
col.f.cell <- "#FF9999"
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 2:l.s.28, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}
# for dgr06 "#99CCFF"
words <- dgr06
col.f.cell <- "#99CCFF"
for (word in words) {
  tableHTML <- tableHTML %>% 
    add_css_conditional_column(columns = 2:l.s.28, #make it work on column 2 to the last column
                               conditional = "contains",
                               value = word,
                               css = list(c("background-color"),
                                          c(col.f.cell)))
}
tableHTML
t.HTML29 <- tableHTML


################################################################################################
# 01 - make a table that compares conventional monitoring and eDNA levels - end
################################################################################################

################################################################################################














maxcol.log10.val.eDNA <- max(sbs.MONIS5eDNA08_df$col_log.10_eDNAlvls)
#rearrange the data fram using the reshape function
sbs.smpls21<-reshape(sbs.smpls20,timevar="season",idvar="Harbour",direction="wide")

#subset based on variable values
# subset among the seasons
sbs.smpls20.for <- sbs.smpls20[ which(sbs.smpls20$season=="foraar" ), ]
sbs.smpls20.eft <- sbs.smpls20[ which(sbs.smpls20$season=="efteraar" ), ]

#XXXXX______begin plot w squares  on map ________XXXX






#