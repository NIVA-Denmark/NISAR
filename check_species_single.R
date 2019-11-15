# http://marineregions.org/mrgid/2401


df <- GetSpeciesInfo("Crassostrea gigas")

searchtext <- "Nymphulinae indet."
searchtext <- "Nymphulinae"
searchtext <- "Asperococcus turneri"
searchtext <- "Crassostrea"
searchtext <-"Mesopodopsis slabberi"
searchtext <- "Crassostrea gigas"
searchtext <-"Trichocerca marina"
searchtext <-"Abietinaria abietina"
searchtext <-"Acanthodoris pilosa"
searchtext <-"Acmaea testudinalis"
searchtext <-"Acrochaete heteroclada"
searchtext <-"Harrimania kupfferi"
searchtext <-"Angulus"
searchtext <-"Acrochaetiaceae"

searchtext <-"Bonnemaisonia / Spermothamnion"

searchtext <-"Abranchus microstomus"
searchtext <-"Abranchus"

df <- GetSpeciesInfo(searchtext)
df <- GetSpeciesID(searchtext)


searchtext <- "Crassostrea gigas"
id <- GetSpeciesID(searchtext)
AphiaID<-id$AphiaID

df <- GetSpeciesInfo(id$AphiaID)
