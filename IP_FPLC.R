func.FPLC <- function(initials = "AL"){
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  if (!require("lubridate")) {
    install.packages("lubridate", dependencies = TRUE); library(lubridate)}
  if (!require("SpadeR")) {
    install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  
  #Initial setup - choosing 
  
  path.file <- file.choose()
  directoryextractor.func(path.file)
  FPLC <- read.xlsx(xlsxFile = path.file)
  FPLC$Dupe <- ""
  
  
  #I'll need to set it up to look at headers to figure out what is what is what
  FPLC$Collection.Date <- as.Date(FPLC$Collection.Date, origin = "1900-01-01")-2
  
  #fold different dates into new sheets, export
  #########check for dupes
  i <- 1
  ##yeah this needs to be redone for dup, maybe add another field for dup comments?
   ID <- FPLC$IdentiGEN.Sample.ID
   EID <- FPLC$EID
   ID.list <- ID[ID %in% unique(ID[duplicated(ID)])]
   EID.list <- EID[EID %in% unique(EID[duplicated(EID)])]
   ID.dupes <- unique(ID.list)
   EID.dupes <- unique(EID.list)
   if(length(ID.dupes)>0) {
     for(i in seq_along(ID.dupes)){
       FPLC[FPLC$IdentiGEN.Sample.ID==ID.dupes[i],8] <- "ID dupe"
     }else{
      for(i in seq_along(EID.dupes)){
        FPLC[FPLC$IdentiGEN.Sample.EID==EID.dupes[i],8] <- "EID dupe"
    }
    
    
  #####!i should be making it look for the date column instead of hardcoding it FOR SURE
  
  dates <- unique(FPLC$Collection.Date)
  depth<- length(dates)
  for(i in 1:depth){
    temp <- FPLC[FPLC$Collection.Date==dates[i],]
    version <- "V_1.0"
    write.csv(temp, file = paste("FPLC killsheet for", dates[i], version, initials, as.character((Sys.Date())), ".csv", sep = " "))
  }
}
