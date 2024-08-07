func.FPLC <- function(initials = "DT"){
  version <- "V 1.0"
    #1.0: 2020-03-10
      #base functionality. Duplicate flagging in new column, dates to different files. 
      #the date thing is really weird and i hope I can fix it somehow.
  
#Library installation/loading
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  if (!require("lubridate")) {
    install.packages("lubridate", dependencies = TRUE); library(lubridate)}
#  if (!require("SpadeR")) {
#    install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
#  if (!require("gdata")) {
#    install.packages("gdata", dependencies = TRUE); library(gdata)}
  
#functions
  #this is my favorite because it setwd() to directory of selected file
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  #this one isn't really necessary, i just hate reusing the same overly complicated expression every time
  date.func <- function(value){
    as.Date(value, origin = "1900-01-01")-2
  }
  
#guts and actual execution
  #Initial setup - choosing 
  path.file <- file.choose()
  directoryextractor.func(path.file)
  FPLC <- read.xlsx(xlsxFile = path.file)
  FPLC$Dupe <- "-"
  FPLC.raw <- FPLC
  
  #I'll need to set it up to look at headers to figure out what is what is what
  FPLC$Collection.Date <- date.func(FPLC$Collection.Date)
  

  #dup checking/flagging
  ID <- FPLC$IdentiGEN.Sample.ID
  EID <- FPLC$EID
  ID.list <- ID[ID %in% unique(ID[duplicated(ID)])]
  EID.list <- EID[EID %in% unique(EID[duplicated(EID)])]
  ID.dupes <- unique(ID.list)
  EID.dupes <- unique(EID.list)
  #this is so I can eventually hard code this and just move things around. Or i'm lazy. either/or.
  dupe.list <- c("ID dupe", "EID dupe")
   if(length(ID.dupes)>0) {
     for(i in seq_along(ID.dupes)){
       FPLC[FPLC$IdentiGEN.Sample.ID==ID.dupes[i],8] <- dupe.list[1]
      }
     }else{
      for(i in seq_along(EID.dupes)){
        FPLC[FPLC$IdentiGEN.Sample.EID==EID.dupes[i],8] <- dupe.list[2]
      }
     }
    
    
#splitting file into sheets for each date, printing num.dupe if dupe
 dates <- unique(FPLC$Collection.Date)
 depth<- length(dates)
  for(i in 1:depth){
    temp <- FPLC[FPLC$Collection.Date==dates[i],]
    length.dupe <- dim(temp[temp$Dupe != "-",])[1]
    print(paste(dates[i], "has", length.dupe, "duplicates"), sep = " ")
    write.csv(temp, file = paste("FPLC killsheet for", dates[i], version, initials, as.character((Sys.Date())), ".csv", sep = " "))
    print("DOUBLE CHECK DATES MATCH")
  }
}

