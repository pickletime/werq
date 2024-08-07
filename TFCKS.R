func.TFCKS <- function(initials = "DT"){
###Library loading or install
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  if (!require("formattable")) {
    install.packages("formattable", dependencies = TRUE); library(scales)}
  if (!require("data.table")) {
    install.packages("data.table", dependencies = TRUE); library(scales)}

###version history:
    version = "V.1.6"
    #1.6: 2023-06-20: we were getting an additional brand column, so I'm taking it out IF it appears
    #1.5: 2023-01-04: the blanks were fucked, so i forced as int, then forced NA to zeroes. hopefully it works now?
    #1.4: 2022-12-16: the blank BC that were in there for some reason were complicating things, so I overwrote those values with "0" so sort will work.
    #1.3: 2022-06-21: They added in a column which FUCKED everything, so i'm adding that into the import so it doesn't bork. thanks I hate it.
    #1.2: 2021-10-14: idkwtf happened but it decided to include a couple dozen completely empty rows, which ruined sorting.
      #thanks i hate it
    #1.1: 2021-06-03: corrected a misc sample issue.
      #idkwtf is was doing before but it was wrong:
        #rbind(temp.b, temp.d[temp.d$BREAKDATE==unique.breakdate[2]])
        #became:
        #rbind(temp.b, temp.d[temp.d$BREAKDATE==unique.breakdate[2],])
    #1.0: 2021-03-06: written up and ready2roll. 
      #The only wonky thing is the date function(because fuck consistency amirite?)

  
  
    
###functions
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  date.func <- function(value){
    as.Date(value, origin = "1900-01-01")-2
  } #the '-2' thing is fucked and i don't really understand why it works sometimes and not others. Keep an eye on this?
  
  
  setwd("L:/Sample Records/Performance Food Group/Kill Sheets/Tyson Finney County")
  path.file <- file.choose()
  directoryextractor.func(path.file)
  temp <- read.xlsx(path.file)
  #idkwtf IN THIS ONE CASE breaktime is *24 but WHATEVER IDFC
  temp$BREAKTIME <- temp$BREAKTIME*24
  #these come through as pseudo-julian dates, to deal with them we use date func, also will always have to be as.numeric()
  unique.breakdate <- sort(unique(temp$BREAKDATE), decreasing = F)
  if(colnames(temp)[11] == "TYSONBRAND"){
    temp <- temp[,-11]
    print("TYSONBRAND column was present (and subsequently removed), doublecheck output files")
  }

  
#This was added in because it was handling blank BC poorly, so I made them zeroes, then forced the BC col as int so it sorted right
  #this was still bugging out, so i reordered it:
    #force BC as integer, if is.na, then bc = 0. 
  temp$BC <- as.integer(temp$BC)
  temp[is.na(temp$BC), 12] <- 0
  #if i flip the order it looks like it'll work?

  
  
#separating samples from each date set into respective frames. 
  #this had to be tweaked because it was transferring a dozen or so COMPLETELY empty rows that fucked up everything
  temp.a <- temp[temp$DNA.DATE==unique.breakdate[1] & !is.na(temp$BC),]
  temp.b <- temp[temp$DNA.DATE==unique.breakdate[2] & !is.na(temp$BC),]

###I don't really understand why these are necessary. 
  #It won't transfer a subset of samples, so this is a SUPER over-engineered workaround
  temp.c <- rbind(temp.a, temp.b)
  temp.d <- temp[!temp$CN %in% temp.c$CN,]
  if(nrow(temp.d) > 0){
      temp.a <- rbind(temp.d[temp.d$BREAKDATE==unique.breakdate[1],], temp.a)
      temp.b <- rbind(temp.d[temp.d$BREAKDATE==unique.breakdate[2],], temp.b)

  }else{
     temp.a <- temp.a[order(temp.a$BC,decreasing = F),]
     temp.b <- temp.b[order(temp.b$BC,decreasing = F),]
  } #There's an }else{ commented out. I'm pretty sure it's completely unnecessary, but i'm scared to delete it. 
      #If this starts bugging out, maybe try bringing that back in
  
  
##I'm pretty sure I could do this more intelligently, but idgaf because it works and i need a win
   temp.a <- temp.a[order(temp.a$BC,decreasing = F),]
   temp.b <- temp.b[order(temp.b$BC,decreasing = F),]
  
  
#Rewriting the fields in temp.a/b that are KNOWN - harvestdate and breakdate are solidly defined.
  temp.a$HARVESTDATE <- as.character(date.func(temp.a$HARVESTDATE))
  temp.a[,c(13,18)] <- as.character(date.func(unique.breakdate[1]))
  temp.a$BREAKTIME <- as.ITime(as.POSIXlt('2020-04-20 00:00:00 CST') + 3600*temp.a$BREAKTIME)
  
  temp.b$HARVESTDATE <- as.character(date.func(temp.b$HARVESTDATE))
  temp.b[,c(13,18)] <- as.character(date.func(unique.breakdate[2]))
  temp.b$BREAKTIME <- as.ITime(as.POSIXlt('2020-04-20 00:00:00 CST') + 3600*temp.b$BREAKTIME)
  
#I'm choosing to do nothing with the breakdate field, which will end up with some holes
  
  
  #file writing. 
  write.table(temp.a, file = paste("TFCKS KD", date.func(as.numeric(unique.breakdate[1])), initials, version, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
  write.table(temp.b, file = paste("TFCKS KD", date.func(as.numeric(unique.breakdate[2])), initials, version, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
  
  print("files output in relevant directory")
  
}