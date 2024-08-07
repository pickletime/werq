func.TWLKS <- function(initials = "DT"){
	options(repos = c(CRAN = "https://cran.rstudio.com"))
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  if (!require("formattable")) {
    install.packages("formattable", dependencies = TRUE); library(scales)}
  if (!require("data.table")) {
    install.packages("data.table", dependencies = TRUE); library(scales)}
  
  ##version history:
    version = "V. 1.6"
    #1.6: 2022-10-24: I made it split each output file into two, for with/without barcodes. Hopefully this works?
    #1.5: 2022-06-06: TWL added two new fields to see wtf is wrong with their shit - this handles them appropriately?
    #1.4: 2022-05-16: It now sorts descending by BC, taking a manual step out.  YIPPEEEEEE
    #1.3: 2022-01-10: new columns, so now it'll deal with the headers assuming there are 11 or 13 of them.
    #1.2: 2021-11-15: additional date added, so i made everything loop instead of a static number. ez-pz?
    #1.1: 2021-05-28: reformatted colnames so i don't have to manually reselect everything every time.
      #that's all. GLHF.
      #post-release edit: ze goggles do nothing. 
      #The DB won't recognize headers, so all of this was for nothing.
    #1.0: 2021-03-06: written up and ready2roll. 
      #The only wonky thing is the date function(because fuck consistency amirite?)
      #the rest is pretty straightforward. 
      #I'm adding a new column for the KD for importing, because that's easier than remembering that the KD is the HD+1?
      #that's all for now KTHX YW
  
  
  #functions
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  date.func <- function(value){
    as.Date(value, origin = "1900-01-01")-2
  } #the '-2' thing is fucked and i don't really understand why it works sometimes and not others. Keep an eye on this?
  #`%notin%` <- Negate(`%in%`)
  setwd("L:/Sample Records/Tyson Pork/Kill Sheets/Tyson Waterloo")
  
  path.file <- file.choose()
  directoryextractor.func(path.file)
  temp <- read.xlsx(path.file,sheet = 1)

  
  
  #this is wonky - i'm adding one to the date so that it reflects the KD in the DB we're going to use.  Wait I should use a different variable, huh.
  temp$DBKD <- temp$HARVESTDATE+1
  unique.harvestdate <- sort(unique(temp$HARVESTDATE), decreasing = T)
  #these come through as pseudo-julian dates, to deal with them we use date func, also will always have to be as.numeric()
  
  #now it sorts by trolleyID, then adds in the fake CN, then seps by date.
  temp <- temp[order(temp$TROLLEY_ID,decreasing = F),]
  
  temp$CN <- as.numeric(paste("-",seq(1:nrow(temp)), sep = ""))
  
  #I'm pretty sure that the two below colnaming things aren't necessary at all.
if(dim(temp)[2]==13){
    colnames(temp) <- c("Tattoo", "HarvestDate", "Harvest.plt", "ProducerNumber", 
                        "ProducerName", "HeadCount", "FarmDescription", "PremiseId", 
                        "DNABarcode", "DNADate", "Trolley_ID", "DBKD", "CN")
}else if(dim(temp)[2]==15){
    colnames(temp) <- c("Tattoo", "HarvestDate", "Harvest.plt", "ProducerNumber", 
                        "ProducerName", "TotalHeadCount", "FarmDescription", "Split.Date", "PremiseId", 
                        "HeadCount", "DNABarcode", "DNADate", "Trolley_ID", "DBKD", "CN")
}else if(dim(temp)[2]==17){
  colnames(temp) <- c("Tattoo", "HarvestDate", "Harvest.plt", "ProducerNumber", 
                      "ProducerName", "TotalHeadCount", "FarmDescription", "Split.Date", "PremiseId", 
                      "HeadCount", "DNABarcode", "DNADate", "Trolley_ID","COLDDATE", "SCALE_TIME", "DBKD", "CN")
    }

    for(i in 1:length(unique.harvestdate)){
  
      temp.t <- temp[temp$HarvestDate==unique.harvestdate[i],]
      temp.t$HarvestDate <- date.func(as.numeric(temp.t$HarvestDate))
      temp.t$DNADate <- date.func(as.numeric(temp.t$DNADate))
      temp.t$DBKD <- date.func(as.numeric(temp.t$DBKD))
      temp.t$COLDDATE <- date.func(as.numeric(temp.t$COLDDATE))
      temp.t$SCALE_TIME <- as.ITime(as.POSIXlt('2020-04-20 00:00:00 CST') + 3600*24*temp.t$SCALE_TIME)

###this line is a new try - hopefully it works?
      temp.final.3 <- temp.t[order(temp.t$DNABarcode, decreasing = F),]
      ##this is a new line - hopefully splitting each file into two - one with barcodes, one without. This is for each date.
      temp.final <- temp.final.3[temp.final.3$DNABarcode != 0,]
      temp.final.0 <- temp.final.3[temp.final.3$DNABarcode == 0,]
###this line is a new try - hopefully it works?
    
      write.table(temp.final, file = paste("TWLKS KD", date.func(as.numeric(unique.harvestdate[i]+1)),initials, version, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
      write.table(temp.final.0, file = paste("TWLKS KD", date.func(as.numeric(unique.harvestdate[i]+1)),initials, version, as.character((Sys.Date())), "zeroes .csv"), row.names = F, na = "", sep = ",")
      print(paste("output datasheet for date:", date.func(as.numeric(unique.harvestdate[i]))))
      print(dim(temp.t)[1])
    }
  #I added this section on 11/16/21 when an additional killdate was included.
  #It has issues FOR SURE (see: treating the 01/01/0001) but it MOSTLY works
  #with the addition of additional killdates the below was worse than just looping it, which is what I ended up doing.
  {  
  # temp.a <- temp[temp$HarvestDate==unique.harvestdate[1],]
  # temp.b <- temp[temp$HarvestDate==unique.harvestdate[2],]
  # 
  # #reformating dates as dates
  # temp.a$HarvestDate <- date.func(as.numeric(temp.a$HarvestDate))
  # temp.a$DNADate <- date.func(as.numeric(temp.a$DNADate))
  # temp.a$DBKD <- date.func(as.numeric(temp.a$DBKD))
  # 
  # temp.b$HarvestDate <- date.func(as.numeric(temp.b$HarvestDate))
  # temp.b$DNADate <- date.func(as.numeric(temp.b$DNADate))
  # temp.b$DBKD <- date.func(as.numeric(temp.b$DBKD))
  # 
  # #writing to file:
  # write.table(temp.a, file = paste("TWLKS KD", date.func(as.numeric(unique.harvestdate[1]+1)),initials, version, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
  # write.table(temp.b, file = paste("TWLKS KD", date.func(as.numeric(unique.harvestdate[2]+1)),initials, version, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
  } #this is the previous way everything was done. I took it out.
}
