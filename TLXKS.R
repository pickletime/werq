func.TLXKS <- function(select.grade = F, initials = "DT"){
    ###TLX killsheet shit
  #if samples have:
    #for BC = 0, make fake barcode by concatenating current date, KD, index#?
    #nondup barcodes
      #if dup: rename with -1, -2, etc
    #DNA date && breakdate
      #if real DNA OR breakdate, put into relevant sheet
    #put into new sheet by date
  #otherwise:
    #if no real dates:
      #assign date by time (?)
  
  #if (!require("SpadeR")) {
  #  install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  if (!require("formattable")) {
    install.packages("formattable", dependencies = TRUE); library(scales)}
  if (!require("data.table")) {
    install.packages("data.table", dependencies = TRUE); library(scales)}
  
  #functions
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  date.func <- function(value){
    as.Date(value, origin = "1900-01-01")-2
  } #the '-2' thing is fucked and i don't really understand why it works sometimes and not others. Keep an eye on this?
  
  #notes etc:
    #that date.func -2 thing is really fucked up. That'll bite me in the ass somehow FOR SURE
    #i need to figure out how to do the date assignments based on time
      #add new col for floor(time), build primitive likelihoods for each date?
      #OR if the times are always pretty different i could just guess? that's a terrible idea.
    #I guess I could just do an n-dimensional array instead of frames for each?
      #this is the real solution. I could likely work with starting filetype?
    
  
  #Is "select" included?
  #grade.select <- T
  
  path.file <- file.choose()
  directoryextractor.func(path.file)
  temp <- read.xlsx(path.file)
  file.retainer <- temp
#  temp <- file.retainer
  unique.dnadate <- sort(unique(temp$DNA.DATE), decreasing = T)
  #these come through as pseudo-julian dates, to deal with them we use date func, also will always have to be as.numeric()
  
  #Time comes through as a decimal. This makes it easier to work with, and it's converted to standard hh:mm:ss at the end.
  temp$BREAKTIME <- temp$BREAKTIME*24
  
  # file.retainer.b <- temp
  #  temp <- file.retainer.b
  #I should rename zeroes, then test for duplicates here
  #renaming idea (replace '1' with 'i'):
    #sum(temp.c[1,c(1:3,7,10,16:18,20)])*(temp.c[1,20]+temp.c[1,1])*10
  
  
  #seperating samples from each date set into respective frames. 
  temp.a <- temp[temp$DNA.DATE==unique.dnadate[1],]
  temp.b <- temp[temp$DNA.DATE==unique.dnadate[2],]
  temp.c <- temp[temp$DNA.DATE==unique.dnadate[3],]
  temp.d <- temp.c
  
  
  #if SEL is included or not
   if(select.grade != T){
     print(paste(dim(temp.c[temp.c$GRADE == "SEL",])[1],"samples excluded because BC=0 and SEL not cut", sep = " "))
     temp.SEL <- temp.c[temp.c$GRADE == "SEL",]
     temp.c <- temp.c[temp.c$GRADE != "SEL",]
     }
#  temp.d <- temp.c
  
  temp.a <- rbind(temp.a, temp.c[temp.c$BREAKDATE == unique.dnadate[1],])
  temp.b <- rbind(temp.b, temp.c[temp.c$BREAKDATE == unique.dnadate[2],])
  #greater than or equal to date two?
  temp.c <- temp.c[!temp.c$BREAKDATE %in% unique.dnadate[1:2],]
    
  floor.a <- floor(temp.a$BREAKTIME)
  floor.b <- floor(temp.b$BREAKTIME)
  floor.sum <- unique(sort(floor(temp$BREAKTIME)))

  
  test.matrix <- matrix(data = NA, nrow = length(floor.sum)+1, ncol = 6)
  #IDK why this is bugging out, so i added +1 to the nrow?
  
  #I need to make a table these are output to, look at %, test, assign
  #i should just make this a dataframe
  #cols are: 
    #floor'd breaktime, so this is the hour within which event occured.
    #number of samples within hour chunk date 1, 
    #number of samples within hour chunk date 2, 
    #proportion of hour chunk from date 1, 
    #proportion of hour chunk from date 2, 
    #which date more represented, which date a date-less sample would be assigned to.
  for(i in 1:length(floor.sum)){
    test.matrix[i,1] <- floor.sum[i]
    test.matrix[i,2] <- length(floor.a[floor.a == floor.sum[i]])
    test.matrix[i,3] <- length(floor.b[floor.b == floor.sum[i]])
  #so this bugged out if both are 0, so i have it only do the maths if both aren't zero.
    if(test.matrix[i,2]!=0 | test.matrix[i,3]!=0){ 
      test.matrix[i,4] <- test.matrix[i,2]/sum(test.matrix[i,2:3])
      test.matrix[i,5] <- test.matrix[i,3]/sum(test.matrix[i,2:3])
      if(test.matrix[i,4] > test.matrix[i,5]){test.matrix[i,6] <- as.numeric(unique.dnadate[1])}
      else if(test.matrix[i,4] < test.matrix[i,5]){test.matrix[i,6] <- as.numeric(unique.dnadate[2])}
      else(test.matrix[i,6]<- "")
    }
      else({test.matrix[i,4:6] <- 0})
  } #YEP IT'S THAT EASY
  
  
  #testing the (breakdate != real) frame for dates, writing.
  #I could make this a while loop I think? continue to redefine temp.c and reassign?
  for(i in 1:dim(temp.c)[1]){
    temp.c$DNA.DATE[i] <- test.matrix[test.matrix[,1]==floor(temp.c$BREAKTIME[i]),6]
  }
  
  #!!!here I should make fake barcodes for the BC=0 samples
  

  
  #adding those samples back into the correct killdates:
  #This is currently for the time-binned samples, which is definitely not correct at all.
  temp.a <- rbind(temp.a, temp.c[temp.c$DNA.DATE==unique.dnadate[1],])
  temp.b <- rbind(temp.b, temp.c[temp.c$DNA.DATE==unique.dnadate[2],])
  temp.c <- temp.c[temp.c$DNA.DATE==0,]
  #temp.a and temp.b are good, but there'll be a handful of temp.c samples to be handled
  
  #####
  #this is probably not going to be used. If samples aren't assigned it'll drop an hour and retest (23 turns into 22, etc)
  # for(i in 1:dim(temp.c)[1]){
  #   temp.c$DNA.DATE[i] <- test.matrix[test.matrix[,1]==floor(temp.c$BREAKTIME[i])-1,6]
  # } #this drops an hour and retests. We'll then reassign.
  # temp.a <- rbind(temp.a, temp.c[temp.c$DNA.DATE==unique.dnadate[1],])
  # temp.b <- rbind(temp.b, temp.c[temp.c$DNA.DATE==unique.dnadate[2],])
  # temp.c <- temp.c[temp.c$DNA.DATE==0,]
  #####
  
  
  #test for duplicate: EID, CN, BC
  #if temp.c is zero, don't write
  #flag moved samples with new column? yeah, that.
  #test for significant overlap of times? flag those in temp.c
  
  
  
  
  par(mar = c(1, 1, 1, 1))
  par(mfrow=c(3,1))
  # # 
  hist(temp.a$BREAKTIME, xlim = c(0,24), col = "green", freq = F, breaks = 24)
  hist(temp.b$BREAKTIME, xlim = c(0,24), col = "blue", freq = F, breaks = 24)
  hist(temp.c$BREAKTIME, xlim = c(0,24), col = "pink", freq = F, breaks = 24)
  
  #formatting fields to be correct, specifically: harvest/dna (and possibly break) date, breaktime.
  #!!!!I need to correct the DNA/Breakdate thing so that it only replaces IF it's a real number, not overwrite fields
    #temp.c$BREAKDATE[temp.c$BREAKDATE > 1] <- unique.dnadate[1]
    #I have to convert the columns to char first, so i could either do that and then replace with chars?
  temp.a$HARVESTDATE <- date.func(as.numeric(temp.a$HARVESTDATE))
  temp.a$DNA.DATE <- date.func(as.numeric(unique.dnadate[1]))
  temp.q <- as.character(temp.a$BREAKDATE)
  #temp.a$BREAKDATE[temp.a$BREAKDATE > 1] <- date.func(as.numeric(unique.dnadate[1]))
  temp.a$BREAKTIME <- as.ITime(as.POSIXlt('2020-04-20 00:00:00 CST') + 3600*temp.a$BREAKTIME)
  
  temp.b$HARVESTDATE <- date.func(as.numeric(temp.b$HARVESTDATE))
  temp.b$DNA.DATE <- date.func(as.numeric(unique.dnadate[2]))
  #temp.b$BREAKDATE <- date.func(as.numeric(unique.dnadate[2]))
  temp.b$BREAKTIME <- as.ITime(as.POSIXlt('2020-04-20 00:00:00 CST') + 3600*temp.b$BREAKTIME)

  temp.c$BREAKTIME <- as.ITime(as.POSIXlt('2020-04-20 00:00:00 CST') + 3600*temp.c$BREAKTIME)
  
  
  
  
  #write.csv(temp.a, file = paste("TLXKS KD", date.func(as.numeric(unique.dnadate[1])), "script testing dt",initials, as.character((Sys.Date())), ".csv"), row.names = F)
  write.table(temp.a, file = paste("TLXKS KD", date.func(as.numeric(unique.dnadate[1])), "script testing",initials, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
  #write.csv(temp.b, file = paste("TLXKS KD", date.func(as.numeric(unique.dnadate[2])), "script testing dt",initials, as.character((Sys.Date())), ".csv"), row.names = F)
  write.table(temp.b, file = paste("TLXKS KD", date.func(as.numeric(unique.dnadate[2])), "script testing",initials, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
  #write.csv(temp.c, file = paste("TLXKS KD", date.func(as.numeric(unique.dnadate[3])), "script testing dt",initials, as.character((Sys.Date())), ".csv"), row.names = F)
  if(dim(temp.c)[1] !=0) {
    write.table(temp.c, file = paste("TLXKS KD problematic samples script testing",initials, as.character((Sys.Date())), ".csv"), row.names = F, na = "", sep = ",")
  } else { print("no problematic samples remain after time-based assignments")}
  
  
  #!!!!times are right
  #some of the dates are right - i need to figure out how to only replace the shit ones
  #this whole thing might be for nothing. so that's neat i guess.
  
}