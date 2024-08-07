func.full.mince.analysis <- function(cars = F){
  version <- "V 0.1"
{
    #0.1: 2022-05-16
	#i updated to a new R, so the unlevel function didn't work. I replaced those with as.numeric instead. so far so good?
    #0: 2021-03-15
    #Oh man where to start. I'm adding in functionality for grind-carcass matching and data acquisition.
    #right now it finds the number of carcasses and dates that grinds match to. I'm pretty sure this can all be secondary.

  }#version history
  
#library loading
  if (!require("SpadeR")) {
    install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  
#local functions
  #extracting values from spadeR is difficult - I function'd it instead of dealing with it every time.
    #somehow drop.levels isn't real but droplevels is - hopefully it still works? i don't know how/why it worked before?
  unlevel.func <- function(value){as.double(levels(droplevels(value)))}
  #There were issues with unlisting, so I overdesigned a solution.
  unlist.func <- function(value){unlist(as.double(length(value)))[1]}
  #this extracts the directory from a choose.file(), so you don't have to redundantly choose dir AND file.
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  #this is being used to pull the number of profiles in a given task (in frame "frame") for a given n (singletons etc)
  profiles.func <- function(frame, n) {length((frame[frame[,i]>n,i]))}
  #i hate this one so much. I shouldn't have to work so hard to get julian dates.
  date.func <- function(value){
    as.Date(value, origin = "1900-01-01")-2}
  

#Initial setup - choosing 
  path.file <- file.choose()
  directoryextractor.func(path.file)
  file.list <- list.files()
  input.file <- read.xlsx(xlsxFile = path.file, sheet = "Capture hist", cols = c(2:100), rows = c(seq(4,10000)))
  read.in <- input.file[,grepl('^[0-9]*$',input.file[1,])]
  num.tasks <- as.double(ncol(read.in))
  output <- data.frame(data = NA, nrow = num.tasks, cols = 8)
  colnames(output) <- c("Profiles", "Nest", "Nest UCI")
#this is to make file naming easier downstream. Yes, I really am reading the file in three times. yep.
  panel.length <- read.xlsx(xlsxFile = path.file, sheet = "Summary report")[3,4]
  snp.cutoff <- as.double(read.xlsx(xlsxFile = path.file, sheet = "Settings")[8,2])
  qc.level <- snp.cutoff/panel.length
#This is an additional safeguard so you can look at the source file and the ORIGINAL source file
  init.source.file <- read.xlsx(xlsxFile = path.file, sheet = "Settings")[4,2]
  source.file <- file.list[grepl(100*qc.level, file.list)]
#the above grepl picks up the output files also, so i just take the first which SHOULD always be right
  source.file <- source.file[1]
    
#####grind x car matches, only runs if carcasses are flagged to be present.
  if(cars != FALSE){
    #reads in the AxA tab and the third sheet, which will always be the sample data tab
    AxA.tab <- read.xlsx(xlsxFile = path.file, sheet = "AxA match", cols = c(1:30), rows = c(seq(4,10000)))
    sample.data <- read.xlsx(xlsxFile = path.file, sheet = 3, cols = c(1:30), rows = c(seq(1,10000)),rowNames = F)
    
    #if carcasses are present:
    if("Carcass" %in% unique(sample.data$Sample.type)){
      AxA.tab.grinds <- AxA.tab[AxA.tab$S1.IdentiGEN.ID < 0 & AxA.tab$S2.Sample.type == "Carcass",]
      #if there are grind x car matches:
      if(dim(AxA.tab.grinds)[1] > 1){
        AxA.tab.grinds$car.date <- sample.data$Date[match(AxA.tab.grinds$S2.IdentiGEN.ID, sample.data$IdentiGENCode)]
        #count them
        grind.carcass.matches <- dim(AxA.tab.grinds)[1]
        #record the dates of the samples they match to.
        grind.carcass.match.dates <- date.func(unique(AxA.tab.grinds$car.date))
      }else{
        grind.carcass.matches <- NA
        grind.carcass.match.dates <- NA
      }
    }
  }
  
  
  #HOLY DUCKS THIS IS FUCKING AWESOME
  #input.file.original <- input.file
  #test <- read.in[sample(1:nrow(read.in), trunc(0.95*nrow(read.in)), replace=F),]
  #HOLY DUCKS THIS IS FUCKING AWESOME
  
#I'm apologizing in advance. What follows is the ugliest/clunkiest thing to have ever graced this brown and dying planet.
  if(num.tasks == 3){
    for(i in 1:num.tasks){
      #I added a test here for if doubletons write nothing so that it doesn't break when we do pop estimates. SUPER unlikely.
      #I could (should?) use a different pop estimator in this case. TDB.
      if(profiles.func(read.in,1)==0) {
        num.profile <- profiles.func(read.in,0)
        num.profiles <- num.profile+((num.profile*(num.profile-1))/(2*(0+1))) 
        output[i,] <- rbind(as.vector(c(num.profile, num.profiles, num.profiles)))

      } else {
        input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
	   output[i,] <- rbind(as.vector(c(as.numeric(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))

      }
    } #calculating pop values
    
    #I know there's a more elegant way to do this.
    output$'% found' <- rep(NA, num.tasks)
    output$'% expected' <- rep(NA, num.tasks)
    output$'# expected' <- rep(NA, num.tasks)
    output$matches <- rep(NA, num.tasks)
    output$'In spec?' <- rep(NA, num.tasks)
    output$'% found' <- output$Profiles/output$`Nest UCI`
    output.2 <- output[c(2, 3),]
    match.grid <- read.in[read.in[,1]!=0,]
    match.grid.2 <- read.in[read.in[,2]!=0,2:3]

    
    
    for(i in 2:num.tasks){
      ##this is for matching to the first task
      ##expected maths
      output$'% expected'[i] <- output$`% found`[1]*output$`% found`[i]
      output$'# expected'[i] <- output$`% expected`[i]*output$`Nest UCI`[i]
      #match math?
      output$matches[i] <- unlist.func(match.grid[match.grid[,i]!=0,i])
      #LOGIC TESTING
      output$'In spec?'[i] <- output$matches[i] >= output$`# expected`[i]
    } #grid 1
    
    ##this is for matching to the second task
    ##expected maths
    output.2[1,5:8] <- NA
    output.2$'% expected'[2] <- output.2$`% found`[1]*output.2$`% found`[2]
    output.2$'# expected'[2] <- output.2$`% expected`[2]*output.2$`Nest UCI`[2]
    #match math?
    output.2$matches[2] <- unlist.func(match.grid.2[match.grid.2[,2]!=0,2])
    #LOGIC TESTING
    output.2$'In spec?'[2] <- output.2$matches[2] >= output.2$`# expected`[2]
     #grid 2
    output <- rbind(output, output.2) #binds grid 1 and grid 2
   #running the grid
  }else if(num.tasks == 4){
    second.order <- c(3,4,2,1)
    for(i in 1:num.tasks){
      if(profiles.func(read.in,1)==0) {
        num.profile <- profiles.func(read.in,0)
        num.profiles <- num.profile+((num.profile*(num.profile-1))/(2*(0+1))) 
        output[i,] <- rbind(as.vector(c(num.profile, num.profiles, num.profiles)))
      }else{
        input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
        output[i,] <- rbind(as.vector(c(as.numeric(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))

      }
    } #calculating pop values

    #I know there's a more elegant way to do this.
    output$'% found' <- rep(NA, num.tasks)
    output$'% expected' <- rep(NA, num.tasks)
    output$'# expected' <- rep(NA, num.tasks)
    output$matches <- rep(NA, num.tasks)
    output$'In spec?' <- rep(NA, num.tasks)
    output$'% found' <- output$Profiles/output$`Nest UCI`
    output.2 <- output[c(3,4,1,2),]
    match.grid <- read.in[read.in[,1]!=0,]
    match.grid.2 <- read.in[read.in[,3]!=0,]
    match.grid.2[,c(3,4,1,2)] <- match.grid.2
    
    
    for(i in 2:num.tasks){
      ##this is for matching to the first task
      ##expected maths
      output$'% expected'[i] <- output$`% found`[1]*output$`% found`[i]
      output$'# expected'[i] <- output$`% expected`[i]*output$`Nest UCI`[i]
      #match math?
      output$matches[i] <- unlist.func(match.grid[match.grid[,i]!=0,i])
      #LOGIC TESTING
      output$'In spec?'[i] <- output$matches[i] >= output$`# expected`[i]
    } #grid 1
  
    
    for(i in 2:num.tasks){
      ##this is for matching to the second task
      ##expected maths
      output.2$'% expected'[i] <- output.2$`% found`[1]*output.2$`% found`[i]
      output.2$'# expected'[i] <- output.2$`% expected`[i]*output.2$`Nest UCI`[i]
      #match math?
      output.2$matches[i] <- unlist.func(match.grid.2[match.grid.2[,i]!=0,i])
      #LOGIC TESTING
      output.2$'In spec?'[i] <- output.2$matches[i] >= output.2$`# expected`[i]
    } #grid 2
    output <- rbind(output, output.2) #binds grid 1 and grid 2
  }else{
  #this is only if it won't go through either of the others.
    print("this function can only handle 3 or 4 task mince jobs - if this doesn't apply try elsewhere kthx")
    } #quits if condition 1 or 2 isn't met.
#Printing to file
  
  #putting the filenames, variables related to carcass matching in an interpretable place  
  output$file <- c(init.source.file, source.file, rep("", nrow(output)-2))
  if(cars != F){
        output$file[3] <- c(paste(grind.carcass.matches,"matching carcasses", sep = " "), rep("", nrow(output)-1))
        output$file[4] <- c(paste("matching carcass date: ", sep = ""))
        output$file[5:(4+length(grind.carcass.match.dates))] <- paste(grind.carcass.match.dates[1:length(grind.carcass.match.dates)], sep = ",")
  }
    
  #standard DT file naming convention
  write.csv(output, file = paste("Full grinds analysis for", num.tasks, "tasks at QC level", qc.level, as.character((Sys.Date())), version, ".csv", sep = " "))
  #I'm just so clever

  print(output[,8]);
  print("Output in chosen directory")
}
