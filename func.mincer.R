func.mince.analysis <- function(){
  version <- "V 1.5+"
  #1.5: 2020-07-14
    #it's been a potential source of error that source filenames aren't included anywhere 
    #they're both addended to the bottom of the results file
    #there are some funny logic checks that i should look into more like broken grepl
    #I added a column for pop estimate in the case of all singletons
#!!!!!I need to made the estimator section actually work
  {
  #1.4: 2020-02-04
    #the T2-T3 matching screwed me again, so i'm adding it in universally. 
    #It's not always applicable, but it'll still be useful.
    #10/10 would work on again
    #turned the in-spec test from greater than to greater than or equal to because i somehow missed that earlier? 
    #I added a "print T/F" so we know results seconds faster than before
  #1.3: 2019-10-30
    #There was ambiguity regarding versions somehow. I declare this a new one so that i can void anything made earlier.
    #In theory this is the validated version. 
    #Ideally I'll streamline this post-validation because I just need the maths to work out.
  #1.2: 2019-09-13
    #I was summing the profiles instead of counting them, which led to the numbers for the primal batch being incorrect. 
    #I added a variable into each loop that counts the length of nonzero vectors because that's the only way i know how.
  #1.1: 2019-09-03
    #I added in the filepath extractor function. that's all.
  #1.0: 2019-08-15
    #I need something that works, then I can go back and streamline
    #runs different things based on number of tasks, because the analyses are slightly different
    #outputs file with resulting grid containing pop values and the resulting in spec T/F
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
    #unlevel.func <- function(value){as.double(levels(drop.levels(value)))}
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
    
#####grind x car matches
  #reads in the qxr tab and the third sheet, which will always be the sample data tab
  QxR.tab <- read.xlsx(xlsxFile = path.file, sheet = "QxR match", cols = c(1:30), rows = c(seq(4,10000)))
  sample.data <- read.xlsx(xlsxFile = path.file, sheet = 3, cols = c(1:30), rows = c(seq(1,10000)),rowNames = F)
  
  #if carcasses are present:
  if("Carcass" %in% unique(sample.data$Sample.type)){
    QxR.tab.grinds <- QxR.tab[QxR.tab$S1.IdentiGEN.ID < 0 & QxR.tab$S2.Sample.type == "Carcass",]
    #if there are grind x car matches:
    if(dim(QxR.tab.grinds)[1] > 1){
      QxR.tab.grinds$car.date <- sample.data$Date[match(QxR.tab.grinds$S2.IdentiGEN.ID, sample.data$IdentiGENCode)]
      #count them
      grind.carcass.matches <- dim(QxR.tab.grinds)[1]
      #record the dates of the samples they match to.
      grind.carcass.match.dates <- date.func(unique(QxR.tab.grinds$car.date))
    }else{
      grind.carcass.matches <- NA
      grind.carcass.match.dates <- NA
    }
  }
    
  
#I'm apologizing in advance. What follows is the ugliest/clunkiest thing to have ever graced this brown and dying planet.
  if(num.tasks == 3){
#second.order <- c(2,3)
    for(i in 1:num.tasks){
      #I added a test here for if doubletons write nothing so that it doesn't break when we do pop estimates. SUPER unlikely.
      #I could (should?) use a different pop estimator in this case. TDB.
      if(profiles.func(read.in,1)==0) {
    #num.profiles <- length((read.in[read.in[,i]>0,i]))
        num.profile <- profiles.func(read.in,0)
        num.profiles <- num.profile+((num.profile*(num.profile-1))/(2*(0+1))) 
        output[i,] <- rbind(as.vector(c(num.profile, num.profiles, num.profiles)))

      } else {
        input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
        output[i,] <- rbind(as.vector(c(unlevel.func(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
      }
    } #calculating pop values
    
    #I know there's a more elegant way to do this.
    output$'% found' <- rep(NA, num.tasks)
    output$'% expected' <- rep(NA, num.tasks)
    output$'# expected' <- rep(NA, num.tasks)
    output$matches <- rep(NA, num.tasks)
    output$'In spec?' <- rep(NA, num.tasks)
    output$'% found' <- output$Profiles/output$`Nest UCI`
#output$'Pop estimator' <- estimator
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
#      output$'Pop estimator'[i] <- estimator
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
#    output.2$'Pop estimator'[2] <- estimator
     #grid 2
    output <- rbind(output, output.2) #binds grid 1 and grid 2
   #running the grid
  }  #3 task - everything BUT FPL
  else if(num.tasks == 4){
  #this is for fpl. There's definitely a more elegant way to do this, but that's to be addressed later.
    second.order <- c(3,4,2,1)
    for(i in 1:num.tasks){
      if(profiles.func(read.in,1)==0) {
        #num.profiles <- length((read.in[read.in[,i]>0,i]))
#        estimator <- "***Chao-1 bc"
        num.profile <- profiles.func(read.in,0)
        num.profiles <- num.profile+((num.profile*(num.profile-1))/(2*(0+1))) 
        output[i,] <- rbind(as.vector(c(num.profile, num.profiles, num.profiles)))
      } else {
#        estimator <- "iChao"
        input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
        output[i,] <- rbind(as.vector(c(unlevel.func(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
      }
#      output$'Pop estimator'[i] <- estimator
    } #calculating pop values

    #I know there's a more elegant way to do this.
    output$'% found' <- rep(NA, num.tasks)
    output$'% expected' <- rep(NA, num.tasks)
    output$'# expected' <- rep(NA, num.tasks)
    output$matches <- rep(NA, num.tasks)
    output$'In spec?' <- rep(NA, num.tasks)
    output$'% found' <- output$Profiles/output$`Nest UCI`
#    output$'Pop estimator'[1] <- estimator
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
#output$'Pop estimator'[i] <- estimator
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
#output.2$'Pop estimator'[i] <- estimator
    } #grid 2
    output <- rbind(output, output.2) #binds grid 1 and grid 2
  } #4 task - FPL
  else if(num.tasks !=3 && num.tasks !=4){
  #this is only if it won't go through either of the others.
    print("this function can only handle 3 or 4 task mince jobs - if this doesn't apply try elsewhere kthx")
    } #quits if condition 1 or 2 isn't met.
#Printing to file
  
  #putting the filenames, variables related to carcass matching in an interpretable place  
  output$file <- c(init.source.file, source.file, rep("", nrow(output)-2))
  output$num.matches <- c(paste(grind.carcass.matches,"matching carcasses", sep = ""), source.file, rep("", nrow(output)-2))
  output$match.dates <- c(paste(grind.carcass.match.dates,"matching carcass dates", sep = ""), source.file, rep("", nrow(output)-2))
  
  #standard DT file naming convention
  write.csv(output, file = paste("Grinds analysis for", num.tasks, "tasks at QC level", qc.level, as.character((Sys.Date())), version, ".csv", sep = " "))
  #I'm just so clever
  #print(1:output[(nrow(output)-2),8]);
  print(output[,8]);
  print("Output in chosen directory")
}
