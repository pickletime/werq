func.full.mince.analysis <- function(cars = F){
  version <- "V 0.2"
{
    
  #0.2: 2023-05-17
    #I changed the functionality quite a bit
    #it now goes through and does pairwise matching across ALL tasks. It still seems to bug out for some numbers, but it works for the most part.
    #this only exists because we needed pairwise matching across all tasks. It serves no other real purpose.
    #I fully expect to never use this again
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
    

#I'm apologizing in advance. What follows is the ugliest/clunkiest thing to have ever graced this brown and dying planet.
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
    output.2 <- output[2:num.tasks,]
    match.grid <- read.in[read.in[,1]!=0,1:num.tasks]
    match.grid.2 <- read.in[read.in[,2]!=0,2:num.tasks]

    
    
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

    

    for(j in 2:(num.tasks-1)){
      output.2 <- output[j:num.tasks,]
      output.2[,5:8] <- NA
      match.grid.2 <- read.in[read.in[,j]!=0,j:num.tasks]
        for(k in 2:length(j:num.tasks)){
          matches <- length(match.grid.2[match.grid.2[,k]!=0,k])
          ##this is for matching to the first task
          ##expected maths
          output.2$'% expected'[k] <- output.2$`% found`[1]*output.2$`% found`[k]
          output.2$'# expected'[k] <- output.2$`% expected`[k]*output.2$`Nest UCI`[k]
          #match math?
          output.2$matches[k] <- matches
          #LOGIC TESTING
          output.2$'In spec?'[k] <- output.2$matches[k] >= output.2$`# expected`[k]
      }
      output <- rbind(output,output.2)
    } #grid
    
  
  output$file <- c(init.source.file, source.file, rep("", nrow(output)-2))
  prev.filename <- gsub(".xlsx", "", source.file)
    
  #standard DT file naming convention
  #if I can get the filename picking thing to work then this is best, otherwise use the other
  #write.csv(output, file = paste("Full grinds analysis for ", prev.filename, " ", as.character((Sys.Date()))," ", qc.level*100, "% ", version, ".csv", sep = ""))
  write.csv(output, file = paste("Full grinds analysis for ", num.tasks, " tasks at QC level ", qc.level*100, "% ", as.character((Sys.Date()))," ", version, ".csv", sep = ""))
  #I'm just so clever

  print(output[,8]);
  print("Output in chosen directory")
}
