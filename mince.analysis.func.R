mince.analysis.func <- function(){
  version <- "V 1.3"
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
  
#library loading
  if (!require("SpadeR")) {
    install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
#  if (!require("formattable")) {
#    install.packages("formattable", dependencies = TRUE); library(scales)}
  
#local functions
  #extracting values from spadeR is difficult - I function'd it instead of dealing with it every time.
  unlevel.func <- function(value){as.double(levels(drop.levels(value)))}
  #There were issues with unlisting, so I overdesigned a solution.
  unlist.func <- function(value){unlist(as.double(length(value)))[1]}
  #this extracts the directory from a choose.file(), so you don't have to redundantly choose dir AND file.
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }

#Initial setup - choosing 
  path.file <- file.choose()
  directoryextractor.func(path.file)
  input.file <- read.xlsx(xlsxFile = path.file, sheet = "Capture hist", cols = c(2:100), rows = c(seq(4,10000)))
  read.in <- input.file[,grepl('^[0-9]*$',input.file[1,])]
  num.tasks <- as.double(ncol(read.in))
  output <- data.frame(data = NA, nrow = num.tasks, cols = 8)
  colnames(output) <- c("Profiles", "Nest", "Nest UCI")
#this is to make file naming easier downstream. Yes, I really am reading the file in three times. yep.
  panel.length <- read.xlsx(xlsxFile = path.file, sheet = "Summary report")[3,4]
  snp.cutoff <- as.double(read.xlsx(xlsxFile = path.file, sheet = "Settings")[8,2])
  qc.level <- snp.cutoff/panel.length

  
#I'm apologizing in advance. What follows is the ugliest/clunkiest thing to have ever graced this brown and dying planet.
  if(num.tasks == 3){
  #only for 3 task jobs, so it'll appropriately handle primals
    for(i in 1:num.tasks){
      #I added a test here for if doubletons write nothing so that it doesn't break when we run primals (assuming proper sampling)
      #primals now have length(i) instead of sum(i) because one is right and the other isn't.
      if(length(read.in[read.in[,i]>1,i])==0 || i==3) {
        num.profiles <- length((read.in[read.in[,i]>0,i]))
        output[i,] <- rbind(as.vector(c(num.profiles, num.profiles, num.profiles)))
      } else {
        input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
        output[i,] <- rbind(as.vector(c(unlevel.func(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
      }
    } #calculating pop values
    #I know there's a more elegant way to do this but i don't really care right now. to be fixed?
    output$'% found' <- rep(NA, num.tasks)
    output$'% expected' <- rep(NA, num.tasks)
    output$'# expected' <- rep(NA, num.tasks)
    output$matches <- rep(NA, num.tasks)
    output$'In spec?' <- rep(NA, num.tasks)
    #trying this as a loop - we'll see
    output$'% found' <- output$Profiles/output$`Nest UCI`
    match.grid <- read.in[read.in[,1]!=0,]
    
    for(i in 2:num.tasks){
      ##expected maths
      output$'% expected'[i] <- output$`% found`[1]*output$`% found`[i]
      output$'# expected'[i] <- output$`% expected`[i]*output$`Nest UCI`[i]
      #match math?
      output$matches[i] <- unlist.func(match.grid[match.grid[,i]!=0,i])
      #LOGIC TESTING
      output$'In spec?'[i] <- output$matches[i] > output$`# expected`[i]
    } #running the grid
  } else if(num.tasks == 4){
  #this is for fpl. There's definitely a more elegant way to do this, but that's to be addressed later.
    second.order <- c(3,4,2,1)
    for(i in 1:num.tasks){
      #I added a test here for if doubletons write nothing so that it doesn't break when we do pop estimates. SUPER unlikely.
      if(length(read.in[read.in[,i]>1,i])==0) {
        num.profiles <- length((read.in[read.in[,i]>0,i]))
        output[i,] <- rbind(as.vector(c(num.profiles, num.profiles, num.profiles)))
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
      output$'In spec?'[i] <- output$matches[i] > output$`# expected`[i]
    } #grid 1
  
    
    for(i in 2:num.tasks){
      ##this is for matching to the second task
      ##expected maths
      output.2$'% expected'[i] <- output.2$`% found`[1]*output.2$`% found`[i]
      output.2$'# expected'[i] <- output.2$`% expected`[i]*output.2$`Nest UCI`[i]
      #match math?
      output.2$matches[i] <- unlist.func(match.grid.2[match.grid.2[,i]!=0,i])
      #LOGIC TESTING
      output.2$'In spec?'[i] <- output.2$matches[i] > output.2$`# expected`[i]
    } #grid 2
    output <- rbind(output, output.2) #binds grid 1 and grid 2
  } else if(num.tasks !=3 && num.tasks !=4){
  #this is only if it won't go through either of the others.
    print("this function can only handle 3 or 4 task mince jobs - if this doesn't apply try elsewhere kthx")
    } #quits if condition 1 or 2 isn't met.
#Printing to file
  #standard DT file naming convention
  write.csv(output, file = paste("Grinds analysis for", num.tasks, "tasks at QC level", qc.level, as.character((Sys.Date())), version, ".csv", sep = " "))
  #I'm just so clever
  print("Output in chosen directory")
}
