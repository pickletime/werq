grindfloVal1 <- function(tasks.per.batch = 10){
  func.version <- "V 0.4"
  #0.0 08/20/19: testbuild for grindflow shit, hopefully it mostly works?
  #0.1 08/21/19: works for 30 tasks, need to add an if for tasks > 30 
  #0.2 08/21/19: works pretty well? I haven't found anything that breaks it yet FINGERS CROSSED
  #0.3 08/23/19: ROS suggestions for clarity and less incompetence. Specifically the ceiling(maxtask/10)
  #0.4 08/27/19: made the output smarter/easier to interpret. Also added in feature to define tasks/batch, defaults to 10.
  
  #TO DO:
    #add in a batches/task option

  
#libraries - this should be all that you need. Hopefully it is?
  if (!require("SpadeR")) {
    install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  if (!require("formattable")) {
    install.packages("formattable", dependencies = TRUE); library(scales)}
  
#local functions
  #the iChao output is formatted strangely, so something like the function below is necessary
  drop.level.func <- function(item){as.double(levels(drop.levels(item)))}
  #I was having issues with unlisting some things, so the following function fixs it.
  unlist.func <- function(item){unlist(as.double(length(item)))[1]}
  #The following function defines task numbers corresponding to batches. Definitely overengineered.
  sequence.func <- function(i) {(tasks.per.batch*(i-1)+1):(tasks.per.batch*i)}
  
  tasks.per.batch <- 10
  
#bUsInEsS tImE  
  setwd(choose.dir())
  path.file <- file.choose()
  #let's see how this goes
  input.file <- read.xlsx(xlsxFile = path.file, sheet = "Capture hist", cols = c(2:100), rows = c(seq(4,10000)))
  read.in <- input.file[,grepl('^[0-9]*$',input.file[1,])]
  num.tasks <- as.double(ncol(read.in))
  max.task <- max(as.double(colnames(read.in)))
  num.samples <- as.double(length(read.in[,1]))
  #rounds up to nearest number of batches - if 31 tasks, then 4 batches
  num.batches <- ceiling(max.task/tasks.per.batch)
  summed.tasks <- data.frame(data = NA, rows = num.samples, cols = num.batches)
  
  #add in QC level - I appreciate how insane it is that i'm reading in the file 3 times but I'm too dumb for an alternative.
  panel.length <- read.xlsx(xlsxFile = path.file, sheet = "Summary report")[3,4]
  snp.cutoff <- as.double(read.xlsx(xlsxFile = path.file, sheet = "Settings")[8,2])
  qc.level <- snp.cutoff/panel.length
  
  
#easiest way to do everything, for me, was to just sum tasks for each batch, move forward with that. 
  for(i in 1:num.batches){
    for(j in 1:num.samples){
    summed.tasks[j,i] <- sum(read.in[j,colnames(read.in) %in% sequence.func(i)])
    }
  } #summing across df
  

  #putting together that super sweet data table
  output.table <- data.frame('batch' = 1:num.batches, 'samples' = NA, 'profiles' = NA, "iChao1 est" = NA, 'N.P' = NA, "coverage" = NA, 
                             "M1" = NA, "M2" = NA,"M3" = NA,
                             "Sh1(x100)" = NA, "Sh2(x100)" = NA,"Sh3(x100)" = NA, "test" = NA, "detection level" = NA)
  for(i in 1:num.batches){
    #setting up table, calculating pop estimates etc
    output.table$samples[i] <- sum(summed.tasks[,i])
    output.table$profiles[i] <- length(summed.tasks[summed.tasks[,i] > 0,i])
    chao.value <- ChaoSpecies(summed.tasks[,i], "abundance", conf = 0.95)
    output.table$iChao1.est[i] <- chao.value$Species_table[5,1]
    output.table$N.P[i] <- output.table$iChao1.est[i]/output.table$profiles[i]
    output.table$coverage[i] <- output.table$profiles[i]/output.table$iChao1.est[i]
    temp <- summed.tasks[summed.tasks[,i]>0,]
     for(j in 1:num.batches){
     #match grid
       if(i >= j){ 
         output.table[j,6+i] <- NA
         } else { 
           output.table[j,6+i] <- sum(temp[temp[,j]>0,j])
         }
     }
} 
  for(i in 1:num.batches){    
  #for some reason doing the shared with the matching doesn't work i'm confused
  #this is calculating the shared material
    for(j in 1:num.batches){
      if(i >= j){ 
        output.table[j,9+i] <- NA
      } else { 
        output.table[j,9+i] <- 100*((output.table[j,6+i]/output.table[j,2]) * output.table[i,5])
      } 
  }
}
  
#The cluster below is for "testing" with 1 sample found in batch N and the resulting detection level.
  #i've commented this out to see if i can, using two frames, make the output more intelligible.
 output.table$test[1] <- 100*((1/output.table[j,2]) * output.table[1,5])
 if(as.double(output.table$test[1]) < 0.10){
   detection.result <- "Detection level sufficient"
 } else {
   detection.result <- "Insufficient detection level"
 }
 output.table$detection.level[1] <- detection.result
 

  
#Printing to file
  #standard naming convention, to be changed for personal preference
  write.csv(output.table, file = paste("Grindflow", num.tasks, "tasks at QC level", qc.level, func.version, ".csv", sep = " "))
  #I'm just so clever
  print("analysis complete, result output")
}
