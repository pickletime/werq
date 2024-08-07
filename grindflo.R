grindfloVal1 <- function(initials = "DT", tasks.per.batch = 10){
  func.version <- "V 1.3"
  {
    #1.3 2024-06-14: we are now using both pop estimates (iChao, Chao1), so the table got a lot more complicated.
      #this definitely took a lot more work than i expected it to, but it works (so far)
    #1.2 2020-09-16: smarted the result output to inform if matches. 
      #Took much longer than it should've.
      #check cells in the df are hardcoded - if anything changed AM FUCKED.
      #it def takes too long to run, but it works so that's what matters?
    #1.1 2020-04-08: this revision is purely aesthetic - I've taken out the "NA" in the final table so it's easier to look at.
      #also i changed the final write.csv -> write.table so now the colnames aren't written BASED KNOWLEDGE
      #also added a test for batch 1 N/P to make it painfully obvious if coverage isn't high enough
    #1.0 2019-08-29: I added the extraction functionality to the file.choose. I figure I might as well make something magical? 
    #0.4 2019-08-27: made the output smarter/easier to interpret. Also added in feature to define tasks/batch, defaults to 10.
    #0.3 2019-08-23: ROS suggestions for clarity and less incompetence. Specifically the ceiling(maxtask/10)
    #0.2 2019-08-21: works pretty well? I haven't found anything that breaks it yet FINGERS CROSSED
    #0.1 2019-08-21: works for 30 tasks, need to add an if for tasks > 30 
    #0.0 2019-08-20: testbuild for grindflow shit, hopefully it mostly works
    #TO DO:
    #Troubleshoot?
  } #version history etc
  
  
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
  #this extracts the directory from a choose.file(), so you don't have to redundantly choose dir AND file.
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  
  
  
  #bUsInEsS tImE  
  path.file <- file.choose()
  directoryextractor.func(path.file)
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
  output.table <- data.frame('batch' = 1:num.batches, 'samples' = NA, 'profiles' = NA, "iChao est" = NA, "Chao1 est" = NA, 
                             'iChao N.P' = NA, "iChao coverage" = NA, 'Chao1 N.P' = NA, "Chao1 coverage" = NA,
                             "M1" = NA, "M2" = NA,"M3" = NA,
                             "iChao Sh1" = NA, "iChao Sh2" = NA,"iChao Sh3" = NA, "iChao test" = NA, "iChao detection level" = NA, 
                             "Chao1 Sh1" = NA, "Chao1 Sh2" = NA,"Chao1 Sh3" = NA, "Chao1 test" = NA, "Chao1 detection level" = NA, "filepath" = NA)
  for(i in 1:num.batches){
    #setting up table, calculating pop estimates etc
    output.table$samples[i] <- sum(summed.tasks[,i])
    output.table$profiles[i] <- length(summed.tasks[summed.tasks[,i] > 0,i])
    chao.value <- ChaoSpecies(summed.tasks[,i], "abundance", conf = 0.95)
    output.table$iChao.est[i] <- round(chao.value$Species_table[5,1],1)
    output.table$Chao1.est[i] <- round(chao.value$Species_table[4,1],1)
    output.table$iChao.N.P[i] <- signif(output.table$iChao.est[i]/output.table$profiles[i], 3)
    output.table$Chao1.N.P[i] <- signif(output.table$Chao1.est[i]/output.table$profiles[i], 3)
    output.table$iChao.coverage[i] <- signif(output.table$profiles[i]/output.table$iChao.est[i], 3)
    output.table$Chao1.coverage[i] <- signif(output.table$profiles[i]/output.table$Chao1.est[i], 3)
    temp <- summed.tasks[summed.tasks[,i]>0,]
    for(j in 1:num.batches){
      #match grid
      if(i >= j){ 
        output.table[j,9+i] <- NA
      } else { 
        output.table[j,9+i] <- sum(temp[temp[,j]>0,j])
      }
    }
  } 
  for(i in 1:num.batches){    
    #for some reason doing the shared with the matching doesn't work i'm confused
    #this is calculating the shared material
    for(j in 1:num.batches){
      if(i >= j){ 
        output.table[j,12+i] <- NA
        output.table[j,17+i] <- NA
      } else { 
        output.table[j,12+i] <- signif(100*((output.table$M1[j]/output.table$samples[j])*output.table$iChao.N.P[i]), 3)
        output.table[j,17+i] <- signif(100*((output.table$M1[j]/output.table$samples[j])*output.table$Chao1.N.P[i]), 3)
      } 
    }
  }
  
  
  #The cluster below is for "testing" with 1 sample found in batch N and the resulting detection level.
  
  if(as.double(output.table[num.batches,10])>0){
    detection.result <- "matches detected"
    output.table$iChao.detection.level[1] <- detection.result
    output.table$Chao1.detection.level[1] <- detection.result
  } else {
    # output.table$iChao.test[1] <- signif(100*((1/output.table$samples[j])*output.table$iChao.N.P[i]), 3)
    # output.table$Chao1.test[1] <- signif(100*((1/output.table$samples[j])*output.table$Chao1.N.P[i]), 3)
    
     output.table$iChao.test[1] <- signif(100*((1/output.table$samples[j])*output.table$iChao.N.P[1]), 3)
     output.table$Chao1.test[1] <- signif(100*((1/output.table$samples[j])*output.table$Chao1.N.P[1]), 3)
    
    if(as.double(output.table$iChao.test[1]) < 0.1){
      detection.result <- "Detection level sufficient"
    } else {
      detection.result <- "Insufficient detection level"
    }; output.table$iChao.detection.level[1] <- detection.result
    if(as.double(output.table$Chao1.test[1]) < 0.1){
      detection.result <- "Detection level sufficient"
    } else {
      detection.result <- "Insufficient detection level"
    }; output.table$Chao1.detection.level[1] <- detection.result
  }
  
  
  
  output.table$filepath[1] <- path.file
  #i want it to remove the unused columns if they're unused. 
  if(num.batches == 3){output.table <- output.table[,-c(12,15, 20)]}
  #transpose, because easier to interpret and more similar to what we've historically put out.
  output.table <- t(output.table)
  output.table[is.na.data.frame(output.table)] <- ""

  
  
  
  #Printing to file
  #standard naming convention, to be changed for personal preference
  write.table(output.table, file = paste("Grindflow", num.tasks, "tasks at QC level", qc.level, initials, func.version, as.character((Sys.Date())), ".csv", sep = " "), col.names = F, sep = ",")
  if(num.batches == 3) {print(output.table[c(15,19),1])
  } else if(num.batches == 4) {print(output.table[c(16,21),1])}
  if(output.table[7,1]<0.7){print("Batch 1 coverage insufficient iChao")}
  if(output.table[9,1]<0.7){print("Batch 1 coverage insufficient Chao1")}
}
