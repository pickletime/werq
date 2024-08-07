func.grindflo.val2 <- function(initials = "DT", tasks.per.batch = 9){
  func.version <- "V 1.0"
  {
    #1.0 2020-04-08: it works. I need to reformat the output and reframe "pass/fail" but for now it's geet oogee
    #TO DO:
    #Troubleshoot?
  } #version history etc
  
#this definitely assumes that the tasks are set up as 1:2, 11:19, etc. If they aren't everything is borked.
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
  #Defines task numbers corresponding to batches. Definitely overengineered.
    func.sequence.val2 <- function(i) {if(i == 1){1:2}else{((tasks.per.batch*(i-1)+1):(tasks.per.batch*i))+(i-1)}}
  #this extracts the directory from a choose.file(), so you don't have to redundantly choose dir AND file.
    func.directoryextractor <- function(selected.file){
      filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
      filename <- filename.vector[length(filename.vector)]
      setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
    }
    
print("this definitely assumes that the tasks are set up as 1:2, 11:19, etc. If they aren't everything is borked.")
print("if this assumption isn't met quit out, reevaluate  your life")
  
  #bUsInEsS tImE  
  path.file <- file.choose()
  func.directoryextractor(path.file)
  #let's see how this goes
  input.file <- read.xlsx(xlsxFile = path.file, sheet = "Capture hist", cols = c(2:100), rows = c(seq(4,10000)))
  read.in <- input.file[,grepl('^[0-9]*$',input.file[1,])]
  num.tasks <- as.double(ncol(read.in))
  max.task <- max(as.double(colnames(read.in)))
  num.samples <- as.double(length(read.in[,1]))
  #rounds up to nearest number of batches - if 31 tasks, then 4 batches
  
  num.batches <- ceiling(max.task/tasks.per.batch)-1
#I redid this line because have the "-1" which is in the commented out line above breaks it REAL BAD  
    #num.batches <- ceiling(max.task/tasks.per.batch)
#I redid this line because have the "-1" which is in the commented out line above breaks it REAL BAD  
  summed.tasks <- data.frame(data = NA, rows = num.samples, cols = num.batches)
  
  #add in QC level - I appreciate how insane it is that i'm reading in the file 3 times but I'm too dumb for an alternative.
  panel.length <- read.xlsx(xlsxFile = path.file, sheet = "Summary report")[3,4]
  snp.cutoff <- as.double(read.xlsx(xlsxFile = path.file, sheet = "Settings")[8,2])
  qc.level <- snp.cutoff/panel.length
  
  
  #the secret is: func.sequence.val2(i) + (i-1)
  
  #easiest way to do everything, for me, was to just sum tasks for each batch, move forward with that. 
  for(i in 1:num.batches){
    for(j in 1:num.samples){
      summed.tasks[j,i] <- sum(read.in[j,colnames(read.in) %in% func.sequence.val2(i)])
    }
  } #summing across df
  
  
  #putting together that super sweet data table
  output.table <- data.frame('Time' = 0:(num.batches-1), 'samples' = NA, 'profiles' = NA, "iChao1 est" = NA, 'N.P' = NA, "coverage" = NA, 
                             "M1" = NA, "M2" = NA,"M3" = NA,
                             "Sh1(x100)" = NA, "Sh2(x100)" = NA,"Sh3(x100)" = NA, "test" = NA, "detection level" = NA, "filepath" = NA)
  for(i in 1:num.batches){
    #setting up table, calculating pop estimates etc
    output.table$samples[i] <- sum(summed.tasks[,i])
    output.table$profiles[i] <- length(summed.tasks[summed.tasks[,i] > 0,i])
    chao.value <- ChaoSpecies(summed.tasks[,i], "abundance", conf = 0.95)
    output.table$iChao1.est[i] <- signif(chao.value$Species_table[5,1],3)
    if(i == 1){output.table$N.P[i] <- 1
    }else{
    output.table$N.P[i] <- signif(output.table$iChao1.est[i]/output.table$profiles[i],3)
    }
    output.table$coverage[i] <- signif(output.table$profiles[i]/output.table$iChao1.est[i],3)
    temp <- summed.tasks[summed.tasks[,i]>0,]
    for(j in 1:num.batches){
      #match grid
      if(i >= j){ 
        output.table[j,6+i] <- NA
      } else { 
        output.table[j,6+i] <- sum(temp[temp[,j-1]>0,j-1])
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
        output.table[j,9+i] <- signif(100*((output.table[j,6+i]/output.table[j,2]) * output.table[i,5]),3)
      } 
    }
  }
  
  ##
  # I need to set the batch 1 pop est to the number of profiles because that's what we're supposed to do. 
  # Also chao-bc, not chao-1?
  ##
  
  #The cluster below is for "testing" with 1 sample found in batch N and the resulting detection level.
  #i've commented this out to see if i can, using two frames, make the output more intelligible.
  output.table$test[1] <- signif(100*((1/output.table[j,2]) * output.table[1,5]),3)
  if(as.double(output.table$test[1]) < 0.10){
    detection.result <- "Detection level sufficient"
  } else {
    detection.result <- "Insufficient detection level"
  }
  #this was bugging out, now i force it to only assign one value
  output.table$detection.level[1] <- detection.result
  output.table$filepath[1] <- path.file[1]
  #i want it to remove the unused columns if they're unused. 
  if(num.batches == 3){output.table <- output.table[,-c(9,12)]}
  #transpose, because easier to interpret and more similar to what we've historically put out.
    #I'm replacing "NA" with "" because the NA are terrible.
  output.table <- t(output.table)
  output.table[is.na.data.frame(output.table)] <- ""
  #this took too long - trying to say which batches had carryover. Shit i'm dumb.
  carryover <- as.double(output.table[7,1:num.batches])>0
  carryover <- as.double(output.table[1,carryover])
  carryover <- carryover[-1]
  
  
  
  
  #Printing to file
  #standard naming convention, to be changed for personal preference
  write.table(output.table, file = paste("Grindflow val 2 at QC level", qc.level, initials, func.version, as.character((Sys.Date())),".csv", sep = " "),col.names = F, sep = ",")
  # write.table(output.table, col.names = F, row.names = T, quote = F,
  #             file = paste("Grindflow", num.tasks, "tasks at QC level", qc.level, initials, func.version, ".csv", sep = " "))
  #I'm just so clever
  print(paste("analysis complete, ", detection.result))
  print(paste("carryover found at timepoints: "))
  print(carryover)
}
