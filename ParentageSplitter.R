ParentageSplitter <- function(){
  #set wd of choosing
  setwd(choose.dir())
  #read in file of choosing
  file <- read.csv(file.choose(), header = T)
  
  #subdivide based on known established criteria
  calves <- file[file$Job!="NoJob",-(2:11)]
  sires <- file[file$Task=="NoTask",-(2:11)]
  genotype <- file[,-(2:11)]
    #remember that numbers have now changed, snp 1 is now in col 2
  
  #standard dt file formatting
  date <- as.character((Sys.Date()))
  
  #output various files
  write.table(calves, paste("calves ", date,".txt"), row.names = F, sep = "\t")
  print("calves done")
  write.table(sires, paste("sires ", date,".txt"), row.names = F, sep = "\t")
  print("sires done")
  write.table(genotype, paste("genotype ", date,".txt"), row.names = F, sep = "\t")
  print("GT file done")
  print("ALL DONE")
}
