
aurKSconsol <- function(directory, initials = "dt"){
  
  #set dir
  setwd(directory)
  
  #only look at actual killsheets, ignore anything OBVIOUSLY not a killsheet  
  file.list <- list.files()
  file.list <- file.list[grepl("AUR", file.list)]
  
  #paste all killsheet together, treating i = 1 as special because of course
  for(i in 1:length(file.list)){
    ifelse(i == 1, 
           ongoing.file <- read.csv(file.list[1]),
           ongoing.file <- rbind(ongoing.file, read.csv(file.list[i])))
  }

  #setwd to standard consolidated location for ezaccess
  setwd("L:/Sample Records/Performance Food Group/Kill Sheets/Aurora/2018/consolidated killsheets")
  
  #standard naming section, minor tweaks for simplicity
  output <- paste(as.character((Sys.Date())),initials, "consolidated AUR killsheets", sep = "-"); 
  output <- paste(output, ".csv", sep = "")
  write.csv(ongoing.file,file = output, row.names = F)
  
  #reset wd?
  setwd(directory)
}