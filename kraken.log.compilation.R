file <- "K:/Log/Event 2018-01"
setwd(file)
abc <- list.files()
abc <- abc[1]
DONKEYBUTT <- read.table(abc, col.names = c(seq(1:30)), fill = TRUE)
fread(abc, sep = " ")
m <- as.data.frame(matrix(0, ncol = 30, nrow = 2))


#grepl for list of folder names, use length as first for loop

#output file at the end
setwd(desired[2]); getwd()
setwd(file); getwd()
file.selection <- list.files()
for(i in 1:length(file.selection)){
#  monthly.file <- as.data.frame("null", col.names = c("1", "2", "3"))
#  cba <- rbind(fread(file.selection[1], header = F, sep = "auto"), cba)
#  if(i==1){cba <- fread(file.selection[i], header = F, sep = "auto", fill = TRUE)}
#  cba <- rbind(fread(file.selection[i], header = F, sep = "auto", fill = TRUE), cba)
  if(i==1){cba <- read.table(file.selection[i], col.names = c(seq(1:30)), fill = TRUE)}
  cba <- rbind(read.table(file.selection[i], col.names = c(seq(1:30)), fill = TRUE), cba)
 
   print(file.selection[i])
}

write.csv(cba,file = "apples.txt", row.names = F)
#DONE
#second for loop walks through files





originalwd <- "K:/Log"
setwd(originalwd); getwd()
test <- list.files()
donkey <- grepl("Event ", test)
desired <- test[donkey]
seqlength <- 158
getwd()
setwd(desired[5]); getwd()
setwd(originalwd); getwd()
for (i in 1:length(desired)){
  setwd(desired[i])
  file.selection <- list.files()
#  j <- 0
  for(j in 1:length(file.selection)){
    if(j==1){daily <- read.table(file.selection[j], col.names = c(seq(1:seqlength)), fill = TRUE)}
    if(j!=1){daily <- rbind(read.table(file.selection[j], col.names = c(seq(1:seqlength)), fill = TRUE), daily)}
#    print(file.selection[j])
#    print(length(file.selection)-j);
  }
  if(i==1){monthly <- daily}
  if(i!=1){monthly <- rbind(monthly, daily)}
  setwd(originalwd)
#  print(desired[i])
#  print(length(desired)-i) 
  print(paste("Month folders remain:", length(desired)-i,"New rows added: ", nrow(daily), "Total rows: ", nrow(monthly), desired[i]))
}
setwd(originalwd); getwd()


write.csv(monthly,file = "apples.txt", row.names = F); list.files()


for(j in 1:length(desired)){
  setwd(desired[j])
  #print(j)
  setwd(originalwd)
}