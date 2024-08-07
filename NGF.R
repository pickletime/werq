#I'm ultimately going to try to do something meaningful here. I think data quality across sisterplates is a reasonable start?
#2019-12-17: i just deleted the unused stuff. I forced the trimmed file to be output into the desired directory. 
  #Maybe i'll get the figs to start working now?

NGF <- function(plotbinary = 1){
#necessary lib loading
  if (!require("data.table")) {
    install.packages("data.table", dependencies = TRUE); library(data.table)}
  if (!require("reshape")) {
    install.packages("reshape", dependencies = TRUE); library(reshape)}
  if (!require("tidyr")) {
    install.packages("tidyr", dependencies = TRUE); library(tidyr)}
  if (!require("gplots")) {
    install.packages("gplots", dependencies = TRUE); library(gplots)}
  if (!require("RColorBrewer")) {
    install.packages("RColorBrewer", dependencies = TRUE); library(RColorBrewer)}
  if (!require("png")) {
    install.packages("png", dependencies = TRUE); library(png)}
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }
  right <- function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
  }
  
  target.file <- file.choose()
  directoryextractor.func(target.file)
  

  #pick target file
  target.file.name <- right(sub(".csv", "", target.file), 10)
  #pull in whole file, filling 
  working.file <- read.table(file = target.file, fill = T)
  #start new df from appropriate location
  
  
  if(dim(working.file)[2] == 3){
    working.file <- read.table(file = target.file, fill = T, row.names = NULL) 
  }
  
  
  df.working.file <- data.frame(working.file[(match("Data",working.file[,1])+2):nrow(working.file),1])
  
  #turning off warnings pseudo-locally
  oldw <- getOption("warn")
  options(warn = -1)
  
  #reformat df with appropriate headers
  df.final <- separate(data = df.working.file, col = 1, sep = "\\,",
                       into = c("DaughterPlate","MasterPlate","MasterWell","Call","X","Y","SNPID","SubjectID",
                                "Norm","Carrier","DaughterWell"))
    #new kraken has another column (AliquotID) but it doesn't seem to be used?
  well.list <- sort(unique(df.final$DaughterWell))
  #snp.list <- unique(df.final$SNPID)   #this isn't used, but i sort of want it anyway
  #this is wrong. i need to correct this for sure
  uncalled <- c("?", "NTC", "")
  
  #turning warnings back on:
  options(warn = oldw)
  
  raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)
  
  
  #how to extract #matching whatever
  #I actually think this works now. No way it's that easy.
  for(i in 1:length(well.list)){
    loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
    raw.output.table[i,1] <-well.list[i]
    raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
  }
  
  
  #this one is fine
  data.cols <- as.numeric(substr(raw.output.table[,1],2,3))
  #this one is not fine i just need to figure this out
  #edit: this one is now fine. I have figured it out.
  data.rows <- substr(raw.output.table[,1],1,1)
  fac <- factor(data.rows); levels(fac) <- 1:16; data.rows <- as.numeric(fac)
  #and of course
  raw.output.table[,2] <- as.numeric(raw.output.table[,2])
  
    
  #once the data.rows is fine we'll be good to go
  #edit: is not gee too gee
  data.table <- matrix(data = 0, nrow = length(unique(data.rows)), ncol = length(unique(data.cols)))
  
  #pushing rows/cols into output df
  raw.output.table[,3] <- data.rows
  raw.output.table[,4] <- data.cols
  
  #this took entirely too long to figure out
  for(i in 1:length(unique(data.rows))){
    subset.output.table <- raw.output.table[raw.output.table[,3] == i,]
    for(j in 1:length(unique(data.cols))){
      ifelse(length(subset.output.table[subset.output.table[,4] == j,2]) >0 , 
             data.table[i,j] <- subset.output.table[subset.output.table[,4] == j,2], 0)
    }
  }
  
  data.table[1:2,1:2] <- 0
  output.matrix <- 100*matrix(as.numeric(unlist(data.table)),nrow=nrow(data.table))
  
  if(plotbinary == 1){
    #plotting shit
    par(mar = c(1, 1, 1, 1))
     plate.pass.rate <- heatmap.2(x = output.matrix, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                                  cellnote = signif(output.matrix,2), notecol = "black", notecex = 0.5,
                                  trace = "none", key = FALSE, xlab = "Column", ylab = "row",
                                  main = target.file, col = colorRampPalette(c("white", "red"))(100))
    t.test(output.matrix[1,], output.matrix[2:15,]); t.test(output.matrix[16,], output.matrix[2:15,])
    t.test(output.matrix[1,], output.matrix[16,])

    par(mfrow=c(2,1))
    boxplot1 <- boxplot(output.matrix, ylab = "average failure rate", xlab = "column index", main = "average failure rate per column", col = "orange", xaxt = 'n')
    boxplot2 <- boxplot(t(output.matrix), ylab = "average failure rate", xlab = "row index", main = "average failure rate per row", col = "gold", xaxt = 'n')
    
    # i'm trying to get it to print the figures because they won't consistently display
    # oldwd <- getwd(); setwd("L:/DT/R/NGF.figures")
    # png(filename = paste())
    # 
    # paste(sub("Genotyping", "Row boxplot", target.file.name),sep = "",
    # setwd(oldwd)
    # dev.off()
  }
  
  #save the trimmed/neat version. also: row.names=F is heaven.
  output <- "L:/DT/R/NGF/"
  write.csv(df.final, file = paste(output, target.file.name, "Trimmed NGF", sep = " ", ".csv"), row.names = F)
  print("NGF file trimmed, placed in relevant directory thx GLHF")
}


