NGF.basic <- function(name = "pls fill in", plot = F){
  if (!require("dplyr")) {
    install.packages("dplyr", dependencies = TRUE); library(dplyr)}
  if (!require("tidyr")) {
    install.packages("tidyr", dependencies = TRUE); library(tidyr)}
  if (!require("gplots")) {
    install.packages("gplots", dependencies = TRUE); library(gplots)}
  if (!require("colorRamps")) {
    install.packages("colorRamps", dependencies = TRUE); library(colorRamps)}

  ####
  
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
}
  
  ###
  
  target.file <- file.choose()
  target.wd <- directoryextractor.func(target.file)
  
  ##
  
  working.file <- read.table(target.file, fill = T, row.names = NULL)
  working.file.a <- data.frame(working.file[(match("Data",working.file[,1])+2):nrow(working.file),1]) 
  ##find a way to turn off that warning here because IDGAF
  df.temp <- separate(data = working.file.a, col = 1, sep = "\\,",
                      into = c("DaughterPlate","MasterPlate","MasterWell","Call","X","Y","SNPID","SubjectID",
                               "Norm","Carrier","DaughterWell"))
  df.temp.a <- df.temp[!is.na(df.temp[,11]),]
  
  df.working.file <- df.temp.a
  df.final <- df.working.file
  
  
  
  
  ###plotting shit
#  if(plot == T){ that's fucking nuts why would i not include this shit?
    
    #new kraken has another column (AliquotID) but it doesn't seem to be used?
    well.list <- sort(unique(df.final$DaughterWell))
    snp.list <- unique(df.final$SNPID)
    plate.list <- unique(df.final$MasterPlate)
    #this is wrong. i need to correct this for sure
    uncalled <- c("?", "NTC", "", "Over", "DUPE", "Uncallable")
    raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)
    
    #idkwtf this wasn't part originally
    plate.table <- data.frame(matrix(NA, nrow = length(plate.list), ncol = 3))
    colnames(plate.table) <- c("plate BC", "% failed", "average distance")
    plate.table$`plate BC` <- plate.list
    
    
    ######
    ######QQC
    ######
    {
      df.called <- df.final[!df.final$Call %in% uncalled,]
      df.called$x.mean <- NA
      df.called$y.mean <- NA
      df.called$distance <- NA
      
      
      #these are for testing
      df.called$x.norm <- NA
      df.called$y.norm <- NA
      df.called$distance.norm <- NA
      
      subset.called.snplist <- unique(df.called$SNPID)
      
      for (i in 1:length(subset.called.snplist)){
        print(paste("SNP #",i))
        #subsetting master DF for each snp
        subset.temp <- df.called[df.called$SNPID == subset.called.snplist[i],]
        #setting up subsetting for each GT/snp
        subset.called.calls <- unique(subset.temp$Call)
        for(j in 1:length(subset.called.calls)){
          #print(paste("j is",j))    
          #temporary dataframe for each genotype
          subset.temp.calls <- subset.temp[subset.temp$Call == subset.called.calls[j],]
          #x/y averages because i'm too dumb to do it any other way, the forced as numeric thing is sure something
          subset.temp.calls$x.mean <- mean(as.numeric(subset.temp.calls$X))
          subset.temp.calls$X <- as.numeric(subset.temp.calls$X)
          subset.temp.calls$y.mean <- mean(as.numeric(subset.temp.calls$Y))
          subset.temp.calls$Y <- as.numeric(subset.temp.calls$Y)
          subset.temp.calls$x.norm <- subset.temp.calls$X/(subset.temp.calls$x.mean/10)
          subset.temp.calls$y.norm <- subset.temp.calls$Y/(subset.temp.calls$y.mean/30)
          #distance between each sample and the center of each cluster
          subset.temp.calls$distance <- with(subset.temp.calls,distance.func(X,x.mean, Y, y.mean))
          subset.temp.calls$distance.norm <- with(subset.temp.calls,distance.func(x.norm,x.mean, y.norm, y.mean))
          
          #reading those distances back into the master DF
          #test
          df.called[df.called$SNPID == subset.called.snplist[i] & df.called$Call == subset.called.calls[j],10:17] <- subset.temp.calls[,10:17]
          #test
        } #determine center for each GT
      } #at the end of this you have the table with xmean, ymean, distance columns and my 'normalized' bullshit 
      #doing errything for the source file
      df.master.list <- df.called
    }
    ######
    ######QQC
    ######
    
    
    #I actually think this works now. No way it's that easy.
    for(i in 1:length(well.list)){
      loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
      raw.output.table[i,1] <-well.list[i]
      raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
    }
    for(i in 1:length(plate.list)){
      loop.df.subset <- df.final[df.final$MasterPlate == plate.list[i],]
      loop.df.subset.qqc <- df.master.list[df.master.list$MasterPlate == plate.list[i],]
      plate.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$MasterPlate))
      plate.table[i,3] <- as.numeric(mean(loop.df.subset.qqc$distance))
    }
      
    
    

  
    
    #this one is fine
    data.cols <- as.numeric(substr(raw.output.table[,1],2,3))
    #this one is not fine i just need to figure this out
    data.rows <- substr(raw.output.table[,1],1,1)
    fac <- factor(data.rows); levels(fac) <- 1:16; data.rows <- as.numeric(fac)
    #and of course
    raw.output.table[,2] <- as.numeric(raw.output.table[,2])
    
    
    #once the data.rows is fine we'll be good to go
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
    
    #data.table[1:2,1:2] <- 0
      #i hate that this has to be here, but it does because FUCK EVERYTHING
    output.matrix <- 100*matrix(as.numeric(unlist(data.table)),nrow=nrow(data.table))
    
    plate.pass.rate <- heatmap.2(x = output.matrix, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                                 cellnote = signif(output.matrix,3), notecol = "black", notecex = 0.5,
                                 trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
                                 main = paste("N = ",length(df.final$DaughterPlate)), col = colorRampPalette(c("yellow", "blue"))(150))
    
    par(mar = c(1, 1, 1, 1))
    par(mfrow=c(2,1))
    boxplot(output.matrix, ylab = "average failure rate", xlab = "column index", main = "average failure rate per column", col = "orange", xaxt = 'n')
    boxplot(t(output.matrix), ylab = "average failure rate", xlab = "row index", main = "average failure rate per row", col = "gold", xaxt = 'n')
  }
  
  
  
###saving the file(s)
  setwd(target.wd)
  proj.num <- unlist(strsplit(working.file[6,2],","))[2]
  
  
  ###IDK why the fuck this isn't working. it seems to be having issues with BOTH row.names = F and the long filename
  long.name <- c(paste(proj.num, ncol(df.final), "rows", ".csv", sep = " "))
  long.table.name <- c(paste(proj.num, "table list", ".csv", sep = " "))
  
  #write.csv(x = df.final, file = long.name, row.names = F); Sys.Date()
  write.csv(df.final, file = paste(long.name, ".csv"))
  write.csv(plate.table, file =long.table.name, row.names = F)
  


}