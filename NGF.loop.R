#pools ngf in target directory, averages, produces some neat figures, saves output.

#i should just functionize this bitch
#next logical steps: walk through directories?


#i'm defaulting here because this is where i'm spitting files out now
setwd("L:/DT/R/NGF")

#pick target file(s) - NEED TO BE UNFORMATTED
git.file <- list.files()
for(i in 1:length(git.file)){
  working.file <- read.csv(git.file[i], sep = ",")
  ifelse(
    i < 2,
    df.working.file <- data.frame(working.file[2:nrow(working.file),]),
    df.working.file <- rbind(df.working.file, data.frame(working.file[2:nrow(working.file),]))
  )
  print(dim(df.working.file)[1])
  print(100*(1-signif(i/length(git.file),3)))
}

#and now the end product
  df.final <- df.working.file

  #various unique lists, just for funsies
  well.list <- sort(unique(df.final$DaughterWell))
  snp.list <- unique(df.final$SNPID) 
  uncalled <- c("?", "NTC", "")
  raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)
  

#  I actually think this works now. No way it's that easy.
  for(i in 1:length(well.list)){
    loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
    raw.output.table[i,1] <- as.character(well.list[i])
    raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
    print(signif(100*(i/length(well.list))),3)
  }
  
  #raw.output.table[,1]
  
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
    #print(100*signif(i/length(unique(data.rows)))) #this isn't useful but i'm scared to delete
  }
  
  data.table[1:2,1:2] <- 0
  
  output.matrix <- 100*matrix(as.numeric(unlist(data.table)),nrow=nrow(data.table))
  
  plate.pass.rate <- heatmap.2(x = output.matrix, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                             cellnote = signif(output.matrix,2), notecol = "black", notecex = 0.5,
                             trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
                             main = length(df.final$DaughterPlate), col = colorRampPalette(c("white", "red"))(150))
  
  par(mar = c(1, 1, 1, 1))
  par(mfrow=c(2,1))
  boxplot(output.matrix, ylab = "average failure rate", xlab = "column index", main = "average failure rate per column", col = "orange", xaxt = 'n')
  boxplot(t(output.matrix), ylab = "average failure rate", xlab = "row index", main = "average failure rate per row", col = "gold", xaxt = 'n')
  
  
  t.test(output.matrix[1:2,], output.matrix[3:16,]); t.test(output.matrix[16,], output.matrix[2:15,])
  t.test(output.matrix[1,], output.matrix[16,])
  length(df.final$DaughterPlate)
  mean(output.matrix)

  # #i could make the grid i'm dreaming of with a two nested for loops?
  # a <- t.test(output.matrix[1:2,])
  # a$p.value
  # str(a)
  
  #save the trimmed/neat version. also: row.names=F is heaven.
  #write.csv(df.final[,-c(9,10)], file = paste("super bulk output", sub(".csv", "", target.file), ".csv", sep = "-"), row.names = F)

