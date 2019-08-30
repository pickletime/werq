#pools ngf in target directory, averages, produces some neat figures, saves output.

#i should just functionize this bitch
#next logical steps: walk through directories?


#i'm defaulting here because this is where i'm spitting files out now
setwd("C:/Users/DYLAN/Desktop/WERQ/R/NGF")
  new.wd <- choose.dir()
setwd(new.wd); getwd()

library(gplots)
distance.func <- function(x1, x2, y1, y2){sqrt((x2-x1)^2 + (y2-y1)^2)}

#pick target file(s) - NEED TO BE UNFORMATTED
file.list <- list.files()
for(i in 1:length(file.list)){
  working.file <- read.csv(file.list[i], sep = ",")
  ifelse(
    i < 2,
    df.working.file <- data.frame(working.file[2:nrow(working.file),]),
    df.working.file <- rbind(df.working.file, data.frame(working.file[2:nrow(working.file),]))
  )
  print(dim(df.working.file)[1])
  print(100*(1-signif(i/length(file.list),3)))
} #reading in files in relevant location


###if i'm looking to fuck around just target the first file?

#This has to exist - at some point i'll probably add more. For the time being this is fine.
uncalled <- c("?", "NTC", "", "Over", "DUPE", "Uncallable")

#This is purely for fucking around now
df.working.file <- read.csv(file.list[2])
df.working.file.called <- df.working.file[!df.working.file$Call %in% uncalled,]
unique(df.working.file.called$Call); unique(df.working.file$Call)

df.final <- df.working.file
#Below line is for removing a bunch of shit
#df.called <- df.final[!df.final$Call %in% uncalled,-c(1,2,3,8,9)]

df.called <- df.final[!df.final$Call %in% uncalled,]
unique(df.working.file.called$Call)
# df.called.2 <- df.called[1:400,]
# df.called.2$x.mean <- NA
# df.called.2$y.mean <- NA
# df.called.2$distance <- NA
head(df.working.file.called)
head(master.snp.df)

#this is overwhelmingly the most constructive
# df.called.2.snplist <- unique(df.called.2$SNPID)
# temp <- df.called.2[df.called.2$SNPID == df.called.2.snplist[1],]
# df.called.2.calls <- unique(temp$Call)
# temp <- df.called.2[df.called.2$Call == df.called.2.calls[3],]
# temp$x.mean <- mean(temp$X)
# temp$y.mean <- mean(temp$Y)
# temp$distance <- with(temp,distance.func(X,x.mean, Y, y.mean))
# #HOLY SHIT IT WORKS I'M A GENIUS
# df.called.2[df.called.2$SNPID == df.called.2.snplist[1] & df.called.2$Call == df.called.2.calls[3],5:6] <- temp[,5:6]

#bulktest
        ##testing
          
        ##testing
df.called$x.mean <- NA
df.called$y.mean <- NA
df.called$distance <- NA
subset.called.snplist <- unique(df.called$SNPID)
for (i in 1:length(subset.called.snplist)){
  print(paste("i is",i))
  #subsetting master DF for each snp
  subset.temp <- df.called[df.called$SNPID == subset.called.snplist[i],]
  #setting up subsetting for each GT/snp
  subset.called.calls <- unique(subset.temp$Call)
  for(j in 1:length(subset.called.calls)){
    print(paste("j is",j))    
    #temporary dataframe for each genotype
    subset.temp.calls <- subset.temp[subset.temp$Call == subset.called.calls[j],]
    #x/y averages because i'm too dumb to do it any other way
    subset.temp.calls$x.mean <- mean(subset.temp.calls$X)
    subset.temp.calls$y.mean <- mean(subset.temp.calls$Y)
    #distance between each sample and the center of each cluster
    subset.temp.calls$distance <- with(subset.temp.calls,distance.func(X,x.mean, Y, y.mean))
    #reading those distances back into the master DF
  #below line is for removing a subset of columns. I think that's a bad idea bc you can never have too much data?
    #df.called[df.called$SNPID == subset.called.snplist[i] & df.called$Call == subset.called.calls[j],5:7] <- subset.temp.calls[,5:7]
    df.called[df.called$SNPID == subset.called.snplist[i] & df.called$Call == subset.called.calls[j],10:12] <- subset.temp.calls[,10:12]
  } #determine center for each GT
}
##end bulktest
#distance table between clusters
distances.matrix <- matrix(nrow = length(unique(df.called$SNPID)), ncol = length(unique(df.called$Call)),NA)
rownames(distances.matrix) <- unique(df.called$SNPID)
colnames(distances.matrix) <- unique(df.called$Call)
df.distance.table <- with(df.called,tapply(distance, list(SNPID = SNPID, Call = Call), mean))
df.distance.table <- df.distance.table[,!colnames(df.distance.table) %in% uncalled]

tester <- df.distance.table[1,!is.na(df.distance.table[1,])]; tester
genotype.calls <- names(df.distance.table[1,!is.na(df.distance.table[1,])])
tester.storage <- array(data =NA, dim = c(2,3,length(unique(df.called$SNPID))))

for(i in 1:2){
#  rownames(tester.storage)[i] <- genotype.calls[i]
  for(j in 1:3){
    tester.storage[i,j,1] <- abs(tester[i] - tester[j])
#    colnames(tester.storage)[j] <- genotype.calls[j]
  }
}
rownames(tester.storage) <- genotype.calls[1:2]
colnames(tester.storage) <- genotype.calls[1:3]
tester.storage[,,1]

#to keep
master.snp.df <- df.called
##to keep




###only if i'm playing around

#and now the end product
  df.final <- df.working.file

  #various unique lists, just for funsies
  well.list <- sort(unique(df.final$DaughterWell))
  snp.list <- unique(df.final$SNPID) 
  uncalled <- c("?", "NTC", "", "Over")
  raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)
  

#  I actually think this works now. No way it's that easy.
  for(i in 1:length(well.list)){
    loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
    raw.output.table[i,1] <- as.character(well.list[i])
    raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
    print(signif(100*(i/length(well.list))),3)
  } #averaging call rate across well location
  
  #raw.output.table[,1]
  
  #this one is fine
  data.cols <- as.numeric(substr(raw.output.table[,1],2,3))
  #this one is not fine i just need to figure this out
  #I think this one is fine now?
  data.rows <- substr(raw.output.table[,1],1,1)
  fac <- factor(data.rows); levels(fac) <- 1:16; data.rows <- as.numeric(fac)
  #and of course
  raw.output.table[,2] <- as.numeric(raw.output.table[,2])
  
  
  #once the data.rows is fine we'll be good to go
  #this is now good to go?
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
  } #building the average table?
  
  data.table[1:2,1:2] <- NA
  
  #mean(data.table)
  
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
  
  library(Hmisc)
  
  abc <- rcorr(output.matrix)
  #abc <- matrix(as.numeric(unlist(abc)),nrow=nrow(abc))

  #heatmap.2(x = abc)
  
  # #i could make the grid i'm dreaming of with a two nested for loops?
  # a <- t.test(output.matrix[1:2,])
  # a$p.value
  # str(a)
  
  #save the trimmed/neat version. also: row.names=F is heaven.
  
  #write.csv(df.final[,-c(9,10)], file = paste("super bulk output", ".csv"), row.names = F)
  #I have no idea why I was using this. Col 9 is the sisterplate well location, which is absolutely necessary.
  write.csv(df.final, file = paste("super bulk output of ",length(df.final$SubjectID), " compiled samples.csv"), row.names = F)
