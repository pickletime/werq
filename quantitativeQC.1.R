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
file.list <- file.list[3:13]
  #new one:
  file.list <- file.list[31]

for(i in 1:length(file.list)){
  df.working.file <- read.csv(file.list[i], sep = ",")
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



#########This is purely for fucking around now
#df.working.file <- read.csv(file.list[2])

df.final <- df.working.file
df.called <- df.final[!df.final$Call %in% uncalled,]


#bulktest
df.called$x.mean <- NA
df.called$y.mean <- NA
df.called$distance <- NA


#these are for testing
df.called$x.norm <- NA
df.called$y.norm <- NA
df.called$distance.norm <- NA

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
  #these are tests
    subset.temp.calls$x.norm <- subset.temp.calls$X/(subset.temp.calls$x.mean/10)
    subset.temp.calls$y.norm <- subset.temp.calls$Y/(subset.temp.calls$y.mean/30)
  #these are tests
    #distance between each sample and the center of each cluster
    subset.temp.calls$distance <- with(subset.temp.calls,distance.func(X,x.mean, Y, y.mean))
  #test
    subset.temp.calls$distance.norm <- with(subset.temp.calls,distance.func(x.norm,x.mean, y.norm, y.mean))
  #test  
    #reading those distances back into the master DF
#test
  df.called[df.called$SNPID == subset.called.snplist[i] & df.called$Call == subset.called.calls[j],10:15] <- subset.temp.calls[,10:15]
#test
  } #determine center for each GT
} #at the end of this you have the table with xmean, ymean, distance columns and my 'normalized' bullshit 


####basic plotting shit
hist(df.called$distance.norm)
hist(df.called$distance)
summary(df.called$distance.norm)

{
# ####
# head(df.called)
# ##end bulktest
# #distance table between clusters - I'm pretty sure this isn't useful either.
# distances.matrix <- matrix(nrow = length(unique(df.called$SNPID)), ncol = length(unique(df.called$Call)),NA)
# rownames(distances.matrix) <- unique(df.called$SNPID)
# colnames(distances.matrix) <- unique(df.called$Call)
# df.distance.table <- with(df.called,tapply(distance, list(SNPID = SNPID, Call = Call), mean))
# df.distance.table <- df.distance.table[,!colnames(df.distance.table) %in% uncalled]
# 
# tester <- df.distance.table[1,!is.na(df.distance.table[1,])]; tester
# genotype.calls <- names(df.distance.table[1,!is.na(df.distance.table[1,])])
# tester.storage <- array(data =NA, dim = c(2,3,length(unique(df.called$SNPID))))
# 

} #misc testing shit i don't even care fuck off

df.master.list <- df.called
#doublecheck that everything is right/makes sense.
#sweeeeet


################################################################################
############select QC file
################################################################################

file.list <- list.files()
file.list <- file.list[a]#n:n




{
  
  
  for(i in 1:length(file.list)){
    df.working.file <- read.csv(file.list[i], sep = ",")
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
  
  
  
  #########This is purely for fucking around now
  #df.working.file <- read.csv(file.list[2])
  
  df.final <- df.working.file
  df.called <- df.final[!df.final$Call %in% uncalled,]
  
  
  #bulktest
  df.called$x.mean <- NA
  df.called$y.mean <- NA
  df.called$distance <- NA
  
  
  #these are for testing
  df.called$x.norm <- NA
  df.called$y.norm <- NA
  df.called$distance.norm <- NA
  
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
      #these are tests
      subset.temp.calls$x.norm <- subset.temp.calls$X/(subset.temp.calls$x.mean/10)
      subset.temp.calls$y.norm <- subset.temp.calls$Y/(subset.temp.calls$y.mean/30)
      #these are tests
      #distance between each sample and the center of each cluster
      subset.temp.calls$distance <- with(subset.temp.calls,distance.func(X,x.mean, Y, y.mean))
      #test
      subset.temp.calls$distance.norm <- with(subset.temp.calls,distance.func(x.norm,x.mean, y.norm, y.mean))
      #test  
      #reading those distances back into the master DF
      #test
      df.called[df.called$SNPID == subset.called.snplist[i] & df.called$Call == subset.called.calls[j],10:15] <- subset.temp.calls[,10:15]
      #test
    } #determine center for each GT
  } #at the end of this you have the table with xmean, ymean, distance columns and my 'normalized' bullshit 
  
}#currently reading in a specific file to be compared to all data. TO CHANGE.

df.comparison <- df.called
subset.df.comparison.snp <- unique(df.comparison$SNPID)
df.master.comparison <- df.master.list[df.master.list$SNPID %in% subset.df.comparison.snp,]
df.master.comparison$SNPID <- factor(df.master.comparison$SNPID,levels = levels(subset.df.comparison.snp))
df.master.comparison$Call <- factor(df.master.comparison$Call, levels = levels(unique(df.comparison$Call)))


comparison.table <- data.frame(SNP = NA, GT = NA, 'p-value' = NA, "QC samples compared" = NA)


for (i in 1:length(subset.df.comparison.snp)){
  #subsetting both frames for each snp
  comparison.subset.temp <- df.comparison[df.comparison$SNPID == subset.df.comparison.snp[i],]
  master.subset.temp <- df.master.comparison[df.master.comparison$SNPID == subset.df.comparison.snp[i],]
  #setting up the subsetting for genotype per snp
  comparison.subset.called.calls <- unique(comparison.subset.temp$Call)
  for(j in 1:length(comparison.subset.called.calls)){
    #temporary dataframes for each genotype
    k <- max(nrow(comparison.table))+1 
    comparison.subset.temp.calls <- comparison.subset.temp[comparison.subset.temp$Call == comparison.subset.called.calls[j],]
    master.comparison.temp.calls <- master.subset.temp[master.subset.temp$Call == comparison.subset.called.calls[j],]
    comparison.table[k,1] <- as.character(subset.df.comparison.snp[i])
    comparison.table[k,2] <- as.character(comparison.subset.called.calls[j])
    comparison.table[k,3] <- (t.test(comparison.subset.temp.calls$distance, master.comparison.temp.calls$distance)[3] < 0.05)
    comparison.table[k,4] <- length(comparison.subset.temp.calls$SubjectID)
  } #write results table for p val and samples compared for each gt for each snp
} #this is the final output - table with pvals and numbers and genotypes OH MY



#Misc shit just for funsies
asdf <- t.test(c(1, 2, 3, 4), c(2, 3, 4))
asdf[1:9]

quantile(comparison.subset.temp$distance, c(.1,.9))

plotter <- df.called[df.called$SNPID == subset.called.snplist[3],]


plot(plotter$Y~plotter$X, col = plotter$Call)
plot(plotter$y.norm~plotter$x.norm, col = plotter$Call)
points(unique(plotter[,10:11]), col = "red", pch = 19, cex = 10)


















###only if i'm playing around
###I'll end up doing something with this, I just don't really know what yet. Maybe I could heatmap based on d for each well?  That sounds fun!
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
