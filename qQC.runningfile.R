############
###### obligatory initializations
############
{
if (!require("plotrix")) {
  install.packages("plotrix", dependencies = TRUE); library(plotrix)}

library(gplots)
#distance formula because i can't be bothered to look up if it already exists
distance.func <- function(x1, x2, y1, y2){sqrt((x2-x1)^2 + (y2-y1)^2)}
#this is fucked I have to be able to drop out the same samples - maybe don't subset within? allow to look at different ranges?
#removes the top and bottom 10% of data from a frame to (hopefully) drop outliers?
quantile.subset.func <- function(df){df[df < quantile(df,0.9) & df > quantile(df,0.1)]}
#    test.func <- function(df){df < quantile(df,0.9) & df > quantile(df,0.1)}
test.func <- function(df){df < quantile(df,0.9)}
uncalled <- c("?", "NTC", "", "Over", "DUPE", "Uncallable")
vector.color <- c("red", "dark green", "blue")
}

############
###### filename to be written
############
filename <- "more testing test"
############
###### filename to be written
############

df.final <- read.csv(file.choose())
df.QC <- read.csv(file.choose())
df.called <- df.final[!df.final$Call %in% uncalled,]

################################################################################
############select Source file
################################################################################
{
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
  print(paste("SNP #",i))
  #subsetting master DF for each snp
  subset.temp <- df.called[df.called$SNPID == subset.called.snplist[i],]
  #setting up subsetting for each GT/snp
  subset.called.calls <- unique(subset.temp$Call)
  for(j in 1:length(subset.called.calls)){
    #print(paste("j is",j))    
    #temporary dataframe for each genotype
    subset.temp.calls <- subset.temp[subset.temp$Call == subset.called.calls[j],]
    #x/y averages because i'm too dumb to do it any other way
    subset.temp.calls$x.mean <- mean(as.numeric(subset.temp.calls$X))
    subset.temp.calls$X <- as.numeric(subset.temp.calls$X)
    subset.temp.calls$y.mean <- mean(as.numeric(subset.temp.calls$Y))
    subset.temp.calls$Y <- as.numeric(subset.temp.calls$Y)
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
} #doing errything for the source file
df.master.list <- df.called



  
################################################################################
############select QC file
################################################################################
{
    df.final <- df.QC
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
      print(paste("SNP #",i))
      #subsetting master DF for each snp
      subset.temp <- df.called[df.called$SNPID == subset.called.snplist[i],]
      #setting up subsetting for each GT/snp
      subset.called.calls <- unique(subset.temp$Call)
      for(j in 1:length(subset.called.calls)){
        #print(paste("j is",j))    
        #temporary dataframe for each genotype
        subset.temp.calls <- subset.temp[subset.temp$Call == subset.called.calls[j],]
        #x/y averages because i'm too dumb to do it any other way
        subset.temp.calls$x.mean <- mean(subset.temp.calls$X)
        subset.temp.calls$y.mean <- mean(subset.temp.calls$Y)
        # #This needs to be done externally, bc it's x/snp max x, not x/max x
        #     subset.temp.calls$x.norm <- subset.temp.calls$X/(subset.temp.calls$x.mean)
        #     subset.temp.calls$y.norm <- subset.temp.calls$Y/(subset.temp.calls$y.mean)
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
      #This needs to be done externally, bc it's x/snp max x, not x/max x
      subset.temp.calls$x.norm <- subset.temp.calls$X/(subset.temp.calls$x.mean)
      subset.temp.calls$y.norm <- subset.temp.calls$Y/(subset.temp.calls$y.mean)
    } #at the end of this you have the table with xmean, ymean, distance columns and my 'normalized' bullshit 
    
  }#doing errything for the QC file
  df.comparison <- df.called
  

######
##trying to subset first, then working with that?
######
  {
   subset.df.comparison.snp <- intersect(df.comparison$SNPID,df.master.list$SNPID)
   df.comparison <- df.comparison[df.comparison$SNPID %in% subset.df.comparison.snp,]
   df.master.comparison <- df.master.list[df.master.list$SNPID %in% subset.df.comparison.snp,]
   #culling snp/call for those not present in both in source file
   df.master.comparison$SNPID <- factor(df.master.comparison$SNPID,levels = subset.df.comparison.snp)
   df.master.comparison$Call <- factor(df.master.comparison$Call, levels = levels(unique(df.comparison$Call)))
   #culling snp/call for those not present in both in the QC file
   df.comparison$SNPID <- factor(df.comparison$SNPID,levels = subset.df.comparison.snp)
   df.comparison$Call <- factor(df.comparison$Call, levels = levels(unique(df.comparison$Call)))
  }
######
##trying to subset first, then working with that?
######

######
##dat maths tho
######
  {
  #####this is where i need to set up the distance calculations
  df.distance.master <- with(df.comparison,tapply(distance, list(SNPID = SNPID, Call = Call), mean))
  df.distance.compar <- with(df.master.comparison,tapply(distance, list(SNPID = SNPID, Call = Call), mean))
  df.distance.master <- df.distance.master[,!colnames(df.distance.master) %in% uncalled]
  df.distance.compar <- df.distance.compar[,!colnames(df.distance.compar) %in% uncalled]
  #shouldn't this be mean?
  #df.distance.diff <- cbind(rowSums(df.distance.master, na.rm = T),rowSums(df.distance.compar, na.rm = T))
  #
  df.distance.diff <- cbind(rowMeans(df.distance.master, na.rm = T), rowMeans(df.distance.compar,na.rm = T ))
  #
  #shouldn't this be mean?
  df.distance <- abs((df.distance.diff[,1] - df.distance.diff[,2]))/df.distance.diff[,1]
  df.distance.quants <- quantile(df.distance,probs = seq(0,1, 0.01), na.rm = T)[2:99]
  #length(df.distance.quants)
  #####this is where i need to set up the distance calculations
  }
  
  hist(df.distance)
  
######
##dat maths tho
######
   
  comparison.table <- data.frame(SNP = NA, GT = NA, 'dGT equal' = NA, "QC samples compared" = NA, "dSNP equal" = NA, "dClust equal" = NA)
  
  
  for (i in 1:length(subset.df.comparison.snp)){
  #for (i in 1:5){
    #subsetting both frames for each snp
    comparison.subset.temp <- df.comparison[df.comparison$SNPID == subset.df.comparison.snp[i],]
    master.subset.temp <- df.master.comparison[df.master.comparison$SNPID == subset.df.comparison.snp[i],]
    #setting up the subsetting for genotype per snp
    comparison.subset.called.calls <- unique(comparison.subset.temp$Call)
    for(j in 1:length(comparison.subset.called.calls)){
      #temporary dataframes for each genotype
      if(i == 1 && j == 1){k <- 1}
      else{k <- max(nrow(comparison.table))+1}
      comparison.subset.temp.calls <- comparison.subset.temp[comparison.subset.temp$Call == comparison.subset.called.calls[j],]
      master.comparison.temp.calls <- master.subset.temp[master.subset.temp$Call == comparison.subset.called.calls[j],]
      comparison.table[k,1] <- as.character(subset.df.comparison.snp[i])
      comparison.table[k,2] <- as.character(comparison.subset.called.calls[j])
      ###adding test for temp.calls$distance OR master.comparison.temp.calls$distance == 0
      if(comparison.subset.temp.calls$distance == 0 || master.comparison.temp.calls$distance == 0) {comparison.table[k,3] <- "-"}
      else {comparison.table[k,3] <- (t.test(comparison.subset.temp.calls$distance, master.comparison.temp.calls$distance, alternative = "greater")[3] > 0.0125)}
      #    else {comparison.table[k,3] <- (t.test(comparison.subset.temp.calls$distance, master.comparison.temp.calls$distance)[3] < 0.05)}
      comparison.table[k,4] <- length(comparison.subset.temp.calls$SubjectID)
      ##this is to make the table clearer
      comparison.table[k,5] <- (t.test(comparison.subset.temp$distance, master.subset.temp$distance, alternative = "greater")[3] > 0.00625)
      comparison.table[k,6] <- df.distance[i] < df.distance.quants[98] & df.distance[i] > df.distance.quants[1]
      ##this is to make the table clearer
    } #write results table for p val and samples compared for each gt for each snp
######PLOTTING
    if(comparison.table[k,5] == FALSE || comparison.table[k,6] == FALSE) {
      par(mar = c(1, 1, 1, 1))
      par(mfrow=c(2,1))
      #plot(master.subset.temp$X~master.subset.temp$Y, main = paste("source for ", subset.df.comparison.snp[i]), col = master.subset.temp$Call, xaxt = 'n')
      plot(master.subset.temp$X~master.subset.temp$Y, main = paste("source for ", subset.df.comparison.snp[i]), 
           col = rep(c("red", "dark green","blue"),ceiling(length(levels(master.subset.temp$Call))/3))[master.subset.temp$Call], xaxt = 'n', pch = 16)
      plot(comparison.subset.temp$X~comparison.subset.temp$Y, main = paste("QC for ", subset.df.comparison.snp[i]), 
           col = rep(c("red", "dark green","blue"),ceiling(length(levels(comparison.subset.temp$Call))/3))[comparison.subset.temp$Call], xaxt = 'n', pch = 16)
    }else{}
######PLOTTING    
    ##adding in overall test/snp
  } #this is the final output - table with pvals and numbers and genotypes OH MY
  print("done")
  
  
  
  # 
  # setwd(choose.dir())
  getwd()
  write.csv(comparison.table, paste(filename,"qQC.csv"))
  
