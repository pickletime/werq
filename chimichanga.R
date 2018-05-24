#plots everything, no heatmap, quantiles, plotting + kmeans
#this one is smarter than fluoro, but lacks the heatmapping

install.packages("gplots")
install.packages("colorRamps")

library(gplots)
library(colorRamps)

setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/2") #dublin 3/21
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.20180323/3") #dublin 3/23
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.20180327") #dublin 3/21
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.feb") #dublin feb 18
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/1") #huge archive of files (like 6k)
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/feb") #feb 18 reads
setwd("//us-kraken/kraken/Plate reader archive/2017/09/22/1") #looking at shifting
setwd("//us-kraken/kraken/Plate reader archive/2017/09/28/1") #more shifting
setwd("//us-kraken/kraken/Plate reader archive/2018/04/09/1") #more shifting

setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/testing")

max.cutoff.ROX <- 150000; min.cutoff.ROX <- 500
max.cutoff.FAM <- 150000; min.cutoff.FAM <- 500
max.cutoff.YY <- 150000; min.cutoff.YY <- 500
library(colorRamps)
library(gplots)
#initializing

test.file <- list.files()
input.vector <- test.file
depth <- length(input.vector)

#ROX
ROX.array <- array(NA, dim=c(16,24, depth))
ROX.mean <- array(NA, dim=c(16, 24))
ROX.sd <- array(NA, dim=c(16, 24))
#FAM
FAM.array <- array(NA, dim=c(16,24, depth))
FAM.mean <- array(NA, dim=c(16, 24))
FAM.sd <- array(NA, dim=c(16, 24))
#YY
YY.array <- array(NA, dim=c(16,24, depth))
YY.mean <- array(NA, dim=c(16, 24))
YY.sd <- array(NA, dim=c(16, 24))

par(mar = c(1, 1, 1, 1))





#Reading the read files in
for(i in 1:depth){
  temp.ROX <- read.csv(input.vector[i], skip = 45, nrows = 16)
  temp.FAM <- read.csv(input.vector[i], skip = 7, nrows = 16)
  temp.YY <- read.csv(input.vector[i], skip = 26, nrows = 16)
  #walking through arrays
  for(j in 1:24){
    ifelse(j == 1 & temp.ROX[,1] == LETTERS[seq(1,16)],
           ROX.array[,1,i] <- temp.ROX[,2], ROX.array[,j,i] <- temp.ROX[,j]
    )
    ifelse(j == 1 & temp.FAM[,1] == LETTERS[seq(1,16)],       
           FAM.array[,1,i] <- temp.FAM[,2], FAM.array[,j,i] <- temp.FAM[,j]
    )
    ifelse(j == 1 & temp.YY[,1] == LETTERS[seq(1,16)],       
           YY.array[,1,i] <- temp.YY[,2], YY.array[,j,i] <- temp.YY[,j]
    )
    #walking through columns
    #replace with NA if outside of informed range - replace with mean[k,j,]
    for(k in 1:16){
      if(ROX.array[k,j,i] < min.cutoff.ROX | ROX.array[k,j,i] > max.cutoff.ROX){ROX.array[k,j,i] <- NA}
      if(FAM.array[k,j,i] < min.cutoff.FAM | FAM.array[k,j,i] > max.cutoff.FAM){FAM.array[k,j,i] <- NA}
      if(YY.array[k,j,i] < min.cutoff.YY | YY.array[k,j,i] > max.cutoff.YY){YY.array[k,j,i] <- NA}
    }
  }
  if(depth > 1){print(100*(1-((length(input.vector) - i)/length(input.vector))))}
}

#big loop mean-ing/sd-ing
for(i in 1:16){ #rows
  for(j in 1:24){ #cols
    #ROX
    ROX.mean[i,j] <- mean(ROX.array[i,j,], na.rm = T)
    ROX.sd[i,j] <- sd(ROX.array[i,j,], na.rm = T)
    #FAM
    FAM.mean[i,j] <- mean(FAM.array[i,j,], na.rm = T)
    FAM.sd[i,j] <- sd(FAM.array[i,j,], na.rm = T)
    #YY
    YY.mean[i,j] <- mean(YY.array[i,j,], na.rm = T)
    YY.sd[i,j] <- sd(YY.array[i,j,], na.rm = T)
  }
}

#REPLACING NA IT'S FINALLY HAPPENED I'M AMAZING
for(i in 1:length(input.vector)){
  for(j in 1:24){
    for(k in 1:16){
      if(is.na(ROX.array[k,j,i])){ROX.array[k,j,i] <- ROX.mean[k,j]}
      if(is.na(FAM.array[k,j,i])){FAM.array[k,j,i] <- FAM.mean[k,j]}
      if(is.na(YY.array[k,j,i])){YY.array[k,j,i] <- YY.mean[k,j]}
    }
  }
}

sum(is.na(ROX.array))
sum(is.na(FAM.array))
sum(is.na(YY.array))
# length(ROX.array)


#plotting value distributions
par(mar = c(1, 1, 1, 1))
par(mfrow=c(1,3))
dist <- max(ROX.array, na.rm = T)-min(ROX.array, na.rm = T)
rox.min <- min(ROX.array, na.rm = T); rox.min
rox.max <- max(ROX.array, na.rm = T); rox.max
hist(ROX.array, freq=FALSE, breaks = seq(rox.min, rox.max, by = dist/200), main = "ROX distribution", col = "red")

dist <- max(FAM.array, na.rm = T)-min(FAM.array, na.rm = T)
FAM.min <- min(FAM.array, na.rm = T); FAM.min
FAM.max <- max(FAM.array, na.rm = T); FAM.max
hist(FAM.array, freq=FALSE, breaks = seq(FAM.min, FAM.max, by = dist/200), main = "FAM distribution", col = "green")

dist <- max(YY.array, na.rm = T)-min(YY.array, na.rm = T)
YY.min <- min(YY.array, na.rm = T); YY.min
YY.max <- max(YY.array, na.rm = T); YY.max
hist(YY.array, freq=FALSE, breaks = seq(YY.min, YY.max, by = dist/200), main = "YY distribution", col = "blue")




#ROX normalization
ROX.sd.norm <- ROX.sd/mean(ROX.sd); ROX.mean.norm <- ROX.mean/mean(ROX.mean); ROX.sd.min <- min(ROX.sd); ROX.sd.max <- max(ROX.sd)

#FAM normalization
FAM.sd.norm <- FAM.sd/mean(FAM.sd); FAM.mean.norm <- FAM.mean/mean(FAM.mean); FAM.sd.min <- min(FAM.sd); FAM.sd.max <- max(FAM.sd)

#YY normalization
YY.sd.norm <- YY.sd/mean(YY.sd); YY.mean.norm <- YY.mean/mean(YY.mean); YY.sd.min <- min(YY.sd); YY.sd.max <- max(YY.sd)



par(mar = c(1, 1, 1, 1))
par(mfrow=c(3,1))
#ROX
boxplot(ROX.mean.norm, ylab = "avg rox", xlab = "column index", main = "avg ROX by col", col = "red")
#FAM
boxplot(FAM.mean.norm, ylab = "avg FAM", xlab = "column index", main = "avg FAM by col", col = "green")
#YY
boxplot(YY.mean.norm, ylab = "avg YY", xlab = "column index", main = "avg YY by col", col = "blue")





#adaptive?
par(mar = c(1, 1, 1, 1))
par(mfrow = c(3,1))
cutoffs.ROX <- quantile(ROX.array, probs = seq(0, 1, .01), na.rm = T)
cutoffs.FAM <- quantile(FAM.array, probs = seq(0, 1, .01), na.rm = T)
cutoffs.YY <- quantile(YY.array, probs = seq(0, 1, .01), na.rm = T)
plot(cutoffs.ROX, main = "ROX cutoffs", col = "red")
plot(cutoffs.FAM, main = "FAM cutoffs", col = "green")
plot(cutoffs.YY, main = "YY cutoffs", col = "blue")

head(cutoffs.ROX, 10); tail(cutoffs.ROX, 10); 
min.cutoff.ROX <- cutoffs.ROX[4]; max.cutoff.ROX <- cutoffs.ROX[length(cutoffs.ROX)-3]

head(cutoffs.FAM, 10); tail(cutoffs.FAM, 10)
min.cutoff.FAM <- cutoffs.FAM[3]; max.cutoff.FAM <- cutoffs.FAM[length(cutoffs.FAM)-2]

head(cutoffs.YY, 10); tail(cutoffs.YY, 10)
min.cutoff.YY <- cutoffs.YY[3]; max.cutoff.YY <- cutoffs.YY[length(cutoffs.YY)-1]

#removing NTC fluoro values that somehow aren't being caught by the ranges earlier
for(i in 1:2){
  for(j in 1:2){
    FAM.sd.norm[i,j] <- mean(FAM.sd.norm)
    FAM.mean.norm[i,j] <- mean(FAM.mean.norm)
    YY.sd.norm[i,j] <- mean(YY.sd.norm)
    YY.mean.norm[i,j] <- mean(YY.mean.norm)
  }
}

##building universal averages:
  

###########additional funsies




for(j in 1:20){
  #walk through huge array by [,,j]
  format.FAM <- as.data.frame(FAM.array[,,j])
  format.YY <- as.data.frame(YY.array[,,j])
  format.ROX <- as.data.frame(ROX.array[,,j])
  
  #comparison, stack
  formatted.X.uncorr <- stack(format.FAM/format.ROX)
  formatted.Y.uncorr <- stack(format.YY/format.ROX)
  
  #format x & y, normalize, stack
  formatted.X <- stack(format.FAM/(format.ROX/ROX.mean.norm))
  formatted.Y <- stack(format.YY/(format.ROX/ROX.mean.norm))
  
  #universal averages, stack:
  # avg.X <- stack(as.data.frame(FAM.mean/ROX.mean))
  # avg.Y <- stack(as.data.frame(YY.mean/ROX.mean))
  # avg <- cbind(avg.X[1:384, 1], avg.Y[1:384, 1])
  # plot(avg)
  # avages <- kmeans()
  
    
  to.plot <- cbind(formatted.X[1:384,1],formatted.Y[1:384,1])
  to.plot.uncorrected <- cbind(formatted.X.uncorr[1:384,1], formatted.Y.uncorr[1:384,1])
  
  # I won't give up on this plotting kmeans thing not now not ever
  par(mar = c(1, 1, 1, 1))
  par(mfrow = c(5,2))
  for(i in 1:5){
    cents <- i
    abc <- kmeans(to.plot, centers = cents, iter.max = 100000, nstart = 1000)
    print(i); print(abc$betweenss/cents)
    plot(to.plot, ylab = "", xlab = "", main = paste0(i, "c"), col = abc$cluster)
    plot(to.plot.uncorrected, ylab = "", xlab = "",main = paste0(i, "uc"), col = abc$cluster)
  }
  print(j)
}
#plot(final, col = abc$cluster)











getwd()
readreader(input.vector[1])
rox1 <- ROX.array[,,index]/ROX.mean.norm
fam1 <- (FAM.array[,,index]/FAM.mean.norm)/rox1
yy1 <- (YY.array[,,index]/YY.mean.norm)/rox1

plot(x = fam1, y = yy1)
rox1 - ROX.array[,,1]
