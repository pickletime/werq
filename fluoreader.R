#heatmapping, that's all?

install.packages("gplots")
install.packages("colorRamps")


setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/2") #dublin 3/21
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.20180323/3") #dublin 3/23
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.20180327") #dublin 3/21
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.feb") #dublin feb 18
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/1") #huge archive of files (like 6k)
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/feb") #feb 18 reads
setwd("//us-kraken/kraken/Plate reader archive/2017/09/22/1") #looking at shifting
setwd("//us-kraken/kraken/Plate reader archive/2017/09/28/1") #more shifting
setwd("//us-kraken/kraken/Plate reader archive/2018/04/09/1") #more shifting

setwd("//us-kraken/kraken/Plate reader archive/2018/06/19/1") #more shifting

setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/testing")

max.cutoff.ROX <- 4500; min.cutoff.ROX <- 1500
max.cutoff.FAM <- 15000; min.cutoff.FAM <- 2500
max.cutoff.YY <- 5000; min.cutoff.YY <- 500
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
#weight.ROX.mean <- vector(length = depth)
#weight.ROX.sd <- vector(length = depth)
# ROX.row.mean <- array(NA, dim=c(24, 16))
# ROX.row.sd <- array(NA, dim=c(24, 16))
#FAM
FAM.array <- array(NA, dim=c(16,24, depth))
FAM.mean <- array(NA, dim=c(16, 24))
FAM.sd <- array(NA, dim=c(16, 24))
#weight.FAM.mean <- vector(length = depth)
#weight.FAM.sd <- vector(length = depth)
# FAM.row.mean <- array(NA, dim=c(24, 16))
# FAM.row.sd <- array(NA, dim=c(24, 16))
#YY
YY.array <- array(NA, dim=c(16,24, depth))
YY.mean <- array(NA, dim=c(16, 24))
YY.sd <- array(NA, dim=c(16, 24))
# weight.YY.mean <- vector(length = depth)
# weight.YY.sd <- vector(length = depth)
# YY.row.mean <- array(NA, dim=c(24, 16))
# YY.row.sd <- array(NA, dim=c(24, 16))
#still worthwhile i promise
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

#big loop for pulling out NA in big arrays, mean-ing/sd-ing
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


#ROX.array[is.na(ROX.array[,,1])] <- 1

sum(is.na(ROX.array))/length(ROX.array)
sum(is.na(FAM.array))/length(FAM.array)
sum(is.na(YY.array))/length(YY.array)
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



# #"weighting"
# #???if i do abs(rox.array[,,i] - rox.mean) then sum those i think it'll give me an approximation of what i'm looking for
# for(i in 1:depth){
#   weight.ROX.mean[i] <- as.numeric(sum(abs(ROX.mean - ROX.array[,,i]), na.rm = T))/mean(ROX.mean)
#   #  weight.ROX.sd[i] <- sum(sd(ROX.array[,,i]/sd(ROX.array)))
# }
# test.file <- array(NA, dim=c(depth,3))
# test.file[,2] <- input.vector
# test.file[,3] <- as.numeric(weight.ROX.mean)
# test.file[,1] <- seq(1, depth, by = 1)
# 
# abc <- test.file[order(as.numeric(test.file[,3]), decreasing = T),]
# head(abc)
# 
# ROX.array[,,as.numeric(abc[1,1])]
# #mean(ROX.array[,,as.numeric(abc[1,1])], na.rm = T)

#plotting
#dev.off()
#ROX
par(mar = c(1, 1, 1, 1))
par(mfrow=c(2,1))
boxplot(ROX.sd.norm, ylab = "sd/mean(sd)", xlab = "column index", main = "ROX sd by col", col = "red")
boxplot(ROX.mean.norm, ylab = "avg rox", xlab = "column index", main = "avg ROX by col", col = "red")
#FAM
boxplot(FAM.sd.norm, ylab = "sd/mean(sd)", xlab = "column index", main = "FAM sd by col", col = "green")
boxplot(FAM.mean.norm, ylab = "avg FAM", xlab = "column index", main = "avg FAM by col", col = "green")
#YY
boxplot(YY.sd.norm, ylab = "sd/mean(sd)", xlab = "column index", main = "YY sd by col", col = "blue")
boxplot(YY.mean.norm, ylab = "avg YY", xlab = "column index", main = "avg YY by col", col = "blue")


#adaptive?
par(mar = c(1, 1, 1, 1))
par(mfrow = c(3,1))
cutoffs.ROX <- quantile(ROX.array, probs = seq(0, 1, .002), na.rm = T)
cutoffs.FAM <- quantile(FAM.array, probs = seq(0, 1, .002), na.rm = T)
cutoffs.YY <- quantile(YY.array, probs = seq(0, 1, .002), na.rm = T)
plot(cutoffs.ROX, main = "ROX cutoffs", col = "red")
plot(cutoffs.FAM, main = "FAM cutoffs", col = "green")
plot(cutoffs.YY, main = "YY cutoffs", col = "blue")

head(cutoffs.ROX, 15); tail(cutoffs.ROX, 5); 
min.cutoff.ROX <- cutoffs.ROX[9]; max.cutoff.ROX <- cutoffs.ROX[length(cutoffs.ROX)-2]

head(cutoffs.FAM, 20); tail(cutoffs.FAM, 20)
min.cutoff.FAM <- cutoffs.FAM[3]; max.cutoff.FAM <- cutoffs.FAM[length(cutoffs.FAM)-2]

head(cutoffs.YY, 70); tail(cutoffs.YY, 5)
min.cutoff.YY <- cutoffs.YY[4]; max.cutoff.YY <- cutoffs.YY[length(cutoffs.YY)-3]


#removing NTC fluoro values that somehow aren't being caught by the ranges earlier
for(i in 1:2){
  for(j in 1:2){
    FAM.sd.norm[i,j] <- mean(FAM.sd.norm)
    FAM.mean.norm[i,j] <- mean(FAM.mean.norm)
    YY.sd.norm[i,j] <- mean(YY.sd.norm)
    YY.mean.norm[i,j] <- mean(YY.mean.norm)
  }
}


#ROX heatmapping
par(mar = c(1, 1, 1, 1))
resolution <- 100
# sd.heatmap.ROX <- heatmap.2(x = ROX.sd.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
#                         cellnote = signif(ROX.sd.norm,3), notecol = "black", notecex = 0.5,
#                         trace = "none", key = FALSE, xlab = "Column", ylab = "row",
#                         main = "heatmapping ROX sd", col = colorRampPalette(c("white", "red"))(resolution))

raw.heatmap.ROX <- heatmap.2(x = ROX.mean.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                         cellnote = signif(ROX.mean.norm,3), notecol = "black", notecex = 0.5,
                         trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
                         main = "heatmapping ROX mean", col = colorRampPalette(c("red", "white"))(resolution))
#FAM heatmapping
# sd.heatmap.FAM <- heatmap.2(x = FAM.sd.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
#                         cellnote = signif(FAM.sd.norm,3), notecol = "black", notecex = 0.5,
#                         trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
#                         main = "heatmapping FAM sd", col = colorRampPalette(c("white", "green"))(resolution))

raw.heatmap.FAM <- heatmap.2(x = FAM.mean.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                         cellnote = signif(FAM.mean.norm,3), notecol = "black", notecex = 0.5,
                         trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
                         main = "heatmapping FAM mean", col = colorRampPalette(c("green", "white"))(resolution))
#YY heatmapping
# sd.heatmap.YY <- heatmap.2(x = YY.sd.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
#                         cellnote = signif(YY.sd.norm,3), notecol = "black", notecex = 0.5,
#                         trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
#                         main = "heatmapping YY sd", col = colorRampPalette(c("white", "blue"))(resolution))

raw.heatmap.YY <- heatmap.2(x = YY.mean.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                         cellnote = signif(YY.mean.norm,3), notecol = "black", notecex = 0.5,
                         trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
                         main = "heatmapping YY mean", col = colorRampPalette(c("blue", "white"))(resolution))



dev.off()




index <- 1

#additional funsies
getwd()
readreader(input.vector[1])
rox1 <- ROX.array[,,index]/ROX.mean.norm
fam1 <- (FAM.array[,,index]/FAM.mean.norm)/rox1
yy1 <- (YY.array[,,index]/YY.mean.norm)/rox1

plot(x = fam1, y = yy1)
rox1 - ROX.array[,,1]
