install.packages("gplots")
install.packages("colorRamps")
getwd()



#setwd("//us-kraken/kraken/Plate reader archive/2018/03/21/1")
#setwd("//us-kraken/kraken/Plate reader archive/2018/03/19/1")
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/2") #dublin 3/21
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.20180323/3") #dublin 3/23
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.20180327") #dublin 3/21
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/dub.feb") #dublin feb 18
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/1") #huge archive of files (like 6k)
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/feb") #feb 18 reads
setwd("//us-kraken/kraken/Plate reader archive/2017/09/22/1") #looking at shifting
setwd("//us-kraken/kraken/Plate reader archive/2017/09/28/1") #more shifting
setwd("//us-kraken/kraken/Plate reader archive/2018/04/03/1") #more shifting

#new set:
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/13/1") #dublin 3/13
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/14/1") #dublin 3/14
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/15/1") #dublin 3/15
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/16/1") #dublin 3/16
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/20/1") #dublin 3/20
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/21/1") #dublin 3/21
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/22/1") #dublin 3/22
#setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader/badates/26/1") #dublin 3/26


max.cutoff <- 100000
min.cutoff <- 10


test.file <- list.files()
input.vector <- test.file
depth <- length(input.vector)
ROX.array <- array(NA, dim=c(16,24, depth))
ROX.mean <- array(NA, dim=c(16, 24))
ROX.sd <- array(NA, dim=c(16, 24))
weight.ROX.mean <- vector(length = depth)
weight.ROX.sd <- vector(length = depth)
ROX.row.mean <- array(NA, dim=c(24, 16))
ROX.row.sd <- array(NA, dim=c(24, 16))
read.max <- max.cutoff #15000
read.min <- min.cutoff #1000
library(colorRamps)
library(gplots)
#dev.off()



#Reading the read files in
for(i in 1:depth){
  temp <- read.csv(input.vector[i], skip = 45, nrows = 16)
  #walking through arrays
  for(j in 1:24){
    ifelse(j == 1 & temp[,1] == LETTERS[seq(1,16)],
           ROX.array[,1,i] <- temp[,2], 
           ROX.array[,j,i] <- temp[,j]
           )
    #walking through columns
    for(k in 1:16){
      if(ROX.array[k,j,i] < read.min | ROX.array[k,j,i] > read.max){ROX.array[k,j,i] <- NA}
    }
    #replace with NA if outside of well-informed measures
  }
  if(depth > 1){print((length(input.vector) - i)/length(input.vector))}
}
#mean/sd looping:
for(i in 1:16){ #rows
  for(j in 1:24){ #cols
    ROX.mean[i,j] <- mean(ROX.array[i,j,], na.rm = T)
    ROX.sd[i,j] <- sd(ROX.array[i,j,], na.rm = T)
  }
}

sum(is.na(ROX.array))
length(ROX.array)


#true
dist <- max(ROX.array, na.rm = T)-min(ROX.array, na.rm = T)
rox.min <- min(ROX.array, na.rm = T); rox.min
rox.max <- max(ROX.array, na.rm = T); rox.max
hist(ROX.array, freq=FALSE, breaks = seq(rox.min, rox.max, by = dist/200), main = getwd())

#trimmed:
# new.max <- 6000
# new.min <- 1000
# hist(ROX.array[ROX.array < new.max & ROX.array > new.min], freq=FALSE, breaks = seq(new.min,new.max, by = (new.max - new.min/5)))

#byrow:
for(i in 1:24){
  ROX.row.mean[i,] <- ROX.mean[,i]
  ROX.row.sd[i,] <- ROX.sd[,i]
}



#idkwtfimdoing
ROX.sd.norm <- ROX.sd/mean(ROX.sd)
ROX.mean.norm <- ROX.mean/mean(ROX.mean)
ROX.row.sd.norm <- ROX.row.sd/mean(ROX.row.sd)
ROX.sd.min <- min(ROX.sd)
ROX.sd.max <- max(ROX.sd)

#"weighting"
#???if i do abs(rox.array[,,i] - rox.mean) then sum those i think it'll give me an approximation of what i'm looking for
for(i in 1:depth){
  weight.ROX.mean[i] <- as.numeric(sum(abs(ROX.mean - ROX.array[,,i]), na.rm = T))/mean(ROX.mean)
#  weight.ROX.sd[i] <- sum(sd(ROX.array[,,i]/sd(ROX.array)))
}
test.file <- array(NA, dim=c(depth,3))
test.file[,2] <- input.vector
test.file[,3] <- as.numeric(weight.ROX.mean)
test.file[,1] <- seq(1, depth, by = 1)

abc <- test.file[order(as.numeric(test.file[,3]), decreasing = T),]
head(abc)

ROX.array[,,as.numeric(abc[1,1])]
#mean(ROX.array[,,as.numeric(abc[1,1])], na.rm = T)

boxplot(ROX.sd.norm, ylab = "sd/mean(sd)", xlab = "column index", main = "normalized sd by col")
#boxplot(ROX.row.mean, ylab = "sd/mean", xlab = "row index", main = "normalized sd by row")
boxplot(ROX.mean.norm, ylab = "avg rox", xlab = "column index", main = "avg rox by col")
boxplot(ROX.mean, ylab = "avg rox", xlab = "row index", main = "avg rox by col")

#SD heatmapping
sd.heatmap <- heatmap.2(x = ROX.sd.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
           cellnote = signif(ROX.sd.norm,3), notecol = "black", notecex = 0.5,
           trace = "none", key = FALSE, xlab = "Column", ylab = "row", main = "heatmapping sd", col = colorRampPalette(c("yellow", "red"))(38))

#raw heatmapping
raw.heatmap <- heatmap.2(x = ROX.mean.norm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = signif(ROX.mean.norm,3), notecol = "black", notecex = 0.5,
          trace = "none", key = FALSE, xlab = "Column", ylab = "row", main = "heatmapping raw rox", col = colorRampPalette(c("red", "yellow"))(38))


#adaptive?
cutoffs <- quantile(ROX.array, probs = seq(0, 1, .002), na.rm = T)
plot(cutoffs)
head(cutoffs, 5)
tail(cutoffs, 5)
min.cutoff <- cutoffs[2]
max.cutoff <- cutoffs[length(cutoffs)-2]



