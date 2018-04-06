#read reader
#DT
#2018-03-01


readreader <- function(test.file){
  #pull in distinct tables
  to.format.FAM <- read.csv(test.file, skip = 7, nrows = 16)
    format.FAM <- stack(to.format.FAM)
  to.format.YY <- read.csv(test.file, skip = 26, nrows = 16)
    format.YY <- stack(to.format.YY)
  to.format.ROX <- read.csv(test.file, skip = 45, nrows = 16)
    format.ROX <- stack(to.format.ROX)

  #format x, y
  formatted.X <- format.FAM[1:384,1]/format.ROX[1:384,1]
  formatted.Y <- format.YY[1:384,1]/format.ROX[1:384,1]
  
  final <- cbind(formatted.X,formatted.Y)
  names(final) <- c("ecks", "why?")
  
  # plot(final)
  # #i'm not sure why i'm doing this or what i'm trying to get out of it  
  # asdf <- 2
  # kmeans(final, centers = asdf, iter.max = 100, nstart = 1)$betweenss/asdf
  # # plot(final, col = abc$cluster)
  
  file <- sub(".csv", "", test.file)
  output_a <- paste(file, ".png", sep = "")
  png(filename = output_a)
  plot(final, ylab = "", xlab = "", main = file)
  dev.off()
}

