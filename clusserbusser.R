#clusserbusser aka let's test hclust BRU

getwd()

inputfile <- list.files()
#b <- grepl("clusser", inputfile)
b <- inputfile[68]
#data.file <- read.csv(inputfile[b], header = T, sep = ",")
data.file <- read.csv(b, header = T, sep = ",")

dfy <- data.file$y
dfx <- data.file$x

busser <- data.frame(x=dfx, y = dfy)

abc <- kmeans(busser, 3)
abc$tot.withinss

plot(dfy~dfx, col = abc$cluster)

abc$tot.withinss
