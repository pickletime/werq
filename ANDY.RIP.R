asdf
setwd("L:/DT/R/RIPANDY")

asdfdsa <- list.files()
fdsa <- asdfdsa[1]
asdf <- read.csv(fdsa)
asdf$call <- as.character(asdf$call)

for(i in 1:length(asdf$call)){
  if(asdf[i,3]==" B") {asdf[i,3] <- "Blue"} 
  if(asdf[i,3]==" G") {asdf[i,3] <- "Green"}
  if(asdf[i,3]==" R") {asdf[i,3] <- "Red"}
  if(asdf[i,3]==" U") {asdf[i,3] <- "purple"}
}

# head(asdf)
# tail(asdf)

plot(asdf$y ~ asdf$x, col = as.character(asdf$call))







asdf[i,3]
asdf[1,3]==" G", asdf[i,3] <- "Blue") 