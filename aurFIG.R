getwd()
setwd("S:/Data Analysis/Performance Food Group/Aurora/2018/02 Feb/dt")



library("rJava")
library("xlsx")

xyz <- list.files()
xyz[4]
# abc <- read.csv(xyz[3])
##for read.xlsx
cba <- read.xlsx(xyz[4], sheetIndex = 1, colIndex = c(1:13))
abc <- cba
##hopefully the above works
abc <- abc[abc$Result != "NO RESULT",]


abc$UpperConfidenceValueOfAngus <- sort(abc$UpperConfidenceValueOfAngus)

#length(abc$UpperConfidenceValueOfAngus)
hist(abc$UpperConfidenceValueOfAngus, freq = FALSE, col = "red", border = "black", breaks = seq(0,1, by = 1/20), 
     ylab = "Percent of data within bracket", xlab = "Angus score", lwd = 0.05, bty = 'n')
abline(v = mean(abc$UpperConfidenceValueOfAngus), col = "blue", lwd = 3)

plot(y = abc$UpperConfidenceValueOfAngus, x = 1:length(abc$UpperConfidenceValueOfAngus), 
     type = "h", col = "red", main = "Aurora Angus values for A MONTH", 
     ylab = "", xlab = "", xaxt = 'n',
     lwd = 0.05, bty = 'n')
lines(abc$UpperConfidenceValueOfAngus, col = "black")
abline(h = mean(abc$UpperConfidenceValueOfAngus), col = "yellow", lwd = 3)

dev.off()
