getwd()
setwd("S:/Data Analysis/Performance Food Group/Aurora/2018/03 mARCH/dt")

library("rJava")
library("xlsx")

#pull in file, do that grepl thing, filter out samples we wouldn't plot anyway
xyz <- list.files()
aurfile <- grepl("[A-z]urora [A-z]ngus [A-z]esults", xyz)
cba <- read.xlsx(xyz[aurfile], sheetIndex = 1, colIndex = c(1:13))
abc <- cba[cba$Result != "NO RESULT",]
uci.mean <- mean(abc$UpperConfidenceValueOfAngus)



#real plotty plot
plot(y = abc$UpperConfidenceValueOfAngus, x = 1:length(abc$UpperConfidenceValueOfAngus), 
     type = "h", col = "red", ylab = "", xlab = "", xaxt = 'n', lwd = 0.05, bty = 'n')
lines(abc$UpperConfidenceValueOfAngus, col = "black", lwd = 1.5)
abline(h = uci.mean, col = "yellow", lwd = 3)
text(length(abc$SampleID)/4,0.2, signif(uci.mean, 4), col = "yellow", cex = 1, pos = 3)


dev.off()
