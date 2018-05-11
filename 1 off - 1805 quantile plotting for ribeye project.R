library("rJava")
library("xlsx")


setwd("S:/Data Analysis/Performance Food Group/PFG Miscellaneous/1805 PFG steak special project")
#pull in file, do that grepl thing, filter out samples we wouldn't plot anyway
xyz <- list.files()
xyz<- xyz[3]

cba <- read.xlsx(xyz, sheetIndex = 1, colIndex = c(1:13))
abc <- cba[cba$brand == "BHC",]

UCA1 <- cba$UpperConfidenceValueOfAngus
UCA2 <- abc$UpperConfidenceValueOfAngus

UCA1q <- quantile(UCA1, probs = seq(0, 1, by = 1/3))
UCA2q <- quantile(UCA2, probs = seq(0, 1, by = 1/3), na.rm = T)
meaningfulpoints <- c(120813629, 120814174, 120813809, 120815133, 120814737, 120815121)

UCAp <- cba[cba$SampleID %in% meaningfulpoints,8]
#UCA2p <- abc[abc$SampleID %in% meaningfulpoints,8]



par(mfrow = c(1, 2))
plot(UCA1, xaxt = "n", xlab = "", ylab = "UCI Angus score", main = "UCI angus scores/quantile")
abline(h = UCAp, col = 6)
text(1, UCA1q, c("Q1", "Q2", "Q3", "Q4"), cex = 0.75, col = c(1, 2, 3, 4), pos=1)
for(i in 1:length(UCA1q)){
  abline(h = UCA1q[i], col = i, lwd = 2)
}


plot(UCA2, xaxt = "n", xlab = "", ylab = "UCI Angus score", main = "UCI angus scores/quantile for BHC")
abline(h = UCAp, col = 6)
text(1, UCA2q, c("Q1", "Q2", "Q3", "Q4"), cex = 0.75, col = c(1, 2, 3, 4), pos=1)
for(i in 1:length(UCA2q)){
  abline(h = UCA2q[i], col = i, lwd = 2)
}


