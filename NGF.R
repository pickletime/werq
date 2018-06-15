#I'm ultimately going to try to do something meaningful here. I think data quality across sisterplates is a reasonable start?

#getting shit where it needs to go
getwd()
setwd("//us-kraken/kraken/Projects/069/013/Results")
setwd("c:/Users/dtaylor.IDENTIGENIRL/Documents/R/NGF")

count <- 2

#necessary lib loading
library(data.table)
library(reshape)
library(tidyr)
if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}


#pick target file(s)
target.file <- list.files()
target.file <- target.file[count]; target.file
#pull in whole file, filling 
working.file <- read.table(file = target.file, fill = T)
#start new df from appropriate location
df.working.file <- data.frame(working.file[(match("Data",working.file$V1)+2):nrow(working.file),1])
#this is necessary because it's too big?
#df.working.file <- data.frame(working.file[3118:nrow(working.file),1])




#
##here is where i make it walk through multiple files I THINK
#

#reformat df with appropriate headers
df.final <- separate(data = df.working.file, col = 1, sep = "\\,",
                     into = c("DaughterPlate","MasterPlate","MasterWell","Call","X","Y","SNPID","SubjectID",
                              "Norm","Carrier","DaughterWell"))
  #new kraken has another column (AliquotID) but it doesn't seem to be used?
well.list <- sort(unique(df.final$DaughterWell))
snp.list <- unique(df.final$SNPID)
#this is wrong. i need to correct this for sure
uncalled <- c("?", "NTC", "")




raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)


#how to extract #matching whatever
#length(df.final$SNPID[df.final$SNPID == snp.list[1]])

#i could just do this every time but that seems stupid. OR I COULD MAKE A 3D DF HOLY SHIT
#df.subset <- df.final[df.final$DaughterWell == well.list[1],]

#length(df.subset$Call[df.subset$Call == uncalled])/length(df.subset$DaughterWell)

#I actually think this works now. No way it's that easy.
for(i in 1:length(well.list)){
  loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
  raw.output.table[i,1] <-well.list[i]
  raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
}

#raw.output.table[,1]

#this one is fine
data.cols <- as.numeric(substr(raw.output.table[,1],2,3))
#this one is not fine i just need to figure this out
data.rows <- substr(raw.output.table[,1],1,1)
fac <- factor(data.rows); levels(fac) <- 1:16; data.rows <- as.numeric(fac)
#and of course
raw.output.table[,2] <- as.numeric(raw.output.table[,2])

  
#once the data.rows is fine we'll be good to go
data.table <- matrix(data = 0, nrow = length(unique(data.rows)), ncol = length(unique(data.cols)))

#pushing rows/cols into output df
raw.output.table[,3] <- data.rows
raw.output.table[,4] <- data.cols

#this took entirely too long to figure out
for(i in 1:length(unique(data.rows))){
  subset.output.table <- raw.output.table[raw.output.table[,3] == i,]
  for(j in 1:length(unique(data.cols))){
    ifelse(length(subset.output.table[subset.output.table[,4] == j,2]) >0 , 
           data.table[i,j] <- subset.output.table[subset.output.table[,4] == j,2], 0)
  }
}

data.table[1:2,1:2] <- 0

output.matrix <- matrix(as.numeric(unlist(data.table)),nrow=nrow(data.table))

plate.pass.rate <- heatmap.2(x = output.matrix, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                             cellnote = 100*signif(output.matrix,1), notecol = "black", notecex = 0.5,
                             trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
                             main = target.file, col = colorRampPalette(c("white", "red"))(100))

t.test(output.matrix[1,], output.matrix[2:15,]); t.test(output.matrix[16,], output.matrix[2:15,])
t.test(output.matrix[1,], output.matrix[16,])

#save the trimmed/neat version. also: row.names=F is heaven.
write.csv(df.final[,-c(9,10)], file = paste("zzz", sub(".csv", "", target.file), ".csv", sep = "-"), row.names = F)

count <- count + 1