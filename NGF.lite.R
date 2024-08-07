#this is only built to bulk process untouched NGF in the one directory. 
#With a bit of work this could be more useful but i just need it to do this for right now

NGF.lite <- function (unformatted.file, wd){
#necessary lib loading
if (!require("data.table")) {
  install.packages("data.table", dependencies = TRUE); library(data.table)}
if (!require("reshape")) {
  install.packages("reshape", dependencies = TRUE); library(reshape)}
if (!require("tidyr")) {
  install.packages("tidyr", dependencies = TRUE); library(tidyr)}
if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE); library(gplots)}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE); library(RColorBrewer)}
  
  directoryextractor.func <- function(selected.file){
    filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
    filename <- filename.vector[length(filename.vector)]
    setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
  }

setwd(wd)
#pick target file(s)
#directoryextractor.func(unformatted.file)
#target.file <- list.files()
#target.file <- target.file[grepl(".csv", target.file)]

working.file <- read.table(file = unformatted.file, fill = T)
#start new df from appropriate location
df.working.file <- data.frame(working.file[(match("Data",working.file$V1)+2):nrow(working.file),1])

#reformat df with appropriate headers
df.final <- separate(data = df.working.file, col = 1, sep = "\\,",
                     into = c("DaughterPlate","MasterPlate","MasterWell","Call","X","Y","SNPID","SubjectID",
                              "Norm","Carrier","DaughterWell", "LongID", "AliquotID"))
##add in panel, test for completeness. Doing this for the main 3 should be easy enough.
#porcID <- c("ALGA0001559", "H3GA0011528", "ASGA0043022", "MARC0096462", "H3GA0033394", "ASGA0068674", "M1GA0001071", "H3GA0019987", "ALGA0047477", "MARC0075680", "H3GA0030429", "ALGA0006150", "ALGA0010541", "ALGA0012456", "ALGA0014515", "ASGA0103938", "H3GA0008408", "ASGA0092767", "ASGA0093631", "MARC0017801", "ALGA0024130", "DIAS0004198", "MARC0026858", "ALGA0033921", "DRGA0006563", "MARC0058178", "ALGA0036718", "H3GA0022882", "MARC0067233", "ALGA0049258", "MARC0029757", "ALGA0055075", "ALGA0057529", "ALGA0060238", "ASGA0054436", "ALGA0067470", "H3GA0036968", "H3GA0037306", "H3GA0037982", "MARC0011128", "CASI0009023", "CASI0007894", "MARC0055746", "ALGA0089624", "MARC0098653", "MARC0050941", "ALGA0094758", "ASGA0083083")

{

#well.list <- sort(unique(df.final$DaughterWell))
#snp.list <- unique(df.final$SNPID)
#this is wrong. i need to correct this for sure
#uncalled <- c("?", "NTC", "", "Over")
#raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)


#how to extract #matching whatever
#length(df.final$SNPID[df.final$SNPID == snp.list[1]])
#length(df.subset$Call[df.subset$Call == uncalled])/length(df.subset$DaughterWell)

#I actually think this works now. No way it's that easy.
#for(i in 1:length(well.list)){
#  loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
#  raw.output.table[i,1] <-well.list[i]
#  raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
#}


#this one is fine
#data.cols <- as.numeric(substr(raw.output.table[,1],2,3))
#this one is not fine i just need to figure this out
#data.rows <- substr(raw.output.table[,1],1,1)
#fac <- factor(data.rows); levels(fac) <- 1:16; data.rows <- as.numeric(fac)
#and of course
#raw.output.table[,2] <- as.numeric(raw.output.table[,2])

  
# #once the data.rows is fine we'll be good to go
# data.table <- matrix(data = 0, nrow = length(unique(data.rows)), ncol = length(unique(data.cols)))
# 
# #pushing rows/cols into output df
# raw.output.table[,3] <- data.rows
# raw.output.table[,4] <- data.cols
# 
# #this took entirely too long to figure out
# for(i in 1:length(unique(data.rows))){
#   subset.output.table <- raw.output.table[raw.output.table[,3] == i,]
#   for(j in 1:length(unique(data.cols))){
#     ifelse(length(subset.output.table[subset.output.table[,4] == j,2]) >0 , 
#            data.table[i,j] <- subset.output.table[subset.output.table[,4] == j,2], 0)
#   }
# }
# 
# #data.table[1:2,1:2] <- 0
# output.matrix <- 100*matrix(as.numeric(unlist(data.table)),nrow=nrow(data.table))
# 
# #plotting shit
# par(mar = c(1, 1, 1, 1))
# unformatted.file <- sub(".csv", "", unformatted.file)
# plate.pass.rate <- heatmap.2(x = output.matrix, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
#                              cellnote = signif(output.matrix,1), notecol = "black", notecex = 0.5,
#                              trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
#                              main = unformatted.file, col = colorRampPalette(c("white", "red"))(96))
# 
# par(mar = c(1, 1, 1, 1))
# par(mfrow=c(2,1))
# boxplot(output.matrix, ylab = "average failure rate", xlab = "column index", main = "average failure rate per column", col = "orange", xaxt = 'n')
# boxplot(t(output.matrix), ylab = "average failure rate", xlab = "row index", main = "average failure rate per row", col = "gold", xaxt = 'n')
# 
}

#save the trimmed/neat version. also: row.names=F is heaven.
#setwd("L:/DT/R/NGF")
if(!dir.exists("./cleaned files")) {dir.create("./cleaned files")}

unformatted.file <- sub(".csv", "", unformatted.file)
write.csv(df.final[,-c(9,10)], file = paste(sub("Genotyping", "Trimmed NGF", unformatted.file),sep = "", ".csv"), row.names = F)
print(paste(sub("Genotyping", "Trimmed NGF", unformatted.file),sep = "", ".csv"))
}
