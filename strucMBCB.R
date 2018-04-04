install.packages("ParallelStructure", repo = "http://R-Forge.R-project.org")

library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)
library(tidyselect)
library("ParallelStructure")


#enterables:

#dir1 <- "S:/Data Analysis/Newport rush/2018/01 Jan/01-15-18/dt"
#dir1 <- "S:/Data Analysis/Performance Food Group/Aurora/2018/01 Jan/dt"
dir1 <- "S:/Data Analysis/Performance Food Group/Aurora/misc/test"
#paste filepath, replace backslash with forward slash
setwd(dir1); getwd(); 
#filename <- "aur ang jan 18"
filename <- "test"
initials <- "dt" 
vers <- 1.0 
#DT in use - it's much smarter now - picking out panel and formating based on that. 
#I'm reasonably confident that i'm amazing.



#reading in file
inputfile <- list.files()
DBEstruc <- grepl("dbe struc", inputfile) #DATGREPLTHO
data.file <- read.csv(inputfile[DBEstruc], header = F)
df.length <- length(data.file)


if(df.length < 100) {
  ang.cutoff = 0.7
  ref.filepath <- "S:/Data Analysis/Performance Food Group/Aurora/2014/MB40 Reference Panel 190214.txt"
  if(!dir.exists("./R")) {dir.create("./R")}
  refpop <- read.table(ref.filepath, sep = "", header = F)
  #test for carcass field, subset for dream brands
  if(df.length == 85) {
    good.brand <- c("AA", "BH")
    data.file <- data.file[data.file$V5 %in% good.brand,]
    data.file <- subset(data.file, select = -c(V5))
    names(data.file) <- names(refpop)
  }
  data.file <- data.file[data.file$V2 == 99,]
  #subsetting out those shitty branded samples
}
if(df.length == 100) {
  ang.cutoff = 0.5
  ref.filepath <- "S:/Data Analysis/Newport rush/CB48 Angus Reference Panel 112315.txt"
  if(!dir.exists("./R")) {dir.create("./R")}
  refpop <- read.table(ref.filepath, sep = "", header = F)
}

#various parameters
fail <- 33 #failure cutoff. this won't change, but hardcoding is for squares.
params.burnin <- 20000 
params.ngens <- 50000
fudge <- 1.085
final.file <- rbind(data.file, refpop)
npops<-c("1,2,99")

structure_list <- c("Job1",npops, 2, params.burnin, params.ngens)
structure_list <- as.matrix(structure_list)
structure_list <- t(structure_list)

ind2 <- nrow(final.file)
loci2 <- (ncol(final.file)-4)/2

write.table((structure_list), file = "./R/joblist2.txt", row.names = F, col.names = F, quote = F, se = " ")
write.table(final.file, file  = "./R/data.txt", row.names = F, col.names = F, quote = F, se = " ")

structure.filepath <-  "C:/Users/dtaylor.IDENTIGENIRL/Documents/R/TEST/"
joblist.filepath   <-  './R/joblist2.txt'
infile.filepath    <-  './R/data.txt'
output.filepath    <-  './R/structure_results'

parallel_structure(structure_path = structure.filepath,  
                   joblist = joblist.filepath, n_cp = 1, infile = infile.filepath, 
                   outpat = output.filepath, numind = ind2, numloc = loci2, printqha = 0, onerowperind = 1, popdata = 1, 
                   popflag = 1, plot_output = 0, usepopinfo = 1, missing = 0, ploid = 2, locdat = 0, phenotypes = 0, 
                   markernames = 0, mapdist = 0, label = 1, phaseinfo = 0, phased = 0, recessivealleles = 0, extracol = 1, 
                   noadmix = 0, linkag = 0, inferalph = 1, alpha = 1, popalphas = 0, unifprioralpha = 1, alphamax = 10, 
                   alphapropsd = 0.025, freqscorr = 0, lambda = 1, computeprob = 1, pfromflagonly = 1, ancestdist = 1, 
                   startatpopinfo = 0, metrofreq = 10, updatefreq = 1, randomize = 1)

output.file <- fread("./R/structure_resultsresults_job_Job1_f", skip = (58 + nrow(refpop)), header = F, sep = c(" "))


#cleaning
output.file <- output.file[,c(-1,-5)]
output.file$V3 <- sub("\\(","",output.file$V3)
output.file$V3 <- sub("\\)","",output.file$V3)
output.file$V3 <- as.numeric(output.file$V3)
output.file$V8 <- sub("\\(","",output.file$V8)	
output.file$V8 <- sub("\\)","",output.file$V8)
output.file$V9 <- sub("\\(","",output.file$V9)
output.file$V9 <- sub("\\)","",output.file$V9)

#breaking apart the ang/non-ang columns
output.file <- separate(data = output.file, col = V8, into = c("banana", "are"), sep = "\\,", remove = T)
output.file <- separate(data = output.file, col = V9, into = c("the", "greatest"), sep = "\\,", remove = T)
colnames(output.file) <- c("SampleID",	"%FailedMarkers",	"Pop",	"ProportionOfAngus",	
                           "ProportionOfNonAngus",	"LowerConfidenceValueOfAngus",	"UpperConfidenceValueOfAngus",	
                           "LowerConfidenceValueOfNonAngus",	"UpperConfidenceValueOfNonAngus")

#fix the UCI angus values?
#output.file$UpperConfidenceValueOfAngus <- as.double(output.file$UpperConfidenceValueOfAngus)
#output.file$UpperConfidenceValueOfAngus <- transform(output.file$UpperConfidenceValueOfAngus * fudge)
#THIS IS CHEATING DIRTY ROTTEN CHEATING

#result assignment
output.file$Result <- "NO RESULT"
output.file$Result[output.file$UpperConfidenceValueOfAngus > ang.cutoff & output.file$`%FailedMarkers` < fail] <- "PASS"
output.file$Result[output.file$UpperConfidenceValueOfAngus <= ang.cutoff & output.file$`%FailedMarkers` < fail] <- "FAIL"

table(output.file$Result)
#output.table.1 <- table(output.file$Result); output.table.1
#output.table.2 <- table(output.file$Result); output.table.2
#output.table.3 <- table(output.file$Result); output.table.3
#output.table.4 <- table(output.file$Result); output.table.4


#output.table; output.table.1; output.table.2; output.table.3; output.table.4 


#file naming. This will DEFINITELY change.
SysDa <- as.character((Sys.Date()))
filename <- paste("Results", filename)
output_a <- paste(filename,initials,vers, SysDa, sep = "-"); 
output_a <- paste(output_a, ".csv", sep = "")
#setwd("./R")
write.csv(output.file,file = output_a, row.names = F)

getwd()

