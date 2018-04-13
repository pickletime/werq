install.packages("ParallelStructure", repo = "http://R-Forge.R-project.org")
library("ParallelStructure")
install.packages("tidyr")

library(data.table)
library(reshape2)
library(tidyr)
library(tidyselect)
library("ParallelStructure")

dir1 <- "S:/Data Analysis/Newport rush/2018/01 Jan/01-02-18/dt"
#paste filepath, replace backslash with forward slash
dir0 <- "c:/Users/dtaylor.IDENTIGENIRL/Documents"
setwd(dir1)
getwd()

vers <- 0.11
initials <- "dt" 
ang.cutoff <- 0.5 #cutoff, ideally this will vary based on # snp included for MB40 vs CB48
fail <- 33 #failure cutoff. this won't change, but hardcoding is for squares.
params.burnin <- 20000 
params.ngens <- 50000

inputfile <- list.files()
data.file <- read.csv(inputfile[1], header = F)
#[n] in this case is the file. This could (will?) be smarter at some point but for now look through inputfile for struc dbe and select

filename <- "ABPLP STRUC SCRIPT TEST"
refpop <- read.table("S:/Data Analysis/Newport rush/CB48 Angus Reference Panel 112315.txt", sep = "", header = F)


final.file <- rbind(data.file,refpop)
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
                   popflag = 1, plot_output = 0, usepopinfo = 1, missing = 0, ploid = 2, locdat = 0, phenotypes = 0, markernames = 0, 
                   mapdist = 0, label = 1, phaseinfo = 0, phased = 0, recessivealleles = 0, extracol = 1, noadmix = 0, linkag = 0, 
                   inferalph = 1, alpha = 1, popalphas = 0, unifprioralpha = 1, alphamax = 10, alphapropsd = 0.025, freqscorr = 0, 
                   lambda = 1, computeprob = 1, pfromflagonly = 1, ancestdist = 1, startatpopinfo = 0, metrofreq = 10, updatefreq = 1, randomize = 0)

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
output.file <- separate(data = output.file, col = V8, into = c("UCI ANG", "LCI ANG"), sep = "\\,", remove = T)
output.file <- separate(data = output.file, col = V9, into = c("UCI non-ANG", "non-ANG"), sep = "\\,", remove = T)

#names
colnames(output.file) <- c("SampleID",	"%FailedMarkers",	"Pop",	"ProportionOfAngus",	
                           "ProportionOfNonAngus",	"LowerConfidenceValueOfAngus",	"UpperConfidenceValueOfAngus",	
                           "LowerConfidenceValueOfNonAngus",	"UpperConfidenceValueOfNonAngus")


#results
output.file$Result <- "NO RESULT"
output.file$Result[output.file$UpperConfidenceValueOfAngus > ang.cutoff & output.file$`%FailedMarkers` < fail] <- "PASS"
output.file$Result[output.file$UpperConfidenceValueOfAngus <= ang.cutoff & output.file$`%FailedMarkers` < fail] <- "FAIL"

#file naming. This will DEFINITELY change.
SysDa <- as.character((Sys.Date()))
filename <- paste("Results", filename)
output_a <- paste(filename,initials,vers, SysDa, sep = "-"); 
output_a <- paste(output_a, ".csv", sep = "")
setwd("./R")
write.csv(output.file,file = output_a, row.names = F)
setwd(dir0)


