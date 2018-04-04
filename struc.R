install.packages("ParallelStructure", repo = "http://R-Forge.R-project.org")
library("ParallelStructure")
install.packages("tidyr")

library(data.table)
library(reshape2)
library(tidyr)
library(tidyselect)
library("ParallelStructure")


dir0 <- "c:/Users/dtaylor.IDENTIGENIRL/Documents"
setwd(dir0)

vers <- 0.1
initials <- "dt" 
ang.cutoff <- 0.5 #cutoff, ideally this will vary based on # snp included for MB40 vs CB48
fail <- 33 #failure cutoff. this won't change, but hardcoding is for squares.
params.burnin <- 20000 
params.ngens <- 50000

data.file <- read.csv("./R/dbe_struct_ABPLP_191017_ap.csv", header =  F)
refpop <- read.table("./R/CB48 Angus Reference panel 112315.txt", sep = "", header = F)

final.file <- rbind(data.file,refpop)
# remove the bullshit column
#final.file <- final.file[,-4]
# only keep if setting extra col = 1, which i'm not GIT GUD
npops<-c("1,2,99")

structure_list <- c("Job1",npops, 2, params.burnin, params.ngens)
structure_list <- as.matrix(structure_list)
structure_list <- t(structure_list)

ind2 <- nrow(final.file)
loci2 <- (ncol(final.file)-4)/2

write.table((structure_list), file = "./R/TEST/joblist2.txt", row.names = F, col.names = F, quote = F, se = " ")
write.table(final.file, file  = "./R/TEST/example_data2.txt", row.names = F, col.names = F, quote = F, se = " ")

structure.filepath <-  "C:/Users/dtaylor.IDENTIGENIRL/Documents/R/TEST/"
joblist.filepath   <-  './R/TEST/joblist2.txt'
infile.filepath    <-  './R/TEST/example_data2.txt'
output.filepath    <-  './R/TEST/structure_results'

parallel_structure(structure_path = structure.filepath,  
          joblist = joblist.filepath, n_cp = 1, infile = infile.filepath, 
          outpat = output.filepath, numind = ind2, numloc = loci2, printqha = 0, onerowperind = 1, popdata = 1, 
          popflag = 1, plot_output = 0, usepopinfo = 1, missing = 0, ploid = 2, locdat = 0, phenotypes = 0, markernames = 0, 
          mapdist = 0, label = 1, phaseinfo = 0, phased = 0, recessivealleles = 0, extracol = 1, noadmix = 0, linkag = 0, 
          inferalph = 1, alpha = 1, popalphas = 0, unifprioralpha = 1, alphamax = 10, alphapropsd = 0.025, freqscorr = 0, 
          lambda = 1, computeprob = 1, pfromflagonly = 1, ancestdist = 1, startatpopinfo = 0, metrofreq = 10, updatefreq = 1, randomize = 0
          )#, ancestpint = 0.975, gensback = 10, migrpriork = 0.005, revert_convert = 0)

output.file <- fread("./R/TEST/structure_resultsresults_job_Job1_f", skip = (58 + nrow(refpop)), header = F, sep = c(" "))
#output.file <- fread("./TEST/structure_resultsresults_job_Job1_f", skip = (58 + nrow(refpop)), header = F, sep = c(" "))

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

output.file.reset <- output.file
output.file <- output.file.reset

#results
output.file$Result <- "NO RESULT"
output.file$Result[output.file$UpperConfidenceValueOfAngus > ang.cutoff & output.file$`%FailedMarkers` < fail] <- "PASS"
output.file$Result[output.file$UpperConfidenceValueOfAngus <= ang.cutoff & output.file$`%FailedMarkers` < fail] <- "FAIL"


SysDa <- as.character((Sys.Date()))
output_a <- paste("Ang analysis run",initials,vers, SysDa, sep = "-"); 
output_a <- paste(output_a, ".csv", sep = "")
setwd("./R/TEST")
write.csv(output.file,file = outpu t_a, row.names = F)
setwd(dir0)
?write.csv

