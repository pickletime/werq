#this project has been declared useless. Given the same seed and params I can't replicate structure's output. 
#B A S E D L U C K
#A
#S
#E
#D
#L
#U
#C
#K

#bulk load relevant libraries
x <- c("data.table", "reshape2","tidyr", "dplyr", "tidyselect", "devtools", "githubinstall")
lapply(x, require, character.only = T)


install_github("pickletime/ParallelStructure_IR"); library(ParallelStructure_IR)

#install_github("b00mir/ParallelStructure_IR")


#enterables:

dir1 <- "S:/Data Analysis/Performance Food Group/Aurora/misc/test/asdf"
dir2 <- "S:/Data Analysis/Performance Food Group/Aurora/2018/04 April/dt"
#paste filepath, replace backslash with forward slash
setwd(dir2); getwd(); 
filename <- "IH8U"
initials <- "dt" 
vers <- 1.0 
#1.0: it's much smarter now - picking out panel and formating based on that. 
#minor touchups for now. At some point i'll edit the library, but that's a ways out.
#I'm reasonably confident that i'm amazing.



#reading in file
inputfile <- list.files()
DBEstruc <- grepl("dbe struc", inputfile) #DATGREPLTHO
data.file <- read.csv(inputfile[DBEstruc], header = F)
df.length <- length(data.file)


if(df.length < 100) {
  params.ang.cutoff = 0.7
  # params.ancestpint = 0.95
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
  params.ang.cutoff = 0.5
  # params.ancestpint = 0.975
  ref.filepath <- "S:/Data Analysis/Newport rush/CB48 Angus Reference Panel 112315.txt"
  if(!dir.exists("./R")) {dir.create("./R")}
  refpop <- read.table(ref.filepath, sep = "", header = F)
}


#various parameters
fail <- 33 #failure cutoff. this won't change, but i wanted it hardcoded.
params.burnin <- 20000 
params.ngens <- 50000

final.file <- rbind(data.file, refpop)
npops<-c("1,2,99")

structure_list <- c("Job1",npops, 2, params.burnin, params.ngens)
structure_list <- as.matrix(structure_list)
structure_list <- t(structure_list)

ind2 <- nrow(final.file)
loci2 <- (ncol(final.file)-4)/2

write.table((structure_list), file = "./R/joblist1.txt", row.names = F, col.names = F, quote = F, sep = " ")
write.table(final.file, file  = "./R/data.txt", row.names = F, col.names = F, quote = F, sep = " ")

#structure.filepath <-  "C:/Users/dtaylor.IDENTIGENIRL/Documents/R/TEST/"
#I'm pretty sure i'll have to copy it into this directory, until then i'm testing a different place
structure.filepath <- "L:/DT/R/struc"
joblist.filepath   <-  './R/joblist1.txt'
infile.filepath    <-  './R/data.txt'
output.filepath    <-  './R/structure_results'

parallel_structure(
    structure_path = structure.filepath,  #filepath to struc
    joblist = joblist.filepath,           #filepath to joblist, outputted earlier
    n_cpu = 1,                            #number of cpu used, this could be fun to play with?
    infile = infile.filepath,             #filepath to data, output earlier
    outpath = output.filepath,            #filepath to structure results, from earlier
    numinds = ind2,                       #number of individuals, pulled from datafile
    numloci = loci2,                      #number of loci, pulled from datafile
    printqhat = 0,                        #
    onerowperind = 1,                     #one row per ind, 1 = yes
    popdata = 1,                          #
    popflag = 1,                          #
    plot_output = 0,                      #
    usepopinfo = 1,                       #
    missing = 0,                          #
    ploidy = 2,                           #
    locdata = 0,                          #
    phenotypes = 0,                       #
    markernames = 0,                      #
    mapdist = 0,                          #
    label = 1,                            #
    phaseinfo = 0,                        #
    phased = 0,                           #
    recessivealleles = 0,                 #
    extracol = 1,                         #
    noadmix = 0,                          #
    linkage = 0,                          #
    inferalpha = 1,                       #
    alpha = 1,                            #
    popalphas = 0,                        #
    unifprioralpha = 1,                   #
    alphamax = 10,                        #
    alphapropsd = 0.025,                  #
    freqscorr = 0,                        #
    fpriormean = 0.1,                     #new version of lib set this to default, unnecessary now
    fpriorsd = 0.1,                       #new version of lib set this to default, unnecessary now
    lambda = 1,                           #
    computeprob = 1,                      #
    pfromflagonly = 1,                    #
    ancestdist = 1,                       #
    startatpopinfo = 0,                   #
    metrofreq = 10,                       #
    updatefreq = 1,                       #
    randomize = 1,                        #
    gensback = 10,                        #
    migrprior = 0.005,                    #
    ancestpint = 0.95)                    #




output.raw <- fread("./R/structure_resultsresults_job_Job1_f", skip = (58 + nrow(refpop)), header = F, sep = c(" "))
output.file <- output.raw

gitfucked <- output.file


#cleaning
output.file$V4 <- rep(99,length(output.file$V4)) #this is only there because i hate seeing pop 3, even if it's technically correct.
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

#result assignment
output.file$Result <- "NO RESULT"
output.file$Result[output.file$UpperConfidenceValueOfAngus > params.ang.cutoff & output.file$`%FailedMarkers` < fail] <- "PASS"
output.file$Result[output.file$UpperConfidenceValueOfAngus <= params.ang.cutoff & output.file$`%FailedMarkers` < fail] <- "FAIL"

table(output.file$Result)


#file naming. This will DEFINITELY change.
SysDa <- as.character((Sys.Date()))
filename <- paste("Results", filename)
output_a <- paste(SysDa, vers, initials, filename, sep = "-"); 
output_a <- paste(output_a, ".csv", sep = "")
#setwd("./R")
write.csv(output.file,file = output_a, row.names = F)

getwd()

