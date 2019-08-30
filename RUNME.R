###HOLY SHIT THIS IS AMAZING
for(i in 1:10){update.packages(ask = FALSE); print(i)}
###HOLY SHIT THIS IS AMAZING

old.wd <- getwd()
#setwd("./R")
pasted.file.path <- "L:/Sample Records/Performance Food Group/Kill Sheets/Aurora/2017/December/Originals"
pasted.file.path <- "L:/Sample Records/FPL Food, Inc/Kill Sheets/2017/12 December/Originals"
dir0 <- "c:/Users/dtaylor.IDENTIGENIRL/Documents"
basedir <- "c:/Users/dtaylor.IDENTIGENIRL/Documents/R"
aurtesting <- "c:/Users/dtaylor.IDENTIGENIRL/Documents/R/aurKS"
fpltesting <- "c:/Users/dtaylor.IDENTIGENIRL/Documents/R/fplKS"
readtesting <- "c:/Users/dtaylor.IDENTIGENIRL/Documents/R/readreader"
#for more explicit, precise work
setwd(readtesting); getwd()


xyz <- list.files()
xyz
killsheet.filenames <- xyz

source("L:/DT/R/auroraKS.R")
source("L:/DT/R/fplKS.R")
source("L:/DT/R/read.reader.R")


######pointing at results directory
setwd(dir); getwd()
file.list <- list.files()
temp.d <- tempdir()
temp.list <- unzip(file.list[1],exdir = temp.d)
output.file <- read.table(file = temp.list[2], fill = T)
unlink(temp.d)
######create tempdir, unzip, pull in the first file, delete tempdir


list.files()
for (i in 1:length(killsheet.filenames)){auroraKS(killsheet.filenames[i])}



#fpl the loop isn't working idkwhy
for (i in 1:length(killsheet.filenames[20:29])){fplKS(killsheet.filenames[i])}

#from specific dir
for (i in 1:length(killsheet.filenames)){fplKS(killsheet.filenames[i])}



######readreader
setwd("//us-kraken/kraken/Plate reader archive/2018/02/27/1"); getwd()

filenames <- list.files()
source("L:/DT/R/read.reader.R")
for(i in 1:(length(filenames)-1)){readreader(filenames[i]); print(i)}
readreader(filenames[193])


#####NGF WHATUP
source("L:/DT/R/NGF.R"); NGF("//us-kraken/kraken/Projects/004/169/Results", 0)

getwd()
source("L:/DT/R/iChao.R"); iChao()





library(allelematch)
vignette("allelematchSuppDoc")

data("amExample1")
example1 <- amDataset(amExample1, indexColumn="sampleId", ignoreColumn ="knownIndividual", missingCode="-99")
amUniqueProfile(example1, doPlot=TRUE)
uniqueExample1 <- amUnique(example1, alleleMismatch=2)


######sequoia?
install.packages("sequoia"); library(sequoia)

Geno <- SimGeno(Ped = Ped_HSg5, nSnp = 200)
#this means we can definitely just read in the exported files YIPPEE HOORAY
?GenoConvert


##parentage splitter
source("L:/DT/R/ParentageSplitter.R"); ParentageSplitter()
