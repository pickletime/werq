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


####aurKSconsol

source("L:/DT/R/aurKSconsol.R")
wd <- "L:/Sample Records/Performance Food Group/Kill Sheets/Aurora/2018/05 May/originals"
aurKSconsol(wd, month = "05 May")


###aurKS what the fuck is going on
source("L:/DT/R/aurKS.R")
wd <- "L:/Sample Records/Performance Food Group/Kill Sheets/Aurora/2018/03 March/Originals/new"
setwd(wd)
killsheet.filenames <- list.files()
for (i in 1:length(killsheet.filenames)){aurKS(killsheet.filenames[i])}

#####NGF WHATUP
source("L:/DT/R/NGF.R")
for(i in 42:43){NGF.lite(dir,i); print(i)}
source("L:/DT/R/NGF.R"); NGF("//us-kraken/kraken/Projects/027/362/Results", 1)
