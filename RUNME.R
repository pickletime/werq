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


killsheet.filenames <- c("ezmode.csv", "ezmode1.csv", "ezmode2.csv", "ezmode3.csv", "ezmode4.csv", "ezmode5.csv", "ezmode6.csv", "ezmode7.csv", "ezmode8.csv", "ezmode9.csv", "ezmode10.csv", "ezmode11.csv", "ezmode12.csv")
tech.initials <- "dt"

fplKS(killsheet.filenames[22], tech.initials)

#aur 5:18

for (i in 1:length(killsheet.filenames)){auroraKS(killsheet.filenames[i])}
#fpl the loop isn't working idkwhy
for (i in 1:length(killsheet.filenames[20:29])){fplKS(killsheet.filenames[i])}

#from specific dir
for (i in 1:length(killsheet.filenames)){fplKS(killsheet.filenames[i])}



######readreader
setwd("//us-kraken/kraken/Plate reader archive/2018/02/27/1"); getwd()

filenames <- list.files()
source("L:/DT/R/read.reader.R")
for(i in 1:length(filenames)){readreader(filenames[i]); print(i)}
readreader(filenames[193])


####aurKSconsol

source("L:/DT/R/aurKSconsol.R")
wd <- "L:/Sample Records/Performance Food Group/Kill Sheets/Aurora/2018/03 March"

aurKSconsol(wd)
