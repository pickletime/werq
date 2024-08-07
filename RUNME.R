###HOLY SHIT THIS IS AMAZING
print(update.packages(ask = FALSE))
###HOLY SHIT THIS IS AMAZING

old.wd <- getwd()
#setwd("./R")

#####analyses:
source("L:Users (Initial Folders)/DT/R/grindflo.R");grindfloVal1()
source("L:Users (Initial Folders)/DT/R/grindfloV2.R");func.grindflo.val2()
source("L:Users (Initial Folders)/DT/R/func.full.mince.analysis.R"); func.full.mince.analysis(F)




####MERCK machines only
  source("L:Users (Initial Folders)/DT/R/func.full.mince.analysis.R"); func.full.mince.analysis(F)
####MERCK machines only


#####killsheets:

#waterloo
source("L:Users (Initial Folders)/DT/R/TWL.R");func.TWLKS()
#Lex
source("L:Users (Initial Folders)/DT/R/TLXKS.basic.R"); func.basic.TLXKS("DT", SEL = F)
#BulkFPL
source("L:Users (Initial Folders)/DT/R/bulk.FPLKS.R");func.bulk.FPLKS("DT")
#Tyson FC
source("L:Users (Initial Folders)/DT/R/TFCKS.R"); func.TFCKS("DT")
#FPLC
source("L:Users (Initial Folders)/DT/R/FPLC.R"); func.FPLC("DT")


