library("data.table")
library("dplyr")
drop.level.func <- function(item){as.double(levels(drop.levels(item)))}
if (!require("SpadeR")) {
  install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
if (!require("gdata")) {
  install.packages("gdata", dependencies = TRUE); library(gdata)}
if (!require("openxlsx")) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
if (!require("formattable")) {
  install.packages("formattable", dependencies = TRUE); library(scales)}

#data is the failure rate/pass rate table
data <- read.csv(file.choose())
asdfasdf <- gsub("",NA, data[1,])
data[,2] <- data[,2]*100

#data <- data[,-9]

#qqc are ID003?
qqc.a <- read.csv(file.choose())
qqc.b <- read.csv(file.choose())

#qqc <- rbind(qqc.a, qqc.b)
#qqc <- qqc[,-c(1,5,6)]
#data$distance <- qqc.a$mean.distance[match(data$`Plate ID`,qqc.a$Plate.ID)]

qqc.c <- qqc.a
data.old <- data

qqc.c$pass.rate <- data$Pass.rate[match(qqc.c$Plate.ID, data$Plate.ID)]
data <- qqc.c


data$pass.rate <- as.numeric(data$`pass rate`)

#shifting all uppercase to remove wonkies?
data$lysed.inits <- sub("\\,", "", data$lysed.inits)
#data$Lysed <- sub("\\s+", "", data$Lysed)
data$incubator.at.temp <- sub("\\,", "", data$incubator.at.temp)
data$cleaned.inits <- sub("\\,", "", data$cleaned.inits)

#force uppercase because initials
data$lysed.inits <- toupper(data$lysed.inits)
#data$`Incubator temp. is 55° ± 3° C (initial)` <- toupper(data$`Incubator temp. is 55° ± 3° C (initial)`)
data$cleaned.inits <- toupper(data$cleaned.inits)

#copy file just in cases
raw.file <- data

#remove cases without tech initials, end result is a (mostly) complete file
data <- data[data$lysed.inits !="" & data$lysed.inits !="---",]; unique(data$lysed.inits)
data <- data[data$cleaned.inits !="" & data$cleaned.inits !="---",]; unique(data$cleaned.inits)


#looking only at complete cases:
complete <- data[data$Pronase.E.or.Proteinase.K != "" & data$Lysis.buffer != "" & 
             data$Binding.beads != "" & data$Binding.buffer != "" & data$Wash.1 != "" &
             data$Wash.2 != "" & data$Elution != "",]
complete_raw <- complete
completed <- complete[,-c(15:21),]
complete <- completed

#force the level variables into characters
completed$Customer <- as.character(completed$Customer)
completed$Pronase.E.or.Proteinase.K <- as.character(completed$Pronase.E.or.Proteinase.K)
completed$Lysis.buffer <- as.character(completed$Lysis.buffer)
completed$Binding.beads <- as.character(completed$Binding.beads)
completed$Binding.buffer <- as.character(completed$Binding.buffer)
completed$Wash.1 <- as.character(completed$Wash.1)
completed$Wash.2 <- as.character(completed$Wash.2)
completed$Elution <- as.character(completed$Elution)

#force the variables that should be dates into date format using format(as.Date(VARIABLE, '%m/%d/%Y'), '%Y/%m/%d')
completed$lysed.dates <- format(as.Date(completed$lysed.dates, '%m/%d/%Y'), '%Y/%m/%d')
completed$cleaned.date <- format(as.Date(completed$cleaned.date, '%m/%d/%Y'), '%Y/%m/%d')

{

#hist(data$`pass rate`, breaks = 100)
hist(data$pass.rate, breaks = 100)
abline(v = mean(data$pass.rate), col = "red", lwd = 2)


hist(complete$pass.rate, breaks = 20)
abline(v = mean(complete$pass.rate), col = "red", lwd = 4)

}
##holy shit this works
completed.a <- completed[!is.na(completed$pass.rate),]
completed <- completed.a

for(i in 1:(length(colnames(completed))-2))
{
  boxplot(completed[,15] ~ completed[,i], ylab = "pass rate", xlab = colnames(completed)[i])
}
#force the variables that exist as levels to characters, using as.character()
#force the variables that should be dates into date format using format(as.Date(VARIABLE, '%m/%d/%Y'), '%Y/%m/%d')
  #YMD because it's easier to sort

boxplot(completed$pass.rate ~ completed$cleaned.inits*completed$lysed.inits)

boxplot(complete$`pass rate`~complete$`Binding buffer`*complete$Cleaned)
boxplot(complete$`pass rate`~complete$`Binding beads`*complete$Cleaned)
boxplot(complete$`pass rate`~complete$`Wash 1`*complete$Cleaned)
boxplot(complete$`pass rate`~complete$`Wash 2`*complete$Cleaned)

boxplot(complete$`pass rate`~complete$`Wash 1`)
boxplot(complete$`pass rate`~complete$`Wash 2`)
boxplot(complete$`pass rate`~complete$`Wash 1`*complete$`Wash 2`)
boxplot(complete$`pass rate`~complete$`Wash 1`*complete$`Wash 2`*complete$`Lysis buffer`)
boxplot(complete$`pass rate`~complete$Date)


#hist(data$pass.rate~data$Cleaned)
boxplot(data$`pass rate`~data$Cleaned)
boxplot(data$distance~data$Cleaned)
boxplot(data$distance~data$Lysed)
boxplot(data$pass.rate~data$`Pronase E or Proteinase K`)
boxplot(data$pass.rate~data$Lysed)
boxplot(data$pass.rate~data$`Lysis buffer`)
boxplot(data$pass.rate~data$`Binding beads`)
boxplot(data$pass.rate~data$`Bindingbuffer`)
boxplot(data$pass.rate~data$`Wash 1`)
boxplot(data$pass.rate~data$`Wash 2`)
boxplot(data$pass.rate~data$Elution)

#nrow(data[data$Pronase.E.or.Proteinase.K !="" | data$Lysis.buffer !="" | data$Binding.beads != "" | data$Binding.buffer != "",])

datumulae <- data[data$Pronase.E.or.Proteinase.K !="" & data$Lysis.buffer !="" & data$Binding.beads != "" & data$Binding.buffer != "" & data$Wash.1 != ""& data$Wash.2 != ""& data$Elution != "",]
datums <- data[data$Pronase.E.or.Proteinase.K !="" | data$Lysis.buffer !="" | data$Binding.beads != "" | data$Binding.buffer != "",]

boxplot(datumulae$pass.rate~datumulae$Pronase.E.or.Proteinase.K)
boxplot(datumulae$pass.rate~datumulae$Lysis.buffer)
boxplot(datumulae$pass.rate~datumulae$Binding.beads)
boxplot(datumulae$pass.rate~datumulae$Binding.buffer)
boxplot(datumulae$pass.rate~datumulae$Wash.1)
boxplot(datumulae$pass.rate~datumulae$Wash.2)
boxplot(datumulae$pass.rate~datumulae$Elution)

####################################################
##bulk datums
####################################################

bulk.file <- fread("P:/data analysis bulked/CC data quality/massive bulk managen export RE bovine data quality dt 012121.csv")
boxplot(bulk.file$percent~bulk.file$Month*bulk.file$Year)

boxplot(bulk.file$percentBad~bulk.file$Year)
boxplot(bulk.file$percentBad~bulk.file$SNPName*bulk.file$MultiBreed_ID)

mb40 <- bulk.file[bulk.file$MultiBreed_40== TRUE,]
boxplot(mb40$percentBad~mb40$SNPName*mb40$Month)
mb40.2020 <- mb40[mb40$Year == 2020,]

boxplot(mb40.2020$percentBad~mb40.2020$Month)


glm_data <- glm(percentBad~SNPName+Year+Month, data = bulk.file)
k_fold_cv_error <- cv.glm(bulk.file, glm_data)


####################################################
##machine learning with ID03?
####################################################

library("data.table")
library("dplyr")
library("tidyverse")
library("reshape2")
library('boot')

data <- fread("P:/ID003/dredgeable data.csv")
asdfasdf <- gsub("",NA, data[1,])
data[,2] <- data[,2]*100

data$pass.rate <- as.numeric(data$`pass rate`)

#shifting all uppercase to remove wonkies?
data$Lysed <- sub("\\,", "", data$Lysed)
#data$Lysed <- sub("\\s+", "", data$Lysed)
data$`Incubator temp. is 55° ± 3° C (initial)` <- sub("\\,", "", data$`Incubator temp. is 55° ± 3° C (initial)`)
data$Cleaned <- sub("\\,", "", data$Cleaned)

#force uppercase because initials
data$Lysed <- toupper(data$Lysed)
#data$`Incubator temp. is 55° ± 3° C (initial)` <- toupper(data$`Incubator temp. is 55° ± 3° C (initial)`)
data$Cleaned <- toupper(data$Cleaned)

#copy file just in cases
raw.file <- data

#remove cases without tech initials, end result is a (mostly) complete file
data <- data[data$Lysed !="" & data$Lysed !="---",]; unique(data$Lysed)
data <- data[data$Cleaned !="" & data$Cleaned !="---",]; unique(data$Cleaned)


#looking only at complete cases:
complete <- data[data$`Pronase E or Proteinase K` != "" & data$`Lysis buffer` != "" & 
                   data$`Binding beads` != "" & data$`Binding buffer` != "" & data$`Wash 1` != "" &
                   data$`Wash 2` != "" & data$Elution != ""]


glm_passrate_ID03 <- glm(`pass rate`~Lysed+Cleaned, data = complete)
glm_distance_ID03 <- glm(distance~Lysed+Cleaned, data = data)

glm_passrate_ID03 <- glm(`pass rate`~., data = complete)
glm_distance_ID03 <- glm(distance~., data = data)
glm_distance_complete_ID03 <- glm(distance~., data = complete)


levels(complete)

par(mar = c(1, 1, 1, 1))
par(mfrow=c(2,2))
boxplot(data$distance~data$Lysed)
boxplot(data$distance~data$Cleaned)
boxplot(data$`pass rate`~data$Lysed)
boxplot(data$`pass rate`~data$Cleaned)


k_fold_cv_error <- cv.glm(complete, glm_ID03)
