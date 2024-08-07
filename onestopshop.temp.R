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


library(openxlsx)
library(ggplot2)

#JFC this actually works what the shit

date.func <- function(value){
  as.Date(value, origin = "1900-01-01")-2
} #the '-2' thing is fucked and i don't really understand why it works sometimes and not others. Keep an eye on this?

n_fun <- function(x){
  return(data.frame(y = median(x)*1.05, label = paste0("n = ",length(x))))
} #this exists purely for putting the #obs into boxplots thanks i hate it


correct.names <- c("cust", "PlateID", "lysed.inits", "lysed.dates", "Inc.temp", "reag.pro", 
                   "lysis.buff", "clean.inits", "clean.dates", "reag.beads", "reag.buff", "reag.w1", "reag.w2", "reag.el")

directoryextractor.func <- function(selected.file){
  filename.vector <- unlist(strsplit(selected.file,'\\\\', fixed = F))
  filename <- filename.vector[length(filename.vector)]
  setwd(substr(selected.file, 1, nchar(selected.file)-nchar(filename)))
}
#distance formula because i can't be bothered to look up if it already exists
distance.func <- function(x1, x2, y1, y2){sqrt((x2-x1)^2 + (y2-y1)^2)}
rename.func <- function(value){
  new.val <- as.integer(unlist(strsplit(value, NULL)))
  new.val.1 <- paste(c(new.val[5], new.val[6], new.val[1], new.val[2], 
                       new.val[3], new.val[4], '-', new.val[8], collapse = ""), collapse= "")
} #renames reagent lots that follow the naming scheme "mmddyy-n" to "yymmdd-n" SO EZ

#####

#wd <- "L:/DT/R/NGF/2023/pork"
wd <- "L:/DT/R/NGF/2023/PACE"
setwd(wd)

#####

unformatted.file <- file.choose()
directoryextractor.func(unformatted.file)
#if(!dir.exists("./cleaned files")) {dir.create("./cleaned files")}
#setwd("./cleaned files")
#df.final <- read.csv(file.choose())
file.list <- list.files()

for(a in 1:length(file.list)){
  working.file <- read.table(file = file.list[a], fill = T)
  #start new df from appropriate location
  df.working.file <- data.frame(working.file[(match("Data",working.file$V1)+2):nrow(working.file),1])
  
  #reformat df with appropriate headers
  df.final <- separate(data = df.working.file, col = 1, sep = "\\,",
                       into = c("DaughterPlate","MasterPlate","MasterWell","Call","X","Y","SNPID","SubjectID",
                                "Norm","Carrier","DaughterWell", "LongID", "AliquotID"))
  ##add in panel, test for completeness. Doing this for the main 3 should be easy enough.
  #porcID <- c("ALGA0001559", "H3GA0011528", "ASGA0043022", "MARC0096462", "H3GA0033394", "ASGA0068674", "M1GA0001071", "H3GA0019987", "ALGA0047477", "MARC0075680", "H3GA0030429", "ALGA0006150", "ALGA0010541", "ALGA0012456", "ALGA0014515", "ASGA0103938", "H3GA0008408", "ASGA0092767", "ASGA0093631", "MARC0017801", "ALGA0024130", "DIAS0004198", "MARC0026858", "ALGA0033921", "DRGA0006563", "MARC0058178", "ALGA0036718", "H3GA0022882", "MARC0067233", "ALGA0049258", "MARC0029757", "ALGA0055075", "ALGA0057529", "ALGA0060238", "ASGA0054436", "ALGA0067470", "H3GA0036968", "H3GA0037306", "H3GA0037982", "MARC0011128", "CASI0009023", "CASI0007894", "MARC0055746", "ALGA0089624", "MARC0098653", "MARC0050941", "ALGA0094758", "ASGA0083083")
  
  #save the trimmed/neat version. also: row.names=F is heaven.
  
  #######
  #######
  {
  well.list <- sort(unique(df.final$DaughterWell))
  snp.list <- unique(df.final$SNPID)
  snp.list <- snp.list[!is.na(snp.list)]
  plate.list <- unique(df.final$MasterPlate)
  uncalled <- c("?", "NTC", "", "Over", "DUPE", "Uncallable")
  }
  #######
  #######
  
  ######
  ######QQC
  ######
  {
    #I remove anything that's not called
    #for some reason it was pulling in samples where the snpid was NA, so i EXPLICITLY removed those
    
    df.final$x.mean <- NA
    df.final$y.mean <- NA
    df.final$distance <- NA
    
    
    #these are for testing
    df.final$x.norm <- NA
    df.final$y.norm <- NA
    df.final$distance.norm <- NA
    
    df.called <- df.final[!df.final$Call %in% uncalled,]
    df.uncalled <- df.final[df.final$Call %in% uncalled,]
    df.called <- df.called[!is.na(df.called$SNPID),]
    
    
  
    
    # df.called$x.mean <- NA
    # df.called$y.mean <- NA
    # df.called$distance <- NA
    # 
    # 
    # #these are for testing
    # df.called$x.norm <- NA
    # df.called$y.norm <- NA
    # df.called$distance.norm <- NA
    
    #fuck i have to redo this with
    
    subset.called.platelist <- unique(df.called$DaughterPlate)
    for(h in 1:length(subset.called.platelist)){
      print(paste("Array #", h, "of", length(subset.called.platelist)))
      #subsetting master DF for each plate
      subset.temp.plate <- df.called[df.called$DaughterPlate == subset.called.platelist[h],]
      #setting up subsetting for each GT/snp
      subset.called.snplist <- unique(subset.temp.plate$SNPID)
      for (i in 1:length(subset.called.snplist)){
        #print(paste("SNP #",i))
        #subsetting master DF for each snp
        subset.temp <- subset.temp.plate[subset.temp.plate$SNPID == subset.called.snplist[i],]
        #setting up subsetting for each GT/snp
        subset.called.calls <- unique(subset.temp$Call)
        for(j in 1:length(subset.called.calls)){
          #temporary dataframe for each genotype
          subset.temp.calls <- subset.temp[subset.temp$Call == subset.called.calls[j],]
          #x/y averages because I'm too dumb to do it any other way, the forced as numeric thing is sure something
          subset.temp.calls$x.mean <- mean(as.numeric(subset.temp.calls$X))
          subset.temp.calls$X <- as.numeric(subset.temp.calls$X)
          subset.temp.calls$y.mean <- mean(as.numeric(subset.temp.calls$Y))
          subset.temp.calls$Y <- as.numeric(subset.temp.calls$Y)
          #distance between each sample and the center of each cluster
          subset.temp.calls$distance <- with(subset.temp.calls,distance.func(X,x.mean, Y, y.mean))
          #reading those distances back into the master DF
          subset.temp[subset.temp$SNPID == subset.called.snplist[i] & subset.temp$Call == subset.called.calls[j],10:17] <- subset.temp.calls[,10:17]
          df.called[df.called$DaughterPlate == subset.called.platelist[h] & df.called$SNPID == subset.called.snplist[i] & df.called$Call == subset.called.calls[j],10:17] <- subset.temp.calls[,10:17]
        } #determine center for each GT
        subset.temp$x.norm <- as.numeric(subset.temp$X)/(subset.temp$x.mean)
        subset.temp$y.norm <- as.numeric(subset.temp$Y)/(subset.temp$y.mean)
        subset.temp$distance.norm <- with(subset.temp,distance.func(x.norm,x.mean, y.norm, y.mean))
        df.called[df.called$DaughterPlate == subset.called.platelist[h] & df.called$SNPID == subset.called.snplist[i], 17:19] <- subset.temp[,17:19]
      } #at the end of this you have the table with xmean, ymean, distance columns 
      df.master.list <- df.called
    }
  }
  ######
  ######QQC
  ######
  
  df.final <- rbind(df.called, df.uncalled)

  {
    unformatted.file <- sub(".csv", "", file.list[a])
    write.csv(df.final[,-c(9,10)], file = paste(sub("Genotyping", "Trimmed and formatted NGF", unformatted.file),sep = "", ".csv"), row.names = F)
    print(paste(sub("Genotyping", "Trimmed NGF", unformatted.file),sep = "", ".csv"))
  } #file writing
}


##the looping aggregator
{
file.list <- list.files()
git.file <- file.list[grepl("Trimmed and formatted",file.list)]

for(i in 1:length(git.file)){
  working.file <- read.csv(git.file[i], sep = ",")
  ifelse(
    i < 2,
    df.working.file <- data.frame(working.file[2:nrow(working.file),]),
    df.working.file <- rbind(df.working.file, data.frame(working.file[2:nrow(working.file),]))
  )
  print(dim(df.working.file)[1])
  print(100*(1-signif(i/length(git.file),3)))
} #reading in files in relevant location

#and now the end product
df.final <- df.working.file

#various unique lists, just for funsies
well.list <- sort(unique(df.final$DaughterWell))
snp.list <- unique(df.final$SNPID) 
uncalled <- c("?", "NTC", "", "Over")
raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)


#  I actually think this works now. No way it's that easy.
for(i in 1:length(well.list)){
  loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
  raw.output.table[i,1] <- as.character(well.list[i])
  raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
  print(signif(100*(i/length(well.list))),3)
} #averaging call rate across well location

#raw.output.table[,1]

#this one is fine
data.cols <- as.numeric(substr(raw.output.table[,1],2,3))
#this one is not fine i just need to figure this out
#I think this one is fine now?
data.rows <- substr(raw.output.table[,1],1,1)
fac <- factor(data.rows); levels(fac) <- 1:16; data.rows <- as.numeric(fac)
#and of course
raw.output.table[,2] <- as.numeric(raw.output.table[,2])


#once the data.rows is fine we'll be good to go
#this is now good to go?
data.table <- matrix(data = 0, nrow = length(unique(data.rows)), ncol = length(unique(data.cols)))

#pushing rows/cols into output df
raw.output.table[,3] <- data.rows
raw.output.table[,4] <- data.cols

#this took entirely too long to figure out
for(i in 1:length(unique(data.rows))){
  subset.output.table <- raw.output.table[raw.output.table[,3] == i,]
  for(j in 1:length(unique(data.cols))){
    ifelse(length(subset.output.table[subset.output.table[,4] == j,2]) >0 , 
           data.table[i,j] <- subset.output.table[subset.output.table[,4] == j,2], 0)
  }
  #print(100*signif(i/length(unique(data.rows)))) #this isn't useful but i'm scared to delete
} #building the average table?



#mean(data.table)

#head(data.table)


output.matrix <- 100*matrix(as.numeric(unlist(data.table)),nrow=nrow(data.table))
output.matrix[1:2,1:2] <- NA

plate.pass.rate <- heatmap.2(x = output.matrix, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                             cellnote = signif(output.matrix,2), notecol = "black", notecex = 0.5,
                             trace = "none", key = FALSE, xlab = "Column", ylab = "row", 
                             main = paste(length(unique(df.final$DaughterPlate)),"unique arrays"), col = colorRampPalette(c("white", "red"))(150))
#
#plot(ROX.mean.norm~output.matrix)
#
par(mar = c(1, 1, 1, 1))
par(mfrow=c(2,1))
boxplot(output.matrix, ylab = "average failure rate", xlab = "column index", main = "average failure rate per column", col = "orange", xaxt = 'n')
boxplot(t(output.matrix), ylab = "average failure rate", xlab = "row index", main = "average failure rate per row", col = "gold", xaxt = 'n')


#save the trimmed/neat version. also: row.names=F is heaven.

#write.csv(df.final[,-c(9,10)], file = paste("super bulk output", ".csv"), row.names = F)
#I have no idea why I was using this. Col 9 is the sisterplate well location, which is absolutely necessary.
write.csv(df.final, file = paste("compiled bulk output of",length(unique(df.final$SubjectID)), " samples.csv"), row.names = F)
}

######
######summary tables
######
{#I actually think this works now. No way it's that easy.
  well.list <- sort(unique(df.final$DaughterWell))
  mp.well.list <- sort(unique(df.final$MasterWell))
  snp.list <- unique(df.final$SNPID)
  snp.list <- snp.list[!is.na(snp.list)]
  plate.list <- unique(df.final$MasterPlate)
  #this is wrong. i need to correct this for sure
  uncalled <- c("?", "NTC", "", "Over", "DUPE", "Uncallable")
  raw.output.table <- matrix(data = NA, nrow = length(well.list), ncol = 4)
  mp.raw.output.table <- matrix(data = NA, nrow = length(mp.well.list), ncol = 4)
  
  #idkwtf this wasn't part originally
  plate.table <- data.frame(matrix(NA, nrow = length(plate.list), ncol = 4))
  colnames(plate.table) <- c("plate BC", "% failed", "average distance", "normalized distance")
  plate.table$`plate BC` <- plate.list
  
  snp.table <- data.frame(matrix(NA, nrow = length(snp.list), ncol = 4))
  colnames(snp.table) <- c("SNP", "% failed", "average distance", "normalized distance")
  snp.table$SNP <- snp.list
  
  df.master.list <- df.final[!df.final$Call %in% uncalled,]
  
  for(i in 1:length(well.list)){
    loop.df.subset <- df.final[df.final$DaughterWell == well.list[i],]
    raw.output.table[i,1] <-well.list[i]
    raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$DaughterWell))
    print(length(well.list)-i)
  }
  for(i in 1:length(mp.well.list)){
    loop.df.subset <- df.final[df.final$MasterWell == mp.well.list[i],]
    mp.raw.output.table[i,1] <-mp.well.list[i]
    mp.raw.output.table[i,2] <- as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$MasterWell))
    print(length(mp.well.list)-i)
  } ####this is the the MP failure rate mapping, this is pretty ez
  for(i in 1:length(plate.list)){
    loop.df.subset <- df.final[df.final$MasterPlate == plate.list[i],]
    loop.df.subset.qqc <- df.master.list[df.master.list$MasterPlate == plate.list[i],]
    plate.table[i,2] <- 100*as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$MasterPlate))
    plate.table[i,3] <- as.numeric(mean(loop.df.subset.qqc$distance))
    plate.table[i,4] <- as.numeric(mean(loop.df.subset.qqc$distance.norm))
    print(length(plate.list)-i)
  }
  for(i in 1:length(snp.list)){
    loop.df.subset <- df.final[df.final$SNPID == snp.list[i],]
    loop.df.subset.qqc <- df.master.list[df.master.list$SNPID == snp.list[i],]
    snp.table[i,2] <- 100*as.numeric(length(loop.df.subset$Call[loop.df.subset$Call %in% uncalled])/length(loop.df.subset$MasterPlate))
    snp.table[i,3] <- as.numeric(mean(loop.df.subset.qqc$distance))
    snp.table[i,4] <- as.numeric(mean(loop.df.subset.qqc$distance.norm))
    print(length(snp.list)-i)
  }
  
  data.cols <- as.numeric(substr(raw.output.table[,1],2,3))
  #this one is not fine i just need to figure this out
  data.rows <- substr(raw.output.table[,1],1,1)
  fac <- factor(data.rows); levels(fac) <- 1:16; data.rows <- as.numeric(fac)
  #and of course
  raw.output.table[,2] <- as.numeric(raw.output.table[,2])
  
  
  #once the data.rows is fine we'll be good to go
  data.table <- matrix(data = 0, nrow = length(unique(data.rows)), ncol = length(unique(data.cols)))
  
  #pushing rows/cols into output df
  raw.output.table[,3] <- data.rows
  raw.output.table[,4] <- data.cols
  
  #this took entirely too long to figure out
  for(i in 1:length(unique(data.rows))){
    subset.output.table <- raw.output.table[raw.output.table[,3] == i,]
    for(j in 1:length(unique(data.cols))){
      ifelse(length(subset.output.table[subset.output.table[,4] == j,2]) >0 , 
             data.table[i,j] <- subset.output.table[subset.output.table[,4] == j,2], 0)
    }
  }
  
}
######
######summary tables
######

######
######plotting
###### 
{
  #data.table[1:2,1:2] <- NA
  #i hate that this has to be here, but it does because FUCK EVERYTHING
  output.matrix <- 100*matrix(as.numeric(unlist(data.table)),nrow=nrow(data.table))
  rnam <-  c(LETTERS[seq( from = 1, to = 16 )])
  plate.pass.rate <- heatmap.2(x = output.matrix, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                               cellnote = signif(output.matrix,2), notecol = "black", notecex = 0.65,
                               par(mar = c(1,1,1,1)),
                               trace = "none", key = FALSE, xlab = "Column", ylab = "row", labRow = rnam, 
                               main = paste(length(unique(df.final$SubjectID)), "samples"), col = colorRampPalette(c("yellow","orange", "red"))(60))
  
  
  par(mfrow=c(1,1))
  plot(plate.table[3:2], main = "sisterplate: avg dist x failure rate")
  plot(snp.table[3:2], main = "SNP: avg dist x failure rate")
  
  par(mar = c(1,1,1,1))
  par(mfrow=c(2,1))
  hist(plate.table$`% failed`, breaks = 100, main = "distribution of plate failure rates", xlab = "failure rate")
  hist(plate.table$`average distance`, breaks = 50, main = "distribution of cluster tightness", xlab = "tightness")
  
  
  par(mfrow=c(2,1))
  boxplot(output.matrix, ylab = "average failure rate", xlab = "column index", main = "average failure rate per column", col = "orange")
  boxplot(t(output.matrix), ylab = "average failure rate", xlab = "row index", main = "average failure rate per row", col = "gold")
}
######
######plotting
###### 


# 
########################################ID003
##obligatories
{
library(openxlsx)
library(ggplot2)

#JFC this actually works what the shit

date.func <- function(value){
  as.Date(value, origin = "1900-01-01")-2
} #the '-2' thing is fucked and i don't really understand why it works sometimes and not others. Keep an eye on this?

n_fun <- function(x){
  return(data.frame(y = median(x)*1.05, label = paste0("n = ",length(x))))
} #this exists purely for putting the #obs into boxplots thanks i hate it


correct.names <- c("cust", "PlateID", "lysed.inits", "lysed.dates", "Inc.temp", "reag.pro", 
                   "lysis.buff", "clean.inits", "clean.dates", "reag.beads", "reag.buff", "reag.w1", "reag.w2", "reag.el")
}
##obligatories


#reading in ID003
path.file <- file.choose()
for(i in 1:8){
  if(i == 1){
    ID003 <- read.xlsx(xlsxFile = path.file, sheet = i, cols = c(1:14), rows = c(2:10000))
    colnames(ID003) <- correct.names
  }else{
    input.file <- read.xlsx(xlsxFile = path.file, sheet = i, cols = c(1:14), rows = c(2:10000))
    colnames(input.file) <- correct.names
    ID003 <- rbind(ID003,input.file)
  }
} #reading in the various tabs
##obligatories
#ID003 <- ID003[!is.na(ID003$clean.inits)]
#####
#####formatting
#####
{
  ID003$pass.rate <- 100-plate.table$`% failed`[match(ID003$PlateID, plate.table$`plate BC`, incomparables = NULL)]
  ID003$dist <- plate.table$`average distance`[match(ID003$PlateID, plate.table$`plate BC`)]
  ID003$dist.norm <- plate.table$`normalized distance`[match(ID003$PlateID, plate.table$`plate BC`)]
  
  #pruning
  ID003.complete <- ID003[!is.na(ID003$pass.rate),]
  ID003.complete <- ID003.complete[!is.na(ID003.complete$reag.pro),]
  ID003.complete <- ID003.complete[!is.na(ID003.complete$clean.inits),]
  
  #uppercasing/reformatting
  ID003.complete$lysed.inits <- toupper(ID003.complete$lysed.inits)
  ID003.complete$Inc.temp <- toupper(ID003.complete$Inc.temp)
  ID003.complete$clean.inits <- toupper(ID003.complete$clean.inits)
  
  ID003.complete$lysed.dates <- date.func(as.integer(ID003.complete$lysed.dates))
  ID003.complete$clean.dates <- date.func(as.integer(ID003.complete$clean.dates))
}
#####
#####formatting
#####


#plotting
#par(mar = c(1,1,1,1))
boxplot(ID003.complete$pass.rate~ID003.complete$clean.inits)
boxplot(ID003.complete$dist~ID003.complete$clean.inits)
boxplot(ID003.complete$dist.norm~ID003.complete$clean.inits)
boxplot(ID003.complete$pass.rate~ID003.complete$lysed.inits)
boxplot(ID003.complete$pass.rate~ID003.complete$reag.beads)
boxplot(ID003.complete$pass.rate~ID003.complete$clean.dates)
boxplot(ID003.complete$dist.norm~ID003.complete$clean.dates)
boxplot(ID003.complete$dist~ID003.complete$reag.beads)

ggplot(ID003.complete, aes(x = factor(clean.inits), y = pass.rate, fill = factor(clean.inits))) + 
  #scale_fill_brewer(palette="Set3") + 
  geom_boxplot() +
  #geom_violin(trim=FALSE) + 
  stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) 
  #stat_boxplot(geom="errorbar",width=0.40)
  

#LOL that was ez, add in tables for all vars
##########
##########std vars
##########

rename.func <- function(value){
  new.val <- as.integer(unlist(strsplit(value, NULL)))
  new.val.1 <- paste(c(new.val[5], new.val[6], new.val[1], new.val[2], 
                       new.val[3], new.val[4], '-', new.val[8], collapse = ""), collapse= "")
} #renames reagent lots that follow the naming scheme "mmddyy-n" to "yymmdd-n" SO EZ

limit <- 10
{
lysed.tab <- table(ID003.complete$lysed.inits)
lysed.min <- as.numeric(sort(lysed.tab[lysed.tab>limit])[1])
cleaned.tab <- table(ID003.complete$clean.inits)
cleaned.min <- as.numeric(sort(cleaned.tab[cleaned.tab>limit])[1])
reag.pro.tab <- table(ID003.complete$reag.pro)
reag.pro.min <- as.numeric(sort(reag.pro.tab[reag.pro.tab>limit])[1])
lysis.buff.tab <- table(ID003.complete$lysis.buff)
lysis.buff.min <- as.numeric(sort(lysis.buff.tab[lysis.buff.tab>limit])[1])
reag.beads.tab <- table(ID003.complete$reag.beads)
reag.beads.min <- as.numeric(sort(reag.beads.tab[reag.beads.tab>limit])[1])
reag.buff.tab <- table(ID003.complete$reag.buff)
reag.buff.min <- as.numeric(sort(reag.buff.tab[reag.buff.tab>limit])[1])
reag.w1.tab <- table(ID003.complete$reag.w1)
reag.w1.min <- as.numeric(sort(reag.w1.tab[reag.w1.tab>limit])[1])
reag.w2.tab <- table(ID003.complete$reag.w2)
reag.w2.min <- as.numeric(sort(reag.w2.tab[reag.w2.tab>limit])[1])
reag.el.tab <- table(ID003.complete$reag.el)
reag.el.min <- as.numeric(sort(reag.el.tab[reag.el.tab>limit])[1])

clean.inits.unique <- names(cleaned.tab[cleaned.tab>limit])
lysed.inits.unique <- names(lysed.tab[lysed.tab>limit])
reag.pro.unique <- names(reag.pro.tab[reag.pro.tab>limit])
lysis.buff.unique <- names(lysis.buff.tab[lysis.buff.tab>limit])
reag.beads.unique <- names(reag.beads.tab[reag.beads.tab>limit])
reag.buff.unique <- names(reag.buff.tab[reag.buff.tab>limit])
reag.w1.unique <- names(reag.w1.tab[reag.w1.tab>limit])
reag.w2.unique <- names(reag.w2.tab[reag.w2.tab>limit])
reag.el.unique <- names(reag.el.tab[reag.el.tab>limit])


for(i in 1:length(clean.inits.unique)){
  if(i == 1){df.compar.clean <- ID003.complete[sample(which(ID003.complete$clean.inits == clean.inits.unique[i]),cleaned.min),]}
  else{df.compar.clean <- rbind(df.compar.clean, ID003.complete[sample(which(ID003.complete$clean.inits == clean.inits.unique[i]), cleaned.min),])}
}
for(i in 1:length(lysed.inits.unique)){
  if(i == 1){df.compar.lysed <- ID003.complete[sample(which(ID003.complete$lysed.inits == lysed.inits.unique[i]),lysed.min),]}
  else{df.compar.lysed <- rbind(df.compar.lysed, ID003.complete[sample(which(ID003.complete$lysed.inits == lysed.inits.unique[i]), lysed.min),])}
}
for(i in 1:length(reag.pro.unique)){
  if(i == 1){df.compar.pro <- ID003.complete[sample(which(ID003.complete$reag.pro == reag.pro.unique[i]),reag.pro.min),]}
  else{df.compar.pro <- rbind(df.compar.pro, ID003.complete[sample(which(ID003.complete$reag.pro == reag.pro.unique[i]),reag.pro.min),])}
}
for(i in 1:length(lysis.buff.unique)){
  if(i == 1){df.compar.lysis.buff <- ID003.complete[sample(which(ID003.complete$lysis.buff == lysis.buff.unique[i]),lysis.buff.min),]}
  else{df.compar.lysis.buff <- rbind(df.compar.lysis.buff, ID003.complete[sample(which(ID003.complete$lysis.buff == lysis.buff.unique[i]),lysis.buff.min),])}
}
for(i in 1:length(reag.beads.unique)){
  if(i == 1){df.compar.reag.beads <- ID003.complete[sample(which(ID003.complete$reag.beads == reag.beads.unique[i]),reag.beads.min),]}
  else{df.compar.reag.beads <- rbind(df.compar.reag.beads, ID003.complete[sample(which(ID003.complete$reag.beads == reag.beads.unique[i]),reag.beads.min),])}
}
for(i in 1:length(reag.buff.unique)){
  if(i == 1){df.compar.reag.buff <- ID003.complete[sample(which(ID003.complete$reag.buff == reag.buff.unique[i]),reag.buff.min),]}
  else{df.compar.reag.buff <- rbind(df.compar.reag.buff, ID003.complete[sample(which(ID003.complete$reag.buff == reag.buff.unique[i]),reag.buff.min),])}
}
for(i in 1:length(reag.w1.unique)){
  if(i == 1){df.compar.reag.w1 <- ID003.complete[sample(which(ID003.complete$reag.w1 == reag.w1.unique[i]),reag.w1.min),]}
  else{df.compar.reag.w1 <- rbind(df.compar.reag.w1, ID003.complete[sample(which(ID003.complete$reag.w1 == reag.w1.unique[i]),reag.w1.min),])}
}
for(i in 1:length(reag.w2.unique)){
  if(i == 1){df.compar.reag.w2 <- ID003.complete[sample(which(ID003.complete$reag.w2 == reag.w2.unique[i]),reag.w2.min),]}
  else{df.compar.reag.w2 <- rbind(df.compar.reag.w2, ID003.complete[sample(which(ID003.complete$reag.w2 == reag.w2.unique[i]),reag.w2.min),])}
}
for(i in 1:length(reag.el.unique)){
  if(i == 1){df.compar.reag.el <- ID003.complete[sample(which(ID003.complete$reag.el == reag.el.unique[i]),reag.el.min),]}
  else{df.compar.reag.el <- rbind(df.compar.reag.el, ID003.complete[sample(which(ID003.complete$reag.el == reag.el.unique[i]),reag.el.min),])}
}

ID003.cleaned <- ID003.complete[ID003.complete$lysed.inits %in% lysed.inits.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$clean.inits %in% clean.inits.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$reag.pro %in% reag.pro.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$lysis.buff %in% lysis.buff.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$reag.beads %in% reag.beads.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$reag.buff %in% reag.buff.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$reag.w1 %in% reag.w1.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$reag.w2 %in% reag.w2.unique,]
ID003.cleaned <- ID003.cleaned[ID003.cleaned$reag.el %in% reag.el.unique,]

#specific for each var:
ID003.cleaned.lysed.inits <- ID003.complete[ID003.complete$lysed.inits %in% lysed.inits.unique,]
ID003.cleaned.clean.inits <- ID003.complete[ID003.complete$clean.inits %in% clean.inits.unique,]
ID003.cleaned.reag.pro <- ID003.complete[ID003.complete$reag.pro %in% reag.pro.unique,]
  for(i in 1:length(reag.pro.unique)){
  ID003.cleaned.reag.pro$reag.pro[ID003.cleaned.reag.pro$reag.pro == reag.pro.unique[i]] <- rename.func(reag.pro.unique[i])
} #renames pro reag lot
ID003.cleaned.lysis.buff <- ID003.complete[ID003.complete$lysis.buff %in% lysis.buff.unique,]
  for(i in 1:length(lysis.buff.unique)){
  ID003.cleaned.lysis.buff$lysis.buff[ID003.cleaned.lysis.buff$lysis.buff == lysis.buff.unique[i]] <- rename.func(lysis.buff.unique[i])
} #renames lysis buff lot
ID003.cleaned.reag.beads <- ID003.complete[ID003.complete$reag.beads %in% reag.beads.unique,]
ID003.cleaned.reag.buff <- ID003.complete[ID003.complete$reag.buff %in% reag.buff.unique,]
ID003.cleaned.reag.w1 <- ID003.complete[ID003.complete$reag.w1 %in% reag.w1.unique,]
  for(i in 1:length(reag.w1.unique)){
  ID003.cleaned.reag.w1$reag.w1[ID003.cleaned.reag.w1$reag.w1 == reag.w1.unique[i]] <- rename.func(reag.w1.unique[i])
} #renames w1 reag lot
ID003.cleaned.reag.w2 <- ID003.complete[ID003.complete$reag.w2 %in% reag.w2.unique,]
  for(i in 1:length(reag.w2.unique)){
  ID003.cleaned.reag.w2$reag.w2[ID003.cleaned.reag.w2$reag.w2 == reag.w2.unique[i]] <- rename.func(reag.w2.unique[i])
} #renames w2 reag lot
ID003.cleaned.reag.el <- ID003.complete[ID003.complete$reag.el %in% reag.el.unique,]
  for(i in 1:length(reag.el.unique)){
  ID003.cleaned.reag.el$reag.el[ID003.cleaned.reag.el$reag.el == reag.el.unique[i]] <- rename.func(reag.el.unique[i])
} #renames el reag lot
}
##########
##########std vars
##########



##########
##########std plots
##########
{
  ggplot(ID003.cleaned, aes(x = factor(clean.inits), y = pass.rate, fill = factor(clean.inits))) + 
    #scale_fill_brewer(palette="Set3") + 
    geom_boxplot() +
    ggtitle("Cleaning performance", ) + 
    xlab("Initials") + 
    ylab("Plate pass rate") +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.1)) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  ggplot(ID003.cleaned.lysed.inits, aes(x = factor(lysed.inits), y = pass.rate, fill = factor(lysed.inits))) + 
#    scale_fill_brewer(palette="Set3") + 
    geom_boxplot() +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
    theme(legend.position = "none")
  
  ggplot(ID003.cleaned.reag.pro, aes(x = factor(reag.pro), y = pass.rate, fill = factor(reag.pro))) + 
#    scale_fill_brewer(palette="Set3") + 
    geom_boxplot() +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
    theme(legend.position = "none")
  
  ggplot(ID003.cleaned.lysis.buff, aes(x = factor(lysis.buff), y = pass.rate, fill = factor(lysis.buff))) + 
#    scale_fill_brewer(palette="Set3") + 
    geom_boxplot() +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
    theme(legend.position = "none")
  
  ggplot(ID003.cleaned.reag.beads, aes(x = factor(reag.beads), y = pass.rate, fill = factor(reag.beads))) + 
    ggtitle("Pass rate of lots of binding beads", ) + 
#    xlab("Binding bead reagent lot") + 
    ylab("Plate pass rate") +
    guides(fill=guide_legend(title = "Lot"),
           legend.position = "center") +
    geom_boxplot() +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
    #theme(legend.position = "none", axis.title.x = element_text(angle = 90))
    theme(axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.title.x = element_blank(), 
          legend.title.align=0.5,
          plot.title = element_text(hjust = 0.5))
  
  ggplot(ID003.cleaned.reag.w1, aes(x = factor(reag.w1), y = pass.rate, fill = factor(reag.w1))) +
    #ggplot(ID003.cleaned.reag.w1, aes(x = factor(reag.w1), y = dist.norm, fill = factor(reag.w1))) + 
#    scale_fill_brewer(palette="Set3") + 
    geom_boxplot() +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
    theme(legend.position = "none")
  
  ggplot(ID003.cleaned.reag.w2, aes(x = factor(reag.w2), y = pass.rate, fill = factor(reag.w2))) + 
#    scale_fill_brewer(palette="Set3") + 
    geom_boxplot() +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
    theme(legend.position = "none")
  
  ggplot(ID003.cleaned.reag.el, aes(x = factor(reag.el), y = pass.rate, fill = factor(reag.el))) + 
#    scale_fill_brewer(palette="Set3") + 
    geom_boxplot() +
#    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
    theme(legend.position = "none")
}
##########
##########std plots
##########




ggplot(ID003.cleaned.reag.beads, aes(x = factor(clean.dates), y = pass.rate, fill = factor(reag.beads))) +
  scale_fill_brewer(palette="Set3") +
  ggtitle("Data quality over time", ) + 
  ylab("Plate pass rate") +
  geom_boxplot() +
  guides(fill=guide_legend(title = "Binding Bead Lot"),
         legend.position = "center") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(), 
        legend.title.align=0.5,
        plot.title = element_text(hjust = 0.5))

ggplot(ID003.cleaned.clean.inits, aes(x = factor(clean.dates), y = dist.norm, fill = factor(clean.inits))) +
  scale_fill_brewer(palette="Set3") +
  geom_boxplot() +
  facet_grid(rows = factor(ID003.cleaned.clean.inits$clean.inits)) +
  theme(strip.text.y = element_text(size = 8, angle = 0))

 # ggplot(df.final, aes(x = factor(SNPID), y = distance, fill = factor(SNPID))) + 
 #   #scale_fill_brewer(palette="Set3") + 
 #   geom_boxplot() +
 #  # stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) +
 #   theme(legend.position = "none")

ggplot(ID003.cleaned.reag.pro, aes(x = factor(clean.dates), y = pass.rate, fill = factor(reag.pro))) + 
  geom_boxplot()

ggplot(ID003.cleaned.reag.w1, aes(x = factor(clean.dates), y = pass.rate, fill = factor(reag.w1))) + 
  geom_boxplot()

ggplot(ID003.cleaned.clean.inits, aes(x = factor(clean.dates), y = pass.rate, fill = factor(clean.inits))) + geom_boxplot()

ggplot(ID003.complete, aes(x = factor(clean.dates), y = pass.rate, fill = factor(clean.dates))) + 
  ggtitle("Masterplate data quality over time", ) + 
  xlab("Date") + 
  ylab("Plate call rate") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + 
  geom_boxplot()

ggplot(ID003.complete, aes(pass.rate)) + geom_histogram(binwidth = 0.5, color="blue", fill="white")
ggplot(ID003.complete, aes(dist)) + geom_histogram(binwidth = 0.0025, color="gold", fill="white")
ggplot(ID003.complete, aes(dist.norm)) + geom_histogram(binwidth = 0.05, color="red", fill="white")
#################################
#################################GLM shit
#################################
ID003.glm <- ID003.complete[,-c(1, 2, 5, 16, 17)] #take out dist/dist.norm because they explain so much of the variation, not necessarily informative
ID003.glm <- ID003.cleaned[,-c(1, 2, 5, 16, 17)] #take out dist/dist.norm because they explain so much of the variation, not necessarily informative
ID003.dist.glm <- ID003.cleaned[-c(1, 15, 16)]
glm_ID003 <- glm(pass.rate~., data = ID003.glm); summary(glm_ID003)
glm_dist <- glm(dist.norm~., data = ID003.dist.glm); summary(glm_dist)
glm_variable_ID03 <- glm(pass.rate~reag.beads, data = ID003.complete); summary(glm_variable_ID03)
glm_cleaned_ID03 <- glm(pass.rate~lysed.inits+clean.inits, data = ID003.cleaned)
glm_ID003 <- glm(pass.rate~., data = ID003.glm); summary(glm_ID003)
glm_distance_ID03 <- glm(dist~lysed.inits+clean.inits, data = ID003.complete)

#aov(summary(glm_ID003))
toselect.x <- summary(glm_ID003)$coeff[-1,4] < 0.0005 # credit to kith
sig.vars <- data.frame(summary(glm_ID003)$coeff)
sig.vars <- sig.vars[order(sig.vars$Pr...t..),]
#!!!!sig.vars is now a table of significant factors explaining variance in data quality

relevant.x <- names(toselect.x)[toselect.x == TRUE] 
#sig.formula <- as.formula(paste("y ~",paste(relevant.x, collapse= "+")))
#sig.model <- glm(formula=sig.formula,data=mydata)

#k_fold_cv_error <- cv.glm(ID003.cleaned, glm_cleaned_ID03)
#k_fold_cv_error <- cv.glm(ID003.complete, glm_passrate_ID03)
       