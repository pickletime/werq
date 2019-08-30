chicago.bh <- function() {
  #DT, 2019-05-02: i wrote this to deal with an impossibly large datafile that consistently crashes excel. 
  #It's really as simple as it seems.
  #This can be pretty easily repurposed.
  #GLHF
  wd <- choose.dir()
    #HOLY SHIT IT'S THE FUTURE
  setwd(wd)
    #allows you paste unformatted filename. Turns backslash into forwardslash, which R requires. BASED GSUB.
  file.list <- list.files()
    #I wish i had a better way to do this.
  raw.file <- read.csv(file.list[grepl("dbe", file.list)])
    #ensure that you're opening the dbe file
  new.file <- raw.file[raw.file$LociUntyped != 40,]
    #make a new file out the samples that have fewer than 40 untyped loci
  write.csv(new.file,file = "empty rows removed.csv", row.names = F)
    #write resulting file with a super generic name
  print("file completed")
    #bc it takes fucking forever and it's nice to when things are done
}