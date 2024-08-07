func.bulk.FPLKS <- function(initials = "DT"){
    #rename duplicate samples across the two potential fields for duplication for killsheets
    #obligatories
  vers <- "V 1.5"
    #1.5 2021-10-25: I added a grepl to the file listing function so that it ONLY pulls out csv. 
      #This could be problematic if filetype changes
    #1.4: took everything out of the (now defunct) fplkillsheetscript.R and put it into this, so it loops etc
      #I added a new dir into "/to be cleaned". Not sure it'll stay, I just wanted to do it.
    #1.3: I'M JUST STUPID AND FEEL BAD ABOUT MYSELF
    #1.2: it was randomly creating duplicate barcodes, so i added a test before the renumbering.  
    #1.1: only prints text if necessary, but still does everything because it's supposed to.
  source.wd <- "L:/Sample Records/FPL Food, Inc/Kill Sheets/to be cleaned"
  setwd(source.wd)
  killsheet.filenames <- list.files()
  #grepl'd in a test for csv, so i don't have to keep deleting all my shit
  killsheet.filenames <- killsheet.filenames[grepl(".csv", killsheet.filenames)]
  #grpl'd in a test for csv
  
  print(paste("files to processed and put into './cleaned files':", length(killsheet.filenames)))
  if(!dir.exists("./cleaned files")) {dir.create("./cleaned files")}
  for (i in 1:length(killsheet.filenames)){
    test <- read.csv(killsheet.filenames[i], header = T, blank.lines.skip = T)
    header <- c("Taken",	"SideID",	"CarcassID",	"BC",	"Lot",	"Producer",	"Kill Date",	"Kill No",	"Back Tag",	"Ear Tag",	"Side",	"Status",	"Description",	"Username",	"Forename",	"Surname")
    
    #obligatory shit this could all be streamlined but i'm much too lazy
    list.CN <- test$CarcassID
    duplicated.list.CN <- list.CN[list.CN %in% unique(list.CN[duplicated(list.CN)])]
    list.CN.unique <- unique(duplicated.list.CN)
    list.ID <- test$Sample.Barcode
    duplicated.list.ID <- list.ID[list.ID %in% unique(list.ID[duplicated(list.ID)])]
    list.ID.unique <- unique(duplicated.list.ID)
    #Update printing
    if(length(duplicated.list.CN) > 0 | length(duplicated.list.ID) > 0){
      print("Date: "); print(killsheet.filenames[i])
      print("Total # samples:"); print(length(list.ID))
      print("Carc dupes:");print(length(duplicated.list.CN))
      print("Sample ID dupes:");print(length(duplicated.list.ID))
    }
    
    #rename dups in Carc ONLY IF dups exist
    if(length(list.CN.unique)>0) {
      for(j in seq_along(list.CN.unique)){
        testloop <- test[test$CarcassID==list.CN.unique[j],]
        testlooplen <- as.numeric(paste(-dim(testloop)[1]))
        testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
        newvals <- as.numeric(paste(testloopvector,testloop$CarcassID, sep=""))
        test$CarcassID[test$CarcassID==list.CN.unique[j]] <- newvals
      }
    }
    
    #rename dups in Sample.ID ONLY IF dups exist
    #for some reason this broke and replaced $Sample.Barcode with $DNA. Check here if it borks again?
    if(length(list.ID.unique)>0){
      for(k in seq_along(list.ID.unique)){
        testloop <- test[test$Sample.Barcode==list.ID.unique[k],]
        testlooplen <- as.numeric(paste(-dim(testloop)[1]))
        testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
        test$Sample.Barcode[test$Sample.Barcode==list.ID.unique[k]] <- as.numeric(paste(testloopvector,testloop$Sample.Barcode, sep=""))
      }
    }
    #file naming/output
    setwd("./cleaned files")
    names(test) <- header
    SysDa <- as.character((Sys.Date()))
    a <- sub(".csv", "", killsheet.filenames[i])
    output_a <- paste(a, initials,vers, SysDa, sep = "-"); 
    output_a <- paste(output_a, ".csv", sep = "")
    write.csv(test,file = output_a, row.names = F)
    print(paste("i = ", i))  #this is for testing, can be taken out whenever
    setwd(source.wd)
  }
}
