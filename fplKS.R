fplKS <- function(a, initials = "AL"){
#rename duplicate samples across the two potential fields for duplication for killsheets
#obligatories
  vers <- "v 1.3" #I'M JUST STUPID AND FEEL BAD ABOUT MYSELF
#1.1: only prints text if necessary, but still does everything because it's supposed to.
#1.2: it was randomly creating duplicate barcodes, so i added a test before the renumbering.
  test <- read.csv(a, header = T, blank.lines.skip = T)
  header <- c("Taken",	"SideID",	"CarcassID",	"Sample Barcode",	"Lot",	"Producer",	"Kill Date",	"Kill No",	"Back Tag",	"Ear Tag",	"Side",	"Status",	"Description",	"Username",	"Forename",	"Surname")
  
#obligatory shit this could all be streamlined but i'm much too lazy
  tg <- test$CarcassID
  gitgud_tg <- tg[tg %in% unique(tg[duplicated(tg)])]
  tg_x <- unique(gitgud_tg)
  td <- test$Sample.Barcode
  gitgud_td <- td[td %in% unique(td[duplicated(td)])]
  td_x <- unique(gitgud_td)
#Update printing
  if(length(gitgud_tg) > 0 | length(gitgud_td) > 0){
    print("Date: "); print(a)
    print("Total # samples:"); print(length(td))
    print("Carc dupes:");print(length(gitgud_tg))
    print("Sample ID dupes:");print(length(gitgud_td))
  }
  
  #rename dups in Carc ONLY IF dups exist
  if(length(tg_x)>0) {
    for(i in seq_along(tg_x)){
      testloop <- test[test$CarcassID==tg_x[i],]
      testlooplen <- as.numeric(paste(-dim(testloop)[1]))
      testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
      newvals <- as.numeric(paste(testloopvector,testloop$CarcassID, sep=""))
      test$CarcassID[test$CarcassID==tg_x[i]] <- newvals
    }
  }
  
  #rename dups in Sample.ID ONLY IF dups exist
  #for some reason this broke and replaced $Sample.Barcode with $DNA. Check here if it borks again?
  if(length(td_x)>0){
    for(i in seq_along(td_x)){
      testloop <- test[test$Sample.Barcode==td_x[i],]
      testlooplen <- as.numeric(paste(-dim(testloop)[1]))
      testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
      test$Sample.Barcode[test$Sample.Barcode==td_x[i]] <- as.numeric(paste(testloopvector,testloop$Sample.Barcode, sep=""))
    }
  }
#file naming/output
  names(test) <- header
  SysDa <- as.character((Sys.Date()))
  a <- sub(".csv", "", a)
  output_a <- paste(a, initials,vers, SysDa, sep = "-"); 
  output_a <- paste(output_a, ".csv", sep = "")
  write.csv(test,file = output_a, row.names = F)
}
