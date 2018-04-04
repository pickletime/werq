fplKS <- function(a, initials = "DT"){
#obligatories
  vers <- "v_1.1" #version tracking? idk maybe this is a thing
#1.1: only prints text if necessary, but still does everything because it's supposed to.
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
  
#id dups in Carc
  for(i in seq_along(tg_x)){
    testloop <- test[test$CarcassID==tg_x[i],]
    testlooplen <- as.numeric(paste(-dim(testloop)[1]))
    testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
    newvals <- as.numeric(paste(testloopvector,testloop$CarcassID, sep=""))
    test$CarcassID[test$CarcassID==tg_x[i]] <- newvals
  }
#id dups in Sample.ID
  for(i in seq_along(td_x)){
    testloop <- test[test$Sample.Barcode==td_x[i],]
    testlooplen <- as.numeric(paste(-dim(testloop)[1]))
    testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
    test$Sample.Barcode[test$Sample.Barcode==td_x[i]] <- as.numeric(paste(testloopvector,testloop$Sample.Barcode, sep=""))
  }
  names(test) <- header
  
#file naming/output
  SysDa <- as.character((Sys.Date()))
  a <- sub(".csv", "", a)
  output_a <- paste(a, initials,vers, SysDa, sep = "-"); 
  output_a <- paste(output_a, ".csv", sep = "")
  write.csv(test,file = output_a, row.names = F)
}
