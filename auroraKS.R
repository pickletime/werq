auroraKS <- function(a, initials = "DT"){
#rename duplicate samples across the two potential fields for duplication for killsheets
#obligatories
  vers <- "v_1.2" #version tracking? idk maybe this is a thing
#1.1: only prints text if necessary, but still does everything because it's supposed to.
#1.2: it was randomly creating duplicate barcodes, so i added a test before the renumbering.
  test <- read.csv(a, header = T, blank.lines.skip = T)
  header <- c("DNA #", "Lot #", "Glatt #", "Breed", "Date", "Weight")
  
#obligatory shit this could all be streamlined but i'm much too lazy
  tg <- test$Glatt..
  gitgud_tg <- tg[tg %in% unique(tg[duplicated(tg)])]
  tg_x <- unique(gitgud_tg)
  td <- test$DNA..
  gitgud_td <- td[td %in% unique(td[duplicated(td)])]
  td_x <- unique(gitgud_td)
  
#Update printing
  if(length(gitgud_tg) > 0 | length(gitgud_td) > 0){
    print("Date: "); print(a)
    print("Total # samples:"); print(length(td))
    print("Glatt dupes:");print(length(gitgud_tg))
    print("Sample ID dupes:");print(length(gitgud_td))
  }
  
  #rename dups in Glatt ONLY IF dups exist
  if(length(tg_x)>0) {
    for(i in seq_along(tg_x)){
      testloop <- test[test$Glatt..==tg_x[i],]
      testlooplen <- as.numeric(paste(-dim(testloop)[1]))
      testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
      newvals <- as.numeric(paste(testloopvector,testloop$Glatt.., sep=""))
      test$Glatt..[test$Glatt..==tg_x[i]] <- newvals
    }
  }
  #rename dups in DNA ONLY IF dups exist
  if(length(td_x)>0){
    for(i in seq_along(td_x)){
      testloop <- test[test$DNA..==td_x[i],]
      testlooplen <- as.numeric(paste(-dim(testloop)[1]))
      testloopvector <- as.numeric(seq(from = -1, to = testlooplen))
      test$DNA..[test$DNA..==td_x[i]] <- as.numeric(paste(testloopvector,testloop$DNA.., sep=""))
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
