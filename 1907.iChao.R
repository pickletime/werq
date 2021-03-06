iChao <- function(){
#i'm pretty sure the library section below is smart enough? hopefully.
  if (!require("SpadeR")) {
    install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}

#i thought this was going to be used more so I function'd it. Now it's used once but i can't bring myself to remove it.
  dropthebass <- function(item){as.double(levels(drop.levels(item)))}
  setwd(choose.dir())
  read.in <- read.csv(file.choose(), header = FALSE,  encoding = "UTF-8", stringsAsFactors = FALSE)
  num.tasks <- ncol(read.in)

#
##maybe add in samples?
#

  output <- matrix(data = NA, nrow = num.tasks, ncol = 3)

  for(i in 1:num.tasks){
    #I added a test here for if doubletons write nothing so that it doesn't break when we run primals
    if(length(read.in[read.in[,i]>1,i])==0) {
      output[i,] <- rbind(as.vector(c("0", "0", "0")))
    } else {
      input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
      output[i,] <- rbind(as.vector(c(dropthebass(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
    }
  }
  colnames(output) <- c("Profiles", "Nest", "Nest UCI")
  write.csv(output, file = paste("iChao estimates for ", num.tasks, " tasks ", as.character((Sys.Date())), ".csv", sep = ""))
  print("iChao'd")
}
