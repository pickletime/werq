iChao <- function(){
  version <- "V 1.1"
  #1.1 7/24/19: added in basic maths. for some reason it keeps borking a little, so that's to be dealt with.
    #TO DO:
      #unbork math
      #loop everything
      #let user pick "master task" for fpl and shit

  #1.0 7/23/19: added primal test to prevent breaking. 

  
#libraries - this should be all that you need. Hopefully it is?
  if (!require("SpadeR")) {
    install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
  if (!require("gdata")) {
    install.packages("gdata", dependencies = TRUE); library(gdata)}
  if (!require("openxlsx")) {
    install.packages("openxlsx", dependencies = TRUE); library(openxlsx)}
  
#local functions
  #I thought this was going to be used more so I function'd it. Now it's used once but i can't bring myself to remove it. 
  #The name makes me laugh and that's all that matters.
  dropthebass <- function(item){as.double(levels(drop.levels(item)))}
  #i'm not convinced this is even useful. Stupi listing.
  superFUNlist <- function(item){unlist(as.double(length(item)))[1]}
  
#bUsInEsS tImE  
  setwd(choose.dir())
  #reading in the AOF - the max row/col are to future proof, to the degree i'm able.
  input.file <- read.xlsx(xlsxFile = file.choose(), sheet = "Capture hist", cols = c(2:100), rows = c(seq(4,10000)))
  read.in <- input.file[,grepl('^[0-9]*$',input.file[1,])]
  num.tasks <- as.double(ncol(read.in))

  #needed to be a data structure. I chose DF because it's easier to math with.
  output <- data.frame(data = NA, nrow = num.tasks, cols = 8)
  colnames(output) <- c("Profiles", "Nest", "Nest UCI")
  
#doing math - calculating the relevant abundance data
  for(i in 1:num.tasks){
    #I added a test here for if doubletons write nothing so that it doesn't break when we run primals (assuming proper sampling)
    #primals now have sum(i) as values, because maths.
    if(length(read.in[read.in[,i]>1,i])==0) {
      output[i,] <- rbind(as.vector(c(sum(read.in[,i]), sum(read.in[,i]), sum(read.in[,i]))))
    } else {
      input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
      output[i,] <- rbind(as.vector(c(dropthebass(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
    }
  }
  
  #I know there's a more elegant way to do this but i don't really care right now. to be fixed?
  output$'% found' <- rep(NA, num.tasks)
  output$'% expected' <- rep(NA, num.tasks)
  output$'# expected' <- rep(NA, num.tasks)
  output$matches <- rep(NA, num.tasks)
  output$'in spec' <- rep(NA, num.tasks)

  #doing our kind of maths.
  output$'% found' <- output$Profiles/output$`Nest UCI`
  match.grid <- read.in[read.in[,1]!=0,]
  
  for(i in 2:num.tasks){
    ##expected maths
    output$'% expected'[i] <- output$`% found`[1]*output$`% found`[i]
    output$'# expected'[i] <- output$`% expected`[i]*output$`Nest UCI`[i]
    #match math?
    output$matches[i] <- superFUNlist(match.grid[match.grid[,i]!=0,i])
    #LOGIC TESTING
    output$'in spec'[i] <- output$matches[i] > output$`# expected`[i]
  }
  
#Printing to file
  #standard DT file naming convention
  write.csv(output, file = paste("iChao estimates for", num.tasks, "tasks", as.character((Sys.Date())), version, ".csv", sep = " "))
  #I'm just so clever
  print("iChao estimates output in chosen directory")
}
