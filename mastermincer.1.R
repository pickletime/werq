mastermincer.1 <- function(){
  version <- "V 1.0"
  #1.0: 2019-08-15
  #this is ugly as all hell. Eventually a bunch of this will be functioned, 
  #I just want something that works first, then I can go back and fix/streamline.
  
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
  #i'm not convinced this is even useful. Stupid listing.
  superFUNlist <- function(item){unlist(as.double(length(item)))[1]}

#bUsInEsS tImE  
  setwd(choose.dir())
  input.file <- read.xlsx(xlsxFile = file.choose(), sheet = "Capture hist", cols = c(2:100), rows = c(seq(4,10000)))
  read.in <- input.file[,grepl('^[0-9]*$',input.file[1,])]
  num.tasks <- as.double(ncol(read.in))

#needed to be a data structure. I chose DF because it's easier to math with.
  output <- data.frame(data = NA, nrow = num.tasks, cols = 8)
  
  
  
#I'm apologizing in advance. What follows is the ugliest/clunkiest thing to have ever graced this brown and dying planet.
  if(num.tasks == 3){
  #only for 3 task jobs, so it'll work for primals
    for(i in 1:num.tasks){
      #I added a test here for if doubletons write nothing so that it doesn't break when we run primals (assuming proper sampling)
      #primals now have sum(i) as values, because maths.
      if(length(read.in[read.in[,i]>1,i])==0 || i==3) {
        output[i,] <- rbind(as.vector(c(sum(read.in[,i]), sum(read.in[,i]), sum(read.in[,i]))))
      } else {
        input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
        output[i,] <- rbind(as.vector(c(dropthebass(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
      }
    }
    colnames(output) <- c("Profiles", "Nest", "Nest UCI")
    #I know there's a more elegant way to do this but i don't really care right now. to be fixed?
    output$'% found' <- rep(NA, num.tasks)
    output$'% expected' <- rep(NA, num.tasks)
    output$'# expected' <- rep(NA, num.tasks)
    output$matches <- rep(NA, num.tasks)
    output$'In spec?' <- rep(NA, num.tasks)
    #trying this as a loop - we'll see
    output$'% found' <- output$Profiles/output$`Nest UCI`
    match.grid <- read.in[read.in[,1]!=0,]
    
    for(i in 2:num.tasks){
      ##expected maths
      output$'% expected'[i] <- output$`% found`[1]*output$`% found`[i]
      output$'# expected'[i] <- output$`% expected`[i]*output$`Nest UCI`[i]
      #match math?
      output$matches[i] <- superFUNlist(match.grid[match.grid[,i]!=0,i])
      #LOGIC TESTING
      output$'In spec?'[i] <- output$matches[i] > output$`# expected`[i]
    } #running the grid
  } else if(num.tasks == 4){
  #this is for fpl. There's definitely a more elegant way to do this, but idc.
    second.order <- c(3,4,2,1)
    for(i in 1:num.tasks){
      #I added a test here for if doubletons write nothing so that it doesn't break when we do pop estimates. SUPER unlikely.
      if(length(read.in[read.in[,i]>1,i])==0) {
        output[i,] <- rbind(as.vector(c(sum(read.in[,i]), sum(read.in[,i]), sum(read.in[,i]))))
      } else {
        input <- ChaoSpecies(read.in[,i], "abundance", conf = 0.95)
        output[i,] <- rbind(as.vector(c(dropthebass(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
      }
    } #running grid
    colnames(output) <- c("Profiles", "Nest", "Nest UCI")
    #I know there's a more elegant way to do this but i don't really care right now. to be fixed?
    output$'% found' <- rep(NA, num.tasks)
    output$'% expected' <- rep(NA, num.tasks)
    output$'# expected' <- rep(NA, num.tasks)
    output$matches <- rep(NA, num.tasks)
    output$'In spec?' <- rep(NA, num.tasks)
    #trying this as a loop - we'll see
    output$'% found' <- output$Profiles/output$`Nest UCI`
    output.2 <- output[c(3,4,1,2),]
    match.grid <- read.in[read.in[,1]!=0,]
    match.grid.2 <- read.in[read.in[,3]!=0,]
    match.grid.2[,c(3,4,1,2)] <- match.grid.2
    
    
    for(i in 2:num.tasks){
      ##this is for matching to the first task
      ##expected maths
      output$'% expected'[i] <- output$`% found`[1]*output$`% found`[i]
      output$'# expected'[i] <- output$`% expected`[i]*output$`Nest UCI`[i]
      #match math?
      output$matches[i] <- superFUNlist(match.grid[match.grid[,i]!=0,i])
      #LOGIC TESTING
      output$'In spec?'[i] <- output$matches[i] > output$`# expected`[i]
    } #grid 1
  
    
    for(i in 2:num.tasks){
      ##this is for matching to the second task
      ##expected maths
      output.2$'% expected'[i] <- output.2$`% found`[1]*output.2$`% found`[i]
      output.2$'# expected'[i] <- output.2$`% expected`[i]*output.2$`Nest UCI`[i]
      #match math?
      output.2$matches[i] <- superFUNlist(match.grid.2[match.grid.2[,i]!=0,i])
      #LOGIC TESTING
      output.2$'In spec?'[i] <- output.2$matches[i] > output.2$`# expected`[i]
    } #grid 2
    output <- rbind(output, output.2)
  } else if(num.tasks !=3 && num.tasks !=4){
  #this is only if it won't go through either of the others. Hopefully this futureproofs?
    print("this function can only handle 3 or 4 task mince jobs - if this doesn't apply try elsewhere kthx")
    exit()
    } #instaquit
#Printing to file
  #standard DT file naming convention
  write.csv(output, file = paste("iChao estimates for", num.tasks, "tasks", as.character((Sys.Date())), version, ".csv", sep = " "))
  #I'm just so clever
  print("iChao estimates output in chosen directory")
}
