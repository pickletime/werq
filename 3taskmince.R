3taskmince <- function(read.in, num.tasks,output){
  version <- "V 1.0"
  #1.0 2019-08-15:  
  
  #doing math - calculating the relevant abundance data
  for(i in 1:num.tasks){
    #I added a test here for if doubletons write nothing so that it doesn't break when we run primals (assuming proper sampling)
    #primals now have sum(i) as values, because maths.
    if(i==3) {
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
    output$'in spec'[i] <- output$matches[i] > output$`# expected`[i]
  }
}
