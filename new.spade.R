# install.packages("SpadeR")
# library(SpadeR)
# install.packages("gdata")
# library(gdata)

#i'm pretty sure the library section below is smart enough? hopefully.
if (!require("SpadeR")) {
  install.packages("SpadeR", dependencies = TRUE); library(SpadeR)}
if (!require("gdata")) {
  install.packages("gdata", dependencies = TRUE); library(gdata)}

#i thought this was going to be used more so I function'd it. Now it's used once but i can't bring myself to remove it.
dropthebass <- function(item){as.double(levels(drop.levels(item)))}


read.in <- read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE)

#read.in <- read.csv(file.choose(), header = FALSE,  encoding = "UTF-8", stringsAsFactors = FALSE)
num.tasks <- ncol(read.in)
original.file <- read.in
as.integer(read.in[1,1])
read.in[1,1] <- 0
# 
# read.in[,1] <- as.double(read.in[,1])
# 
# class(read.in[,1])
# table(read.in[,2])
#
##maybe add in samples?
#
for(i in 1:num.tasks){print(class(read.in[,i])); print(i)}
class(read.in)

output <- matrix(data = NA, nrow = num.tasks, ncol = 3)
# read.in[,31] <- read.in[,1]
# 
# input <- ChaoSpecies(read.in[,31], "abundance", conf = 0.95)
read.in[,1] <- read.in[,2]

for(i in 1:num.tasks){
  # if(class(read.in[,i])=="character"){
  #   read.in[,i] <- as.double(read.in[,i])
  # }
  input <- ChaoSpecies(as.double(read.in[,i]), "abundance", conf = 0.95)
  output[i,] <- rbind(as.vector(c(dropthebass(input$Basic_data_information[2,2]),input$Species_table[5,1], input$Species_table[5,4])))
  print(i)
}
colnames(output) <- c("Profiles", "Nest", "Nest UCI")
write.csv(output, file = paste("iChao estimates for ", num.tasks, " tasks ", as.character((Sys.Date())), ".csv", sep = ""))
getwd()
