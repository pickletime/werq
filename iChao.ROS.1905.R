####################################################
# Functions for grinds iChao1 population estimates #
# and expected matching between two grinds batches #
####################################################

library(SpadeR)

# Read in Genecap style data in csv format
setwd(choose.dir())
gcdata <- read.csv(file.choose(), header = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)




# Separate tasks into vectors and run SpadeR ChaoSpecies function
task_1 <- ChaoSpecies(gcdata[,1], "abundance", conf = 0.95)
task_2 <- ChaoSpecies(gcdata[,2], "abundance", conf = 0.95)
task_3 <- ChaoSpecies(gcdata[,3], "abundance", conf = 0.95)
task_4 <- ChaoSpecies(gcdata[,4], "abundance", conf = 0.95)

# Function to isolate relevant iChao1 results from SpadeR output
iChao1 <- function(ChaoPop){
  # Profiles found
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  ProfilesObserved <- as.numeric.factor(ChaoPop$Basic_data_information$Value[2])
  
  # Population estimate and CI intervals
  iChao1Estimate <- ChaoPop$Species_table[5,1]
  iChao1Est_CILower <- ChaoPop$Species_table[5,3]
  iChao1Est_CIUpper <- ChaoPop$Species_table[5,4]
  
  # Return table of desired results
  return(cbind(ProfilesObserved, iChao1Estimate, iChao1Est_CILower, iChao1Est_CIUpper))
}

# Run each task
iChao1(task_1)
iChao1(task_2)
iChao1(task_3)
iChao1(task_4)

# Function to calculate expected matching between two tasks
iChao1_Expected <- function(ChaoPop_A, ChaoPop_B){
  # Table of population size estimates
  results_table <- rbind(iChao1(ChaoPop_A), iChao1(ChaoPop_B))
  rownames(results_table) <- c(deparse(substitute(ChaoPop_A)), deparse(substitute(ChaoPop_B)))
  
  # Calculate proportion of estimates found
  PropUpperFound <- c(results_table[1,1]/results_table[1,4], results_table[2,1]/results_table[2,4])
  
  # Calculateexpected matching proportion and count
  ExpectedPropUpperMatching <- c(NA, prod(PropUpperFound))
  ExpectedNumberUpperMatching <- c(NA, results_table[2,4]*ExpectedPropUpperMatching[2])
  
  # Return table of expectations
  return(cbind(results_table, PropUpperFound, ExpectedPropUpperMatching, ExpectedNumberUpperMatching))
}

##I added this section because I want it to spit out results. I'll try to add a true/false test into this also.
write.csv(rbind(iChao1_Expected(task_1, task_2),iChao1_Expected(task_3, task_4)), file = paste("iChao estimates for ", as.character((Sys.Date())), ".csv", sep = ""))



