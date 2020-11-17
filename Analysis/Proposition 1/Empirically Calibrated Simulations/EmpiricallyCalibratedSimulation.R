################################################################################

# This is the script to create the simulations for proposition 1.
# THIS SHOULD BE RUN BEFORE ANYTHING ELSE

# This script models the wisdom of crowds with discrete choice.
# We assume one parameter of individual behavior: P(CHANGE | SOCIAL INFO)

################################################################################

###############
# Preparation #
###############

# Loading dependencies
library(igraph)
source("dependencies.R")

# Loading the simulation functions
source('Analysis/Proposition 1/Empirically Calibrated Simulations/SimulationFunctions.R', echo=TRUE)

############################
# Creating the simulations #
############################

outcomes = data.frame(  initial_accuracy=numeric()
                      , final_accuracy=numeric()
                      , expected_accuracy=numeric()
                      , k=numeric()
                      , N=numeric()
                      )

reps=1000
N=20
k=19
probs=seq(0,1,by=0.05)
total = reps*length(probs)
pb=txtProgressBar(0,total,style=3)

filename = paste0("Analysis/Proposition 1/Empirically Calibrated Simulations/empirical_sim_",sample(10000000,1),".csv")


for(i in 1:reps) {
  pop = degree.sequence.game(rep(k,N))
  for(overfit in c(T,F)){
    for(prob_correct in probs){
      V(pop)$belief = (runif(N,0,1)<prob_correct)*1 
      new_row = RunWoCGame(pop, 3, overfit=overfit)
      new_row$expected_accuracy = prob_correct
      new_row$k=k
      new_row$N=N
      outcomes[nrow(outcomes)+1, ] = new_row
      setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
      
      write.table(new_row, filename, sep = ","
                  , col.names = !file.exists(filename)
                  , append = T
                  , row.names=F)
      
    } 
  }
}



