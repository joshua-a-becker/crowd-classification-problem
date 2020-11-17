################################################################################

# This is the script that contains the functions to run the simulations.
# (NOTE: THIS IS THE PREREGISTERED VERSION WITH AN ERROR)

################################################################################

###############
# Preparation #
###############

# Loading dependencies
require(igraph)

########################
# Simulation functions #
########################

fn_prob_change = function(p) {
  soc_info = round(p)
  function_map = data.frame(
      agree=c(0,  0.1,  0.2,  0.3,  0.4,  0.50, 0.6,  0.7,  0.8,  0.9,  1   )
    , prob =c(0.7,0.65, 0.25, 0.20, 0.15, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05)
  )
  function_map$prob[function_map$agree==soc_info]
}


CalculateNewBeliefs = function(pop, prob_change=fn_prob_change) {
  sapply(V(pop), function(node){
    prop_agree = mean( V(pop)[nei(node)]$belief==V(pop)$belief[node] )
    prob_of_changing = prob_change(prop_agree)
    ifelse(runif(1,0,1)<prob_of_changing,
           1-V(pop)$belief[node],
           V(pop)$belief[node]
    )
  })
}

RunWoCGame = function(pop, rounds, prob_change=fn_prob_change) {
  initial_accuracy = mean(V(pop)$belief)
  
  for(i in 1:(rounds-1)) {
    V(pop)$belief = CalculateNewBeliefs(pop, prob_change)
  }
  
  final_accuracy = mean(V(pop)$belief)
  return(data.frame(
    initial_accuracy = initial_accuracy,
    final_accuracy = final_accuracy
  ))
}
