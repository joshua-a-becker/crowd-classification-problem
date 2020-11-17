################################################################################

# This is the script that contains the functions to run the simulations.

################################################################################

###############
# Preparation #
###############

# Loading dependencies
require(igraph)

########################
# Simulation functions #
########################

# Functions are based on the empirical data, smoothed to produced a monotonic 
# function with no 0 or 1 probabilities (to avoid determinate behavior).

fn_prob_change = function(p
                          , initially_correct=1  # setting defaults
                          , overfit=F            # as a cheap hack
                          ) {
  
  soc_info = round(p,1)
  if(overfit) {
    
    function_map = data.frame(
      agree=c(  0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1
              , 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
      , prob =c(  0.7, 0.7, 0.3, 0.3, 0.2, 0.1,  0.05, 0.05, 0.05, 0.05, 0.05
                , 0.4, 0.4, 0.2, 0.2, 0.2, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)
      , initially_correct=c(rep(F,11),rep(T,11))
    )
  } else {
    initially_correct=1
    function_map = data.frame(
      agree=c(0,  0.1,  0.2,  0.3,  0.4,  0.50, 0.6,  0.7,  0.8,  0.9,  1   )
      , prob =c(0.7,0.65, 0.25, 0.20, 0.15, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05)
      , initially_correct=T
    )
  }
  
  function_map$prob[function_map$agree==soc_info & 
                      function_map$initially_correct==(initially_correct==1)]
}


CalculateNewBeliefs = function(  pop
                               , prob_change=fn_prob_change
                               , overfit=F
                               ) {
  sapply(V(pop), function(node){

    prop_agree = mean( V(pop)[nei(node)]$belief==V(pop)$belief[node] )
    prob_of_changing = prob_change(prop_agree
                                   ,initially_correct=V(pop)$belief[node] # 1=correct
                                   ,overfit=overfit
                                   )
    ifelse(runif(1,0,1)<prob_of_changing,
           1-V(pop)$belief[node],
           V(pop)$belief[node]
    )
  })
}

RunWoCGame = function(  pop
                      , rounds
                      , prob_change=fn_prob_change
                      , overfit=F
                      ) {
  initial_accuracy = mean(V(pop)$belief)
  
  for(i in 1:(rounds-1)) {
    V(pop)$belief = CalculateNewBeliefs(pop, prob_change, overfit)
  }
  
  final_accuracy = mean(V(pop)$belief)
  return(data.frame(
    initial_accuracy = initial_accuracy,
    final_accuracy = final_accuracy,
    overfit=overfit
  ))
}

