#### INITIALIZE THE MODEL ####
new_population = function(N=50, var.term=10, prob.correct=0.6, g=NULL) {
  ### THE DETAILS OF THIS FUNCTION ARE ANNOTATED IN 
  ### 00_CI-Discrete-Simulation-Definition.R
  
  require(igraph)
  if(is.null(g)) {
    # just a default
    g = degree.sequence.game(rep(4,N), method="vl")
  }
  
  signal = rbinom(N, var.term, prob.correct)/var.term
  
  confidence = abs(signal-0.5)*2
  confidence =  round(confidence, 1)

  threshold = (confidence / 2) + 0.5

  belief = unlist(lapply(signal, FUN=function(x){runif(1,0,1)<x}))*1
  
  return(list(
    N=N,
    var.term=var.term,
    prob.correct=prob.correct,
    signal = signal,
    confidence = confidence,
    threshold = threshold, 
    belief = belief,
    g=g
  ))
}


updated_belief = function(population) {
  
  ### CYCLE THROUGH ALL THE AGENTS
  ### FIND OUT HOW MANY OF THEIR NEIGHBORS DISAGREE
  ### UPDATE IF THIS PROPORTION IS GREATER THAN THRESHOLD
  new_belief = unlist(lapply(1:population$N, FUN=function(i) {
      
      ### GET AGENT'S BELIEF & THRESHOLD
      agent.belief = population$belief[i]
      agent.threshold = population$threshold[i]
      
      ### GET NEIGHBORS BELIEFS
      neighbors = population$belief[
        as.numeric(V(population$g)[nei(i)])
      ]
      
      ### GET PROPORTION DISAGREED
      disagree = mean(neighbors!=agent.belief)
      
      #cat("Agent: ",i,"\n")
      #cat("Belief: ",agent.belief,"\n")
      #cat("Threshold: ", agent.threshold,"\n")
      #cat("Soc info: ", disagree,"\n")
      
      ### IF PROPORTION DISAGREE > THRESHOLD, SWAP BELIEFS
      if(disagree>agent.threshold) {
        agent.belief = 1 - agent.belief
      }
      
      #cat("New belief: ",agent.belief,"\n")
      #print("----")
      
      return(agent.belief)
    }
  ))
}


outcome_record = function(pop, round) {
  data.frame(N=pop$N,
             var.term=pop$var.term,
             baseline.prob.correct=pop$prob.correct, 
             network=pop$g$name,
             centralization=pop$g$cent,
             round=round, 
             prob.correct=mean(pop$belief),
             stringsAsFactors=F)
}

irwin.hall = function(size=1, N=1, lower=0, upper=1) {
  replicate(size, sum(runif(N,lower,upper)))
}
