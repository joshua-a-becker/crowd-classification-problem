require(igraph)

myd = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
                       , stringsAsFactors=F)


myd$distance_remaining_1 = (myd$mean.neighbor.time1 - myd$response_1)
myd$alpha_1 = (myd$response_2 - myd$response_1) / myd$distance_remaining_1
alphaset = subset(myd, !is.na(alpha_1) & is.finite(alpha_1))$alpha_1
alphaset = alphaset[alphaset>0 & alphaset<1]

rm(myd)



new_population = function(N=100, 
                          cor=0.25, 
                          skew=0.0, 
                          t.quantile=0.5, 
                          rev.choice=F, 
                          c.quantile=0.25, 
                          shape="symmetrical",
                          g=NULL) {

  pop=list()
  
  if(is.null(g)) {
    pop$g = graph.full(N)
    pop$g$cent=0
  } else { pop$g = g}
  
  pop$belief = rlnorm(N, 6.2, 0.6)
  
  if(shape == "symmetrical") {
    pop$belief = rnorm(N, 0, 1)
  }
  
  pop$c.quantile = c.quantile
  pop$t.quantile = t.quantile
  pop$shape = shape
  pop$rev.choice = rev.choice
  
  pop$mu=mean(pop$belief)
  pop$med=mean(pop$belief)
  
  pop$true.value = sort(pop$belief, decreasing=F)[floor(N*t.quantile)]
  
  pop$choice.threshold = sort(pop$belief, decreasing=F)[floor(N*c.quantile)]
  
  pop$choice = (pop$belief<pop$choice.threshold)*1
  pop$truth = (pop$true.value<pop$choice.threshold)*1
  
  if(rev.choice) {
    pop$choice = (pop$belief>pop$choice.threshold)*1
    pop$truth = (pop$true.value>pop$choice.threshold)*1
  }
  
  
  
  pop$err = abs(pop$belief - pop$true.value)
  
  pop$conf = get.confidence(pop$err, cor)
  pop$threshold = get.threshold(pop$conf)
  
  pop$N=N
  
  pop
}



get.confidence = function(err, r=0.25) {
  n=length(err)
  x1 = rnorm(n)
  x2 = err
  
  y1 = scale(x2) * r  +  scale(residuals(lm(x1~x2))) * sqrt(1-r*r)
  
  q=sort(sample(alphaset, n, replace=T))
  conf=q[order(order(as.numeric(y1)))]

  (1-conf)/2
}

get.threshold = function(conf) {

  pct = (0.5 - conf) / ( 1 - conf )
  1-pct
}


demo = function() {
  require(ggplot2)
  pop = new_population(N=10000, cor=0.4) 
  

  ggplot(data.frame(b=pop$choice, c=pop$conf, e = pop$err, t=pop$thresh), 
         aes(x=round(e/10), y=t)) +
    stat_summary(fun.y="mean", geom="point")
}




updated_belief = function(population) {
  
  ### CYCLE THROUGH ALL THE AGENTS
  ### FIND OUT HOW MANY OF THEIR NEIGHBORS DISAGREE
  ### UPDATE IF THIS PROPORTION IS GREATER THAN THRESHOLD
  new_belief = unlist(lapply(1:population$N, FUN=function(i) {
    
    ### GET AGENT'S BELIEF & THRESHOLD
    agent.belief = population$choice[i]
    agent.threshold = population$threshold[i]
    
    ### GET NEIGHBORS BELIEFS
    neighbors = population$choice[
      as.numeric(V(population$g)[nei(i)])
      ]
    
    ### GET PROPORTION DISAGREED
    disagree = mean(neighbors!=agent.belief)
    
    # cat("Agent: ",i,"\n")
    # cat("Belief: ",agent.belief,"\n")
    # cat("Threshold: ", agent.threshold,"\n")
    # cat("Soc info: ", disagree,"\n")
    
    ### IF PROPORTION DISAGREE > THRESHOLD, SWAP BELIEFS
    if(disagree>agent.threshold) {
      agent.belief = 1 - agent.belief
    }
    
    # cat("New belief: ",agent.belief,"\n")
    # print("----")
    
    return(agent.belief)
  }
  ))
  
  new_belief
}


outcome_record = function(pop, round=-1, trial=-1) {
  data.frame(  N=pop$N
             , c.quantile=pop$c.quantile
             , t.quantile = pop$t.quantile
             , shape = pop$shape
             , choice.threshold = pop$choice.threshold
             , true.value = pop$true.value
             , truth = pop$truth
             , network=pop$g$name
             , centralization=pop$g$cent
             , rev.choice = pop$rev.choice
             , round=round
             , trial=trial
             , prob.correct=mean(pop$choice == pop$truth)
             , stringsAsFactors=F
             , cor.thresh = cor(pop$err, pop$threshold))
}
