gen_A = function(N=100
                 , g_fun = function(N){graph.full(N)}
                 ) {
  
  require(igraph)
  
  
  ### GENERATE TIE NETWORK
  g = g_fun(N)
  A_unweighted = get.adjacency(g, sparse=F)
  
  
  ### GENERATE STOCHASTIC/ADJACENCY MATRIX
  self_weight = runif(N, 0, 1)
  A = lapply(1:N, function(i){
    if(sum(A_unweighted[i,])>0) {
       return(A_unweighted[i,]*((1-self_weight[i])/sum(A_unweighted[i,])))
    } else {
      return(A_unweighted[i,])
    }
  }) %>% 
    do.call(rbind, .)
  
  diag(A) = self_weight
  
  # a final normalization for rows with only diag entries
  # should not be necessary, this model isn't intended for such sparse A-matrices
  A = A %>%
    apply(., 1, function(A_i){
      A_i / sum(A_i) # normalize rows with only diag entries
    }) %>% 
    t # not sure why apply is transposing
  
  A
}

