standard_mutate <- 
  function(x){
    round((x-mean(x, na.rm = T))/sd(x, na.rm = T),2)
  }