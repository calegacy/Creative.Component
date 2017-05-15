# Teaching functions

#Function to make a probability distribution for proportions

binomProb = function(n, p){
  out = rbinom(n,1,p)
  propSuccess = round(length(which(out == 1))/n, 4)
  return(propSuccess)
}


binfun = function(data,n){
  bin = (3.5*sd(data) )/(n^(1/3))
  return(bin)
}
