
#Inputs: 
  # n = n steps of size n*dt year, we set n=1 here for this function
  # dt = returnPeriod/252 (eg,1/252,5/252)
#Returns: k sample paths for a pair of brownian motions in i trails(days), N steps of size DT years
  # bm = array(k(row) by 2(col) by i(days))

corrbmsampset <- function(dt,k,mu_gbm,rho){
  sigma <- list()
  bm <- list()
  
  if(class(rho)=="numeric"){
    ntrails <- length(rho)
  }else{
    ntrails <- nrow(rho)
  }
 
  for(i in 1:ntrails){
    sigma[[i]] <- matrix(c(1,rho[i],rho[i],1),2,2)
    bm[[i]] <- sqrt(dt)*mvrnorm(n=k,mu_gbm[i,],Sigma = sigma[[i]])
  }
  return(bm)
}

# ll <- corrbmsampset(1/252,1000,mm$mu_gbm,mm$rho)

