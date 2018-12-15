
#Inputs: 
  # n = n steps of size n*dt year, we set n=1 here for this function
  # dt = returnPeriod/252 (eg,1/252,5/252)
#Returns: k sample paths for a pair of brownian motions, N steps of size DT years

library(MASS)

corrbmsampset <- function(dt,k,mu_gbm,rho){
  sigma <- list()
  bm <- list()
  ntrails <- length(rho)
  # bm_1 <- matrix(NA,nrow=k,ncol=length(rho)*2)
  # bm_2 <- matrix(NA,nrow=k,ncol=length(rho)*2)
  for(i in 1:ntrails){
    # sigma[[i]] <- matrix(c(1,rho[i],rho[i],1),2,2)
    # bm_1[,i] <- mvrnorm(n=k,mu_gbm[i,],Sigma = sigma[[i]])[,1]
    # bm_2[,i] <- mvrnorm(n=k,mu_gbm[i,],Sigma = sigma[[i]])[,2]
    # bm[[i]] <- sqrt(dt)*mvrnorm(n=k,mu_gbm[i,],Sigma = sigma[[i]])
    
    bm[[i]] <- sqrt(dt)*mvrnorm(n=k,mu_gbm[i,],Sigma = matrix(c(1,rho[i],rho[i],1),2,2))
  }
  
  # w_1 <- matrix(NA,nrow=k,ncol = length(bm))
  # w_2 <- matrix(NA,nrow=k,ncol = length(bm))
  # for (j in 1:length(rho)){
  #   w_1[,i] <- bm[[i]][,1]
  #   w_2[,i] <- bm[[i]][,2]
  # }
  return(bm)
}

ll <- corrbmsampset(1/252,1000,mu,rho)

i<-2
bm[[1]] <- sqrt(1/252)*mvrnorm(n=10000,mu[i,],Sigma = matrix(c(1,rho[i],rho[i],1),2,2))
