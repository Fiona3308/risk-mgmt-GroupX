# Draft for corrbmsampset

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