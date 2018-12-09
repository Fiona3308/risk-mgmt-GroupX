# assuming p follows GBM

# s0: Initial portfolio value
# mu: Drift
# sigma: Volatility
# p: probability of VaR
# t: Default 5/252
# npaths: number of paths to generate bm

MCVaR <- function(s0, mu, sigma, p, dt, npaths,years,dRtn){
  
  dRtn <- 5
  s0 <- 10000
  horizon <- dRtn/252
  p <- 0.99
  years <- 5 # years
  npts <- 252*years
  npaths <- 10000
  ntrials <- length(mu) 
  
  # mu <- par1$mu_gbm
  # sigma <- par1$sigma_gbm
  
  MCVaR <- NA
  st_bm <- matrix(0,nrow=ntrials,ncol=npaths)
  for (i in 1:ntrials){
    n <- 1
    dt <- dRtn/252
    # tv <- sample(0,n*dt,dt)
    w <- sqrt(dt)*rnorm(npaths,0,1)
    # w <- cbind(0,w)
    st_bm[i,] <- c(s0*exp(sigma[i]*w+(mu[i]-sigma[i]^2/2)*dt))
    MCVaR[i] <- s0-quantile(st_bm[i,],1-p)
  }
  return(MCVaR)
}


MCES <- function(s0, mu, sigma, p, dt, npaths,years,dRtn){
  
  dRtn <- 5
  s0 <- 10000
  horizon <- dRtn/252
  p <- 0.99
  years <- 5 # years
  npts <- 252*years
  npaths <- 10000
  ntrials <- length(mu) 
  
  # mu <- par1$mu_gbm
  # sigma <- par1$sigma_gbm
  
  MCES <- NA
  loss_point <- NA
  st_bm <- matrix(0,nrow=ntrials,ncol=npaths)
  for (i in 1:ntrials){
    n <- 1
    dt <- dRtn/252
    # tv <- sample(0,n*dt,dt)
    w <- sqrt(dt)*rnorm(npaths,0,1)
    # w <- cbind(0,w)
    st_bm[i,] <- c(s0*exp(sigma[i]*w+(mu[i]-sigma[i]^2/2)*dt))
    loss_point[i]<-quantile(st_bm[i,],1-p)
    
    MCES[i] <- s0-mean(st_bm[i,][which(st_bm[i,] < loss_point[i])])
  }
  return(MCES)
}

# plot(MCES,type = "l")
# plot(MCVaR,type = "l")
