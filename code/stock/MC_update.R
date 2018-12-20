#-- Assuming underlying stock follows GBM --#
# Here only did one time step in monte carlo simulation of GBM

#Input:
  # s0: Initial portfolio value
  # mu: Drift
  # sigma: Volatility
  # p: probability of VaR/ES
  # dRtn: 1/5/10 days of return
  # dt: Default 5/252
  # npaths: number of paths to generate bm

#Output:
  # Monte Carlo VaR/ES for one stock

MCVaR <- function(s0, mu, sigma, p, npaths,years,dRtn){
  
  # dRtn <- 5
  # s0 <- 10000
  # p <- 0.99
  # years <- 5  
  # npaths <- 10000
  
  horizon <- dRtn/252
  npts <- 252*years
  ntrials <- length(mu) 
  
  MCVaR <- NA
  st_bm <- matrix(0,nrow=ntrials,ncol=npaths)
  for (i in 1:ntrials){
    n <- 1
    dt <- dRtn/252
    w <- sqrt(dt)*rnorm(npaths,0,1)

    st_bm[i,] <- c(s0*exp(sigma[i]*w+(mu[i]-sigma[i]^2/2)*dt))
    MCVaR[i] <- s0-quantile(st_bm[i,],1-p)
  }
  return(MCVaR)
}


MCES <- function(s0, mu, sigma, p,npaths,years,dRtn){
  
  # dRtn <- 5
  # s0 <- 10000
  # p <- 0.99
  # years <- 5   
  # npaths <- 10000 
  
  horizon <- dRtn/252
  npts <- 252*years
  ntrials <- length(mu) 
  
  MCES <- NA
  loss_point <- NA
  st_bm <- matrix(0,nrow=ntrials,ncol=npaths)
  for (i in 1:ntrials){
    n <- 1
    dt <- dRtn/252
    w <- sqrt(dt)*rnorm(npaths,0,1)

    st_bm[i,] <- c(s0*exp(sigma[i]*w+(mu[i]-sigma[i]^2/2)*dt))
    loss_point[i]<-quantile(st_bm[i,],1-p)
    
    MCES[i] <- s0-mean(st_bm[i,][which(st_bm[i,] < loss_point[i])])
  }
  return(MCES)
}

