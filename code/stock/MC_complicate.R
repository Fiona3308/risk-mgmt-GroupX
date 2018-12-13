#Assume Portfolio follows GBM

# Since we know the distribution of GBM at the terminal time, 
# We don't need intermediate values of the portfolio, 
# we can do the stock simulatoin with just 1 time step. 
# So, here n=1

MCVaR <- function(s0, mu, sigma, p, dt, npaths,years,dRtn){
  
  # dRtn <- 5
  # s0 <- 10000
  # p <- 0.99
  # years <- 5 
  # npaths <- 10000  
  
  horizon <- dRtn/252
  npts <- 252*years
  k <- npaths
  ntrials <- length(mu) 
  
  # mu <- par1$mu_gbm
  # sigma <- par1$sigma_gbm
  
  MCVaR <- NA
  vt <- matrix(0,nrow=npaths,ncol=ntrials)

  for (i in 1:ntrials){
    n <- 1
    dt <- dRtn/252
    tv <- seq(from=0,to=n*dt,by=dt)
    w <- bmsampset(n,dt,k)
    vt[,i] <- c((s0*exp(sigma[i]*w+(mu[i]-sigma[i]^2/2)*tv))[,-1])
    MCVaR[i] <- s0-quantile(vt[,i],1-p)
  }
  return(MCVaR)
}

MCES <- function(s0, mu, sigma, p, dt, npaths,years,dRtn){
  
  # dRtn <- 5
  # s0 <- 10000
  # p <- 0.975
  # years <- 5 
  # npaths <- 10000  
  
  horizon <- dRtn/252
  npts <- 252*years
  k <- npaths
  ntrials <- length(mu) 
  
  # mu <- par1$mu_gbm
  # sigma <- par1$sigma_gbm
  
  MCES <- NA
  loss_point <- NA
  vt <- matrix(0,nrow=npaths,ncol=ntrials)
  
  for (i in 1:ntrials){
    n <- 1
    dt <- dRtn/252
    tv <- seq(from=0,to=n*dt,by=dt)
    w <- bmsampset(n,dt,k)
    vt[,i] <- (s0*exp(sigma[i]*w+(mu[i]-sigma[i]^2/2)*tv))[,-1]
    loss_point[i] <- quantile(vt[,i],1-p)
    
    MCES[i] <- s0-mean(vt[,i][which(vt[,i] < loss_point[i])])
  }
  return(MCES)
}


