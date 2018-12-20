# mu: matrix
# sigma: matrix
# s0: matrix


# stock1Shares <- 156
# stock2Shares <- 200
library(MASS)

stock1 <- XOM$PX_LAST
stock2 <- INTC$PX_LAST

source("../code/parameter/winEstGBM2.R")
source("../code/corrbmsampset.R") #generate random numbers of pair brownian motion

# inputs
prices <- comb.col(stock1,stock2)
weight <- c(156/(156+200),200/(156+200))
# shares <- weight*v0/prices[1,]
shares <- c(156,200)

dRtn <- 5
years <- 5
v0 <- 10000
npaths <- 100
p <- 0.99

# parameters
est <- winEstGBM2(prices,dRtn,year)


MCVaR <- function(v0, mu, sigma,rho, shares,p, npaths,years,dRtn){
  # v0 <- 10000
  # p <- 0.99
  # years <- 5 
  # npaths <- 10000
  
  dt <- dRtn/252
  k <- npaths
  npts <- 252*years
  
  if(class(rho)=="numeric"){
    ntrails <- length(rho)
  }else{
    ntrails <- nrow(rho)
  }

  MCVaR <- NA
  st1_bm <- matrix(0,nrow=k,ncol=ntrails)
  st2_bm <- matrix(0,nrow=k,ncol=ntrails)
  startprice <- NA
  port_px <- matrix(0,nrow=k,ncol=ntrails)
  portPos <- NA
  
  for (i in 1:ntrails){
    n <- 1
    dt <- dRtn/252
    w <- corrbmsampset(dt,k,mu,rho)
    s0 <- prices
    
    st1_bm[,i] <- c(s0[i,1]*exp(sigma[i,1]*w[[i]][,1]+(mu[i,1]-sigma[i,1]^2/2)*dt))
    st2_bm[,i] <- c(s0[i,2]*exp(sigma[i,2]*w[[i]][,2]+(mu[i,2]-sigma[i,2]^2/2)*dt))
    
    # startprice[i] <- shares[1]*prices[i,1]+shares[2]*prices[i,2]
    startprice[i] <- shares*prices[i,]
    portPos[i] <- v0/startprice[i]
    
    for (j in 1:k){  
      port_px[j,i] <- (shares[1]*st1_bm[j,i]+shares[2]*st2_bm[j,i])*portPos[i]
    }
    
    MCVaR[i] <- v0-quantile(port_px[,i],1-p)
  }
  print(i)
  return(MCVaR)
}


ll <- MCVaR(v0, est$mu_gbm, est$sigma, est$rho,shares,p, npaths,years,dRtn)
plotGraph(ll,INTC$Dates)
# save(ll,file = "MCstocks.RData")

###################################
dRtn <- 5
dt <- dRtn/252
npaths <- 10
k <- npaths
years - 5
npts <- 252*years
ntrails <- length(est$rho)
ntrails <- 2000
mu <- est$mu_gbm
sigma <- est$sigma_gbm

st1_bm <- matrix(0,nrow=k,ncol=ntrails)
st2_bm <- matrix(0,nrow=k,ncol=ntrails)

for (i in 1:ntrails){
  n <- 1
  dt <- dRtn/252
  w <- corrbmsampset(dt,k,mu,rho)
  s0 <- prices
  
  st1_bm[,i] <- c(s0[i,1]*exp(sigma[i,1]*w[[i]][,1]+(mu[i,1]-sigma[i,1]^2/2)*dt))
  st2_bm[,i] <- c(s0[i,2]*exp(sigma[i,2]*w[[i]][,2]+(mu[i,2]-sigma[i,2]^2/2)*dt))
  print(i)
}

startprice <- NA
port_px <- matrix(0,nrow=k,ncol=ntrails)
portPos <- NA
MCVaR <- NA

for (i in 1:ntrails){
  startprice[i] <- shares*prices[i,]
  portPos[i] <- v0/startprice[i]
  
  for (j in 1:k){
    port_px[j,i] <- (shares[1]*st1_bm[j,i]+shares[2]*st2_bm[j,i])*portPos[i]
    }
}

for (i in 1:ntrails){
  MCVaR[i] <- v0-quantile(port_px[,i],1-p)
}

plotGraph(MCVaR,INTC$Dates)
  

