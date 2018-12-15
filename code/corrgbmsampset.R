# mu: matrix
# sigma: matrix
# s0: matrix


# stock1Shares <- 156
# stock2Shares <- 200

stock1 <- XOM$PX_LAST
stock2 <- INTC$PX_LAST

source("../code/parameter/winEstGBM2.R")
source("../code/corrbmsampset.R")

prices <- comb.col(stock1,stock2)

par1 <- winEstGBM(XOM$PX_LAST,5,5)
par2 <- winEstGBM(INTC$PX_LAST,5,5)

mu <- comb.col(par1$mu_gbm,par2$mu_gbm)
sigma <- comb.col(par1$sigma_gbm,par2$sigma_gbm)

rho <- winEstGBM2(prices,5,5)

v0 <- 10000

weight <- c(0.5,0.5)
shares <- weight*v0/prices[1,]

MCVaR <- function(v0, mu, sigma,rho, p, npaths,years,dRtn){
  v0 <- 10000
  dt <- dRtn/252
  p <- 0.99
  years <- 5 
  npaths <- 10000
  k <- npaths
  
  npts <- 252*years
  ntrails <- length(rho)
  
  
  MCVaR <- NA
  st1_bm <- matrix(0,nrow=k,ncol=ntrails)
  st2_bm <- matrix(0,nrow=k,ncol=ntrails)
  startprice <- NA
  port_px <- matrix(0,nrow=k,ncol=ntrails)
  
  for (i in 1:ntrails){
    n <- 1
    dt <- dRtn/252
    w <- corrbmsampset(n,dt,k,mu,rho)
    
    st1_bm[,i] <- c(v0*exp(sigma[i,1]*w[[i]][,1]+(mu[i,1]-sigma[i,1]^2/2)*dt))
    st2_bm[,i] <- c(v0*exp(sigma[i,2]*w[[i]][,2]+(mu[i,2]-sigma[i,2]^2/2)*dt))
    
    startprice[i] <- shares[1]*prices[i,1]+shares[2]*prices[i,2]
    
    for (j in 1:k){
      port_px[j,i] <- v0*(shares[1]*st1_bm[j,i]+shares[2]*st2_bm[j,i])/startprice[i]
    }
    
    MCVaR[i] <- v0-quantile(port_px[,i],1-p)
  }
  return(MCVaR)
}

  


dt<-5/252
st1_bm[,1] <- c(v0*exp(sigma_gbm[1,1]*bm[[1]][,1]+(mu_gbm[1,1]-sigma_gbm[1,1]^2/2)*dt))
View(st1_bm)



mm <- MCVaR(v0, mu, sigma,rho, p, npaths,years,dRtn)
