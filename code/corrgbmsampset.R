# mu: matrix
# sigma: matrix
# s0: matrix


# stock1Shares <- 156
# stock2Shares <- 200

stock1 <- XOM$PX_LAST
stock2 <- INTC$PX_LAST

comb.col <- function(dat1, dat2) {
  n.row <- min(length(dat1),length(dat2))
  length(dat1) <- n.row
  length(dat2) <- n.row
  cbind(dat1, dat2)
}

v0 <- 10000
prices <- comb.col(stock1,stock2)
weight <- c(0.5,0.5)
shares <- weight*v0/prices[1,]

MCVaR <- function(s0, mu, sigma,rho, p, npaths,years,dRtn){
  dRtn <- 5
  s0 <- 10000
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
  
  for (i in 1:ntrials){
    n <- 1
    dt <- dRtn/252
    w <- corrbmsampset(n,dt,k,mu,rho)
    
    st1_bm[,i] <- c(s0*exp(sigma[i,1]*w[[i]][,1]+(mu[i,1]-sigma[i,1]^2/2)*dt))
    st2_bm[,i] <- c(s0*exp(sigma[i,2]*w[[i]][,2]+(mu[i,2]-sigma[i,2]^2/2)*dt))
    
    startprice[i] <- shares[1]*prices[i,1]+shares[2]*prices[i,2]
    # port_px <- s0*(shares[1]*st1_bm

    MCVaR[i] <- s0-quantile(st_bm[i,],1-p)
  }
  
  
  return(MCVaR)
  
}

startprice <- stock1shares*stock1[i]+stock2shares*stock2[i]
  


dt<-5/252
st1_bm[,1] <- c(s0*exp(sigma_gbm[1,1]*bm[[1]][,1]+(mu_gbm[1,1]-sigma_gbm[1,1]^2/2)*dt))
View(st1_bm)
