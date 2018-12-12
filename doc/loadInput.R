# load input matrics
  
s1 <- XOM$PX_LAST
s2 <- INTC$PX_LAST

comb.col <- function(dat1, dat2) {
  n.row <- min(length(dat1),length(dat2))
  length(dat1) <- n.row
  length(dat2) <- n.row
  cbind(dat1, dat2)
}

dRtn <- 5
year <- 5
v0 <- 10000
wgt <- c(156/(156+200),200/(156+200))
p_price <- comb.col(s1,s2)
prices <- p_price
s0 <- p_price[1,] 
a <-  wgt*v0/s0

par1 <- winEstGBM(prices[,1],dRtn,year)
par2 <- winEstGBM(prices[,2],dRtn,year)
mu <- comb.col(par1$mu_gbm,par2$mu_gbm)
sigma <- comb.col(par1$sigma_gbm,par2$sigma_gbm)

rho <- function(prices,dRtn,year){
  datadt <- dRtn/252
  par1 <- winEstGBM(prices[,1],dRtn,year)
  par2 <- winEstGBM(prices[,2],dRtn,year)
  mu <- comb.col(par1$mu_gbm,par2$mu_gbm)
  sigma <- comb.col(par1$sigma_gbm,par2$sigma_gbm)
  rtn1 <- -diff(log(prices[,1]),dRtn)
  rtn2 <- -diff(log(prices[,2]),dRtn)
  # rtn <- comb.col(rtn1,rtn2)
  rho <- cov(rtn1,rtn2)/datadt/(sigma[,1]*sigma[,2])
  return(rho)
}

rho <- rho(p_price,1,5)
t <- dRtn/252
p <- 0.99
a <- c(156,200)

PVaR <- parametricVaR(a,s0,mu,sigma,rho,p,t,v0)
plotGraph(PVaR,INTC$Dates)
