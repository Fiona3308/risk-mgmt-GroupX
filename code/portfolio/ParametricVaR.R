
# portfolio = stock1 * N1 + stock2 * N2
# V = S1 * N1 + S2 * N2
# dS1 = u1S1dt + u2S2dt
# dw1dW2 = rho*dt
# Assuming S1 and S2 follows GBM, Portfolio follows Normal distribution

# Args
# a = position vector (number of shares of each stock)
# s = initial stock price vector
# mu = drift for each stock
# sigma = volatility fro each stock
# rho = correlation matrci of driving brownian motion
# p = percentile at which to compute VaR
# t = horizon at which to compute VaR (in years)

# Returns
# VaR = VaR calculated from mean and variance of portfolio
#       Assumes portfolio is normally distributed.
#       VaR=V0-Vt=V0-(E[VT]-qnorm(p)sd[VT])
# v0 = current portfolio value
# evt = expected value of portfolio at horizon
# evt2 = expected value of square of portfolio value at horizon
# sdvt = standard deviation of portfolio values at horizon

a <- c(N1,N2)  # if we only assign weights to each component of portfolio? how to decide position here?
s0 <- c(s1_0,s2_0)  # what's s1_0 and s2_0 here, is the dailty stock price?
v0 <- T
mu <- c(mu1,mu2)
sigma <- c(sigma1,sigma2)
datadt <- 1/252
rtn1 <- -diff(log(s1),1)
rtn2 <- -diff(log(s2),1)
rho <- cov(rtn1,rtn2)/sigma1*sigma2*datadt
t <- 5/252 # horizon


parametricVaR <- function(a,s0,mu,sigma,rho,p,t){
  
  # v0
  value <- 0 
  for (i in 1:length(a)){
    value <- value+a[i]*s0[i]    # initial value of portfolio
  }
  
  if (v0 == T){
    v0
  }else{
    v0 <- value
  }
  
  # evt
  exp_vt <- 0
  for(i in 1:length(a)){
    exp_vt <- exp_vt + a[i]*s0[i]*exp(mu[i]*t)
  }
  
  # evt2
  part1 <- 0
  for (i in 1:length(a)){
    part1 <- part1 + a[i]^2*s0[i]^2*exp((2*mu[i]+sigma[i]^2)*t)
  }
  
  part2 <- 2*a[1]*a[2]*s0[1]*so[2]*exp((mu[1]+mu[2]+rho*sigma[1]*sigma[2])*t)
  
  exp_vt2 <- part1+part2
  
  # var[vt]
  varvt <- exp_vt2-exp_vt^2
  
  # sd[vt]
  sdvt <- sqrt(varvt)
  
  # VaR
  VaR <- v0 - exp_vt+qnorm(p)*sdvt
  
  return(VaR)
}
