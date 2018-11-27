# Calculate VaR and ES using Parametric Method (2)
# VaR by fitting GBM to constituent history and assuming portfolio is normally distributed




paramtricS <- function(v0,dt,w1,w2,mu1,mu2,sigma1,sigma2,varp,esp){
  #v0 = Initial portfolio value
  #mu = Drift
  #sigma = Volatility.
  #varp = Percentile at which to compute VaR.
  #dt = Horizon at which to compute VaR (in years).
  #ES = Expected shortfall of portfolio (Assumes portfolio follows GBM)
  
  rho <- cov(rtn1,rtn2)/(sigma1*sigma2*(dt))
  vt <- w1*stock1+w2*stock2
  vt <- v0*exp((mu-sigma^2/2)*dt+sigma*sqrt(dt)*qnorm(1-varp))
  VaR <- v0-vt
  ES <- v0*(1-exp(mu*dt)/(1-sp)*pnorm(qnorm(1-esp)-sigma*sqrt(dt)))
  return(ES)
}