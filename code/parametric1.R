# Calculate VaR and ES using Parametric Method (1)
# VaR by fitting GBM to index history

paramtricP <- function(v0,t,p,mu,vol){
  #v0 = Initial portfolio value
  #mu = Drift
  #sigma = Volatility.
  #p = Percentile at which to compute VaR.
  #t = Horizon at which to compute VaR (in years).
  #ES = Expected shortfall of portfolio (Assumes portfolio follows GBM)
  
  #vt <- w1*stock1+w2*stock2
  
  vt <- v0*exp((mu-sigma^2/2)*t+sigma*sqrt(t)*qnorm(1-p))
  VaR <- v0-vt
  ES <- v0*(1-exp(mu*t)/(1-p)*pnorm(qnorm(1-p)-sigma*sqrt(t)))
  return(ES)
  return(VaR)
}






