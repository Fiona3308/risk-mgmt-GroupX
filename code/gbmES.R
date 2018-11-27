gbmES<-function(v0,t,p,mu,vol){
  #v0 = Initial portfolio value
  #mu = Drift
  #sigma = Volatility.
  #p = Percentile at which to compute VaR.
  #t = Horizon at which to compute VaR (in years).
  #ES = Expected shortfall of portfolio (Assumes portfolio follows GBM)
  
  ES <- v0*(1-exp(mu*t)/(1-p)*pnorm(qnorm(1-p)-sigma*sqrt(t)))
  return(ES)
}