gbmVaR <- function(v0,mu,sigma,p,t){
  #v0 = Initial portfolio value
  #mu = Drift
  #sigma = Volatility.
  #p = Percentile at which to compute VaR.
  #t = Horizon at which to compute VaR (in years).(eg.1/252)
  #VaR = VaR of portfolio (Assumes portfolio follows GBM)
  
  vt <- v0*exp((mu-sigma^2/2)*t+sigma*sqrt(t)*qnorm(1-p))
  VaR <- v0-vt
  return(VaR)
}