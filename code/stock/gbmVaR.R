gbmVaR <- function(v0,mu,sigma,p,dRtn){
  #v0 = Initial portfolio value
  #mu = Drift of prices
  #sigma = Volatility of prices
  #p = Percentile at which to compute VaR.
  #dt = Horizon at which to compute VaR (in years).(eg.1/252)
  #VaR = VaR of portfolio (Assumes portfolio follows GBM)
  dt=dRtn/252 # change the horizon unit from day to year
  
  vt <- v0*exp((mu-sigma^2/2)*dt+sigma*sqrt(dt)*qnorm(1-p))
  VaR <- v0-vt
  return(VaR)
}