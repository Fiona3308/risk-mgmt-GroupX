gbmES <- function(v0,mu,sigma,p,dRtn){
  #v0 = Initial portfolio value
  #mu = Drift of price
  #sigma = Volatility of price
  #p = Percentile at which to compute ES
  #dt = Horizon at which to compute VaR (in years).(eg,1/252,5/252)
  #ES = Expected shortfall of portfolio (Assumes portfolio follows GBM)
  dt=dRtn/252 # change the horizon unit from day to year
  
  ES <- v0*(1-exp(mu*dt)/(1-p)*pnorm(qnorm(1-p)-sigma*sqrt(dt)))
  return(ES)
}