gbmES <- function(v0,mu,sigma,p,t){
  #v0 = Initial portfolio value
  #mu = Drift of price
  #sigma = Volatility of price
  #p = Percentile at which to compute ES
  #t = Horizon at which to compute VaR (in years).(eg,1/252,5/252)
  #ES = Expected shortfall of portfolio (Assumes portfolio follows GBM)
  t=t/252 # change the horizon unit from day to year
  
  ES <- v0*(1-exp(mu*t)/(1-p)*pnorm(qnorm(1-p)-sigma*sqrt(t)))
  return(ES)
}