# parametric method to calculate option portfolio VaR
Option_ParametricVaR=function(s0, mu, sigma, rf1, nstocks, iv1, strike1, maturity1, 
                      rf2, iv2, strike2, maturity2, ncalls, nputs,
                      VaRp, horizon){
  dt <- horizon / 252
  ntrials <- length(mu) 
  st <- matrix(0,nrow=ntrials,ncol=1)
  for (i in 1:ntrials){
    st[i,1] <- s0*exp((mu[i]-sigma[i]^2/2)*dt+sigma[i]*sqrt(dt)*qnorm(1-VaRp))
  }
  vtStock = st * nstocks
  v0Stock = s0 * nstocks
  putt = Put(st, strike1, rf1, maturity1-horizon, iv1)
  calll = call(st, strike2, rf2, maturity2-horizon, iv2)
  vtPut = nputs * putt
  vtCall = ncalls * calll
  put0 = Put(s0, strike1, rf1, maturity1, iv1)
  call0 = Call(s0, strike2, rf2, maturity2, iv2)
  v0Put = nputs * put0
  v0Call = ncalls * call0
  loss = v0Stock + v0Put + v0Call - (vtStock + vtPut + vtCall)
  VaR = quantile(loss, 100*VaRp)
  return (VaR)
}