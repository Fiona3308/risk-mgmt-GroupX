# parametric method to calculate option portfolio VaR
Option_ParametricVaR <- function(price0,s0, mu, sigma, rf, nstocks, iv1, maturity1, 
                      iv2, maturity2, ncalls, nputs,
                      VaRp, horizon){
  dt <- horizon / 252
  ntrials <- length(mu) 
  st <- matrix(0,nrow=ntrials,ncol=1)
  for (i in 1:ntrials){
    st[i,1] <- price0*exp((mu[i]-sigma[i]^2/2)*dt+sigma[i]*sqrt(dt)*qnorm(1-VaRp))
  }
  vtStock <- st * nstocks
  v0Stock <- s0 * nstocks
  putt <- Put(st, st, rf, maturity1-horizon, iv1)
  calll <- Call(st, st, rf, maturity2-horizon, iv2)
  vtPut <- nputs * putt
  vtCall <- ncalls * calll
  put0 <- Put(s0, s0, rf, maturity1, iv1)
  call0 <- Call(s0, s0, rf, maturity2, iv2)
  v0Put <- nputs * put0
  v0Call <- ncalls * call0
  
  loss <- v0Stock + v0Put + v0Call - (vtStock + vtPut + vtCall)
  VaR <- quantile(loss, VaRp)
  return (VaR)
}