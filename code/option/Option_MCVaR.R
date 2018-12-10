Put <- function(S0, K, r, T, vol) {
  d1  <-  (log(S0/K) + (r + vol^2/2)*T) / (vol*sqrt(T))
  d2  <-  d1 - vol*sqrt(T)
  p <- -S0 * pnorm(-d1) + K*exp(-r*T)*pnorm(-d2)       #put price
  return(p)
}

Call <- function(S0, K, r, T, vol) {
  d1  <-  (log(S0/K) + (r + vol^2/2)*T) / (vol*sqrt(T))
  d2  <-  d1 - vol*sqrt(T)
  c <- S0 * pnorm(d1) - K*exp(-r*T)*pnorm(d2)            #call price
  return(c)
}

# MC method to calculate option portfolio VaR
Option_MCVaR=function(s0, mu, sigma, rf1, nstocks, iv1, strike1, maturity1, 
                      rf2, iv2, strike2, maturity2, ncalls, nputs,
                   VaRp, horizon, npath){
  tv = horizon / 252  
  bm = sqrt(tv) * rnorm(length(mu), 0, 1)
  st = s0 * exp(sigma * bm + (mu - sigma ^ 2 / 2) * tv)
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
