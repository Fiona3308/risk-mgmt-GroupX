
# MC method to calculate option portfolio VaR
Option_MCVaR <- function(s0, mu, sigma, rf, nstocks, iv1, maturity1, 
                      iv2, maturity2, ncalls, nputs,
                   VaRp, horizon, npath){
  tv <-  horizon / 252  
  bm <- sqrt(tv) * rnorm(length(mu), 0, 1)
  st <- s0 * exp(sigma * bm + (mu - sigma ^ 2 / 2) * tv)
  vtStock <- st * nstocks
  v0Stock <- s0 * nstocks

  putt <- Put(st, st, rf, maturity1-horizon, iv1)   # assume at the money, strike=st 
  calll <- Call(st, st, rf, maturity2-horizon, iv2)
  
  vtPut <- nputs * putt
  vtCall <- ncalls * calll
  put0 <- Put(s0, s0, rf, maturity1, iv1)
  call0 <- Call(s0,s0,rf, maturity2, iv2)
  
  v0Put <- nputs * put0
  v0Call <- ncalls * call0
  loss <- v0Stock + v0Put + v0Call - (vtStock + vtPut + vtCall)
  VaR <- quantile(loss, VaRp)
  return (VaR)
}


mu <- par1$mu_gbm
sigma <- par1$sigma_gbm


Option_MCVaR(s0, mu, sigma, rf, nstocks, iv1, strike1, maturity1, 
                      iv2, strike2, maturity2, ncalls, nputs,
                      VaRp, horizon, npath)

