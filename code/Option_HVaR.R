# Historical method to calculate option portfolio VaR
source("../code/BlackScholesOption.R")

Option_HVaR <- function(s0, price, year, rf, nstocks, iv1, strike1, maturity1, 
                      iv2, strike2, maturity2, ncalls, nputs,
                      VaRp, horizon){
  vtStock <- price * nstocks
  putt <- Put(price, strike1, rf, maturity1-horizon, iv1)
  calll <- call(price, strike2, rf, maturity2-horizon, iv2)
  vtPut <- nputs * putt
  vtCall <- ncalls * calll
  vt <- vtStock + vtPut + vtCall
  rtn <- -diff(log(vt), horizon)
  N <- length(rtn)
  n <- year*252
  temp <- rep(NA, N-n)
  v0Stock <- s0 * nstocks
  put0 <- Put(s0, strike1, rf, maturity1, iv1)
  call0 <- Call(s0, strike2, rf, maturity2, iv2)
  v0Put <- nputs * put0
  v0Call <- ncalls * call0
  v0 <- v0Stock + v0Put + v0Call
  for(i in 1:(N-n)){
    temp[i] <- quantile(rtn[i:(i+n)],1-VaRp)
    VaR[i] <- v0*(1-exp(temp[i]))
  }
  return (VaR)
}