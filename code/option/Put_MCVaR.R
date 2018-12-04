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
Put_MCVaR=function(s0, mu, sigma, rf, iv, strike, maturity, nstocks, 
                      nputs, VaRp, horizon, npath){
  ######################
  tv = seq(0,n*horizon,horizon)  #horizon=1, n=?, dt=horizon?
  b = rowSums(sqrt(horizon) * as.matrix(rnorm(npath*n), nrow = npath, ncol = n))
  a = matrix(0, nrow = npath, ncol = 1)
  bm = cbind(a,b)
  ######################
#   tv = matrix(rep(1,npath))*horizon
#   bm = sqrt(horizon) * as.matrix(rnorm(npath))
  st = s0 * exp(sigma * bm + (mu - sigma ^ 2 / 2) * tv)
  vtStock = st * nstocks
  v0Stock = s0 * nstocks
  putt = Put(st, strike, rf, maturity-horizon, iv)
  vtPut = nputs * putt
  put0 = Put(s0, strike, rf, maturity, iv)
  v0Put = nputs * put0
  loss = v0Stock + v0Put - (vtStock + vtPut)
  VaR = quantile(loss, 100*VaRp)
  return (VaR)
}