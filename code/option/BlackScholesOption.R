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