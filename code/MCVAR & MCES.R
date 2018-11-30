
# s0: Initial portfolio value
# mu: Drift
# sigma: Volatility
# p: 
# t: Default 5/252
# npaths: 

# MONTE CARLO
Monte_VaR <- function(s0, mu, sigma, p, t, npaths){
  MCVaR <- NULL
  len <- length(mu)
  for (j in 1:len){
    mc <- NULL
    for (i in 1:npaths){
      random <- rnorm(1,0,sqrt(t))
      temp <- s0 - (s0 * exp((sigma[j] * sqrt(t)  * qnorm(1-p)) 
                            + (mu[j]+random-sigma[j] ^ 2 / 2) * t))
      mc <- c(mc, temp)
    }
    MCVaR <- c(MCVaR, quantile(mc, p, na.rm = TRUE))
  }
  return(MCVaR)
}

Monte_ES <- function(s0, mu, sigma, p, t, npaths){
  MCES <- NULL
  len <- length(mu)
  for (j in 1:len){
    mc <- NULL
    for (i in 1:npaths){
      random <- rnorm(1,0,sqrt(t))
      temp <- s0 - (s0 * exp((sigma[j] * sqrt(t)  * qnorm(1-p)) 
                             + (mu[j]+random-sigma[j] ^ 2 / 2) * t))
      mc <- c(mc, temp)
    }
    es <- mc[mc > quantile(mc, p, na.rm = TRUE)]
    ESvalue <- mean(es)
    MCES <- c(MCES, ESvalue)
  }
  return(MCES)  
}