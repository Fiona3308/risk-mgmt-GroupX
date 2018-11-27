expEstGBM <- function(prices,dRtn,lambda){
  # prices = stock prices
  # dRtn = 1/5/10 days return
  # lambda = lambda for weight calculation
  
  rtn <- -diff(log(prices),dRtn)   # log returns of stock price  
  mu_bar <- NA                     # mean of log returns
  var_bar <- NA                    # standard deviation of log returns
  mu_gbm <- NA                     # estimates for mu of price
  sigma_gbm <- NA                  # estimates for sigma of price
  
  wid_len <- ceiling(log(0.01)/log(lambda))
  if (wid_len > 5000){
    wid_len = 5000
  }  
  period <- length(rtn)-wid_len    # period for loop 
 
  w <- lambda^(1:wid_len)
  w <- w/sum(w)
  
  for (i in 1:period){
    mu_bar[i] <- mean((rtn[i:(i+wid_len-1)])*w)
    var_bar[i] <- var((rtn[i:(i+wid_len-1)])*w)
    
    sigma_gbm[i] <- sqrt(var_bar[i]/(dRtn/252))
    mu_gbm[i] <- mu_bar[i]/(dRtn/252) + (sigma_gbm[i]^2)/2
  }
  
  est <- as.data.frame(cbind(mu_gbm,sigma_gbm))
  return(est)
}



