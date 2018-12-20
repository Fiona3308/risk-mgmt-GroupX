# Parameters of the portfolio (only include two stocks)

# Output: mu and sigma of each stock in the portfolio, rho of two stocks

# Inputs:
#   prices=vector of daily historical prices, from oldest to newest 
#   prices <- comb.col(s1,s2) 
#   year=#of year when rolling window
#   win_len=length of window to use in estimation (in days)
#   
# Returns:
#   rtn = log returns
#   mubar = mean of log returns
#   sigmabar = standard deviation of log returns
#   mu = estimates for drift
#   sigma = estimates for sigma
#   rho = if prices has 2 price series, rho has the correlation of the brownian motion drivers.

# combine two stocks into one dataframe with the same length
comb.col <- function(dat1, dat2) {
  n.row <- min(length(dat1),length(dat2))
  length(dat1) <- n.row
  length(dat2) <- n.row
  cbind(dat1, dat2)
}


winEstGBM2 <- function(prices,dRtn,year){
  wid_len <- year*252
  rtn <- -diff(log(prices),dRtn)                                           # log returns
  rtnsq <- rtn^2                                                           # square of log returns
  period <- nrow(rtn)-wid_len                                              # period for loop 
  datadt <- dRtn/252
  mu_bar <- matrix(NA,nrow = period,ncol=ncol(prices))                     # mean of log returns
  var_bar <- matrix(NA,nrow = period,ncol=ncol(prices))                    # standard deviation of log returns
  mu_gbm <- matrix(NA,nrow = period,ncol=ncol(prices))                     # estimates for mu of price
  sigma_gbm <- matrix(NA,nrow = period,ncol=ncol(prices))                  # estimates for sigma of price
  
  for (i in 1:period){
    mu_bar[i,] <- t(colMeans(rtn[i:(i+wid_len-1),]))
    var_bar[i,] <- t(apply(rtn[i:(i+wid_len-1),],2,var))
    
    sigma_gbm[i,] <- sqrt(var_bar[i,]/(dRtn/252))
    mu_gbm[i,] <- mu_bar[i,]/(dRtn/252) + (sigma_gbm[i,]^2)/2
  }
  
  if (ncol(prices)==2){
    rho <- cov(rtn[,1],rtn[,2])/datadt/(sigma_gbm[,1]*sigma_gbm[,2])
  }else{
    rho <- NA
  }
  return(list("mu_gbm"=mu_gbm,"sigma_gbm"=sigma_gbm,"rho"=rho))
}


