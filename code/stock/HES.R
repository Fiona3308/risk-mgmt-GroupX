ES_S<- function(price,p,dRtn,year,v0){
  return <- -diff(log(price),dRtn)
  N <- length(return)
  n <- year*252
  v0 <- 10000
  rtn_w <- NA
  Hes <- NA
  mu <- NA
  
  for(i in 1:(N-n)){
    rtn_w[i] <- quantile(return[i:(i+n)],1-p)
    mu[i] <- mean(return[which(return < rtn_w[i])])
    Hes[i] <- v0*(1-exp(mu[i]))
  }
  return(as.data.frame(Hes))
}