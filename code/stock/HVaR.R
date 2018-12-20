HVaR_S <- function(price,p,dRtn,year,v0){
  return <- diff(log(price),dRtn)
  N <- length(return)
  n <- year*252
  rtn_w <- NA
  Hvar <- NA
  
  for(i in 1:(N-n)){
    rtn_w[i] <- quantile(return[i:(i+n)],1-p)
    Hvar[i] <- v0*(1-exp(rtn_w[i]))
  }
  return(as.data.frame(Hvar))
}
