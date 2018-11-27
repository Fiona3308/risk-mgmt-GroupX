HVaR_S <- function(return,p){
  N <- length(return)
  n <- 5*252
  v0 <- 10000
  rtn_w <- NA
  Hvar <- NA
  
  for(i in 1:(N-n)){
    rtn_w[i] <- quantile(return[i:(i+n)],1-p)
    Hvar[i] <- v0*(1-exp(rtn_w[i]))
  }
  return(as.data.frame(Hvar))
}