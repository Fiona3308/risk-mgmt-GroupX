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

#############################################

# HVaR_S <- function(pirce,p,dRtn,year){
#   stocklog <- log(price)
#   npts <- year*252
#   npaths <- npts-dRtn
#   ntrials <- length(price)-npts
#   stockRtn[i] <- stocklog[i]-stocklog[i+4]
#   
#   
#   
#   N <- length(return)
#   n <- 5*252
#   v0 <- 10000
#   rtn_w <- NA
#   Hvar <- NA
#   
#   for(i in 1:(N-n)){
#     rtn_w[i] <- quantile(return[i:(i+n)],1-p)
#     Hvar[i] <- v0*(1-exp(rtn_w[i]))
#   }
#   return(as.data.frame(Hvar))
# }
# 
# #####################################################33
# 
# dRtn <- -diff(log(XOM$PX_LAST),5)
# 
# rtn <- c()
# for (i in 1:length(XOM$PX_LAST)){
#   logr <- log(XOM$PX_LAST)
#   rtn[i] <- logr[i]-logr[i+4]
# }
# 
# View(rtn)
# 
# 
# m <- c(1,3,5,6,7)
# -diff(m)
