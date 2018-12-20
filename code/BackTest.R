BackTest <- function(Prices,VaR,windowlen,S0){
  #we set the horizon to 5 days. We also count the number of exceptions that occur in each 1 year
  exception <- rep(0,(length(VaR)-252*windowlen))
  shares1<-rep(NA,(length(VaR)-252*windowlen))
  pricet1<-rep(NA,(length(VaR)-252*windowlen))
  Loss<-rep(0,(length(VaR)-252*windowlen))
  for (i in 1:(length(VaR)-252*windowlen)){
    window_data <- Prices[(length(VaR)-i-252*windowlen):(length(VaR)-i-1)]
    shares<-rep(NA,247)
    pricet<-rep(NA,247)
    loss<-rep(NA,247)
    k <- VaR[(length(VaR)-i-252+1):(length(VaR)-i-252+247)]
    for (j in 1:length(window_data)-5){
      shares[j] <- S0/window_data[j]
      pricet[j] <- window_data[j+5]
      loss[j] <- S0 - pricet[j]*shares[j]
    }
  }
  return(expection)
}

# BackTest <- function(Prices,VaR,windowlen,S0){
#   exception <- rep(0,5040)
#   for (i in 1:(length(VaR)-252*windowlen)){
#     window_data <- Prices[(length(VaR)-i-252*windowlen):(length(VaR)-i-1)]
#     shares<-rep(NA,247)
#     pricet<-rep(NA,247)
#     loss<-rep(NA,247)
#     k <- VaR[(length(VaR)-i-252+1):(length(VaR)-i-252+247)]
#     for (j in 1:length(window_data)-5){
#       shares[j] <- S0/window_data[j]
#       pricet[j] <- window_data[j+5]
#       loss[j] <- S0 - pricet[j]*shares[j]
#     }
#     exception[i] <- sum(loss>k)
#   }
#   return(cbind(exception,loss))
# }