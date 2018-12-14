# BackTest <- function(Prices,VaR,windowlen,S0){
#   exception <- rep(NA,length(VaR))
#   for (i in 1:length(VaR)){
#     window_data <- Prices[(length(VaR)-i-252*windowlen):(length(VaR)-i-1)]
#     shares<-rep(NA,length(window_data)-5)
#     pricet<-rep(NA,length(window_data)-5)
#     loss<-rep(NA,length(window_data)-5)
#     k <- VaR[(length(VaR)-i-252+1):(length(VaR)-i-252+length(window_data)-5)]
#     for (j in 1:length(window_data)-5){
#       shares[j] <- S0/window_data[j]
#       pricet[j] <- window_data[j+5]
#       loss[j] <- S0 - pricet[j]*shares[j]
#     }
#     exception[i] <- sum(loss>k)
#   }
#   return(exception)
# }
# 


BackTest <- function(Prices,VaR,windowlen,S0){
  exception <- rep(0,5040)
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
      #      exception[i] <- sum(loss>k)
    }
    exception[i] <- sum(loss>k)
  }
  return(exception)
}