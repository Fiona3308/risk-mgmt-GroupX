Loss <- function(Prices,VaR,windowlen,S0){
  exception <- rep(0,(length(VaR)-252*windowlen-5))
  shares<-rep(NA,(length(VaR)-252*windowlen-5))
  pricet<-rep(NA,(length(VaR)-252*windowlen-5))
  loss<-rep(0,(length(VaR)-252*windowlen-5))
  for (i in 1:(length(VaR)-252*windowlen-5)){
      shares[i] <- S0/Prices[i]
      pricet[i] <- Prices[i+5]
      loss[i] <- S0 - pricet[i]*shares[i]
    }
  return(cbind(loss,VaR[length(VaR)-252*windowlen-5]))
}
 
