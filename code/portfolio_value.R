# wgt <- c("0.5","o.3","0.1","0.1")
# p_price <- cbind(s1,s2,call,put)
# Assume weights are kept the same of each position each day in the portfilio

comb.col <- function(dat1, dat2) {
  n.row <- min(length(dat1),length(dat2))
  length(dat1) <- n.row
  length(dat2) <- n.row
  cbind(dat1, dat2)
}

portfolio_px <- function(prices,wgts,positions,v0){
  port_px <- NA
  for(i in 1:length(positions)){
      if (positions[i] == "short"){
        wgts[i] <- (-1)*wgts[i]
      }else{
    wgts[i]
      }
    
    for(j in 1:nrow(prices)){
    num <-  wgt*v0/prices[1,]
    port_px[j] <- sum(prices[j,]*num)
    }
  }
  return(port_px)
}


