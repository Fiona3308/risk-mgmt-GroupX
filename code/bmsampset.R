# Create function to generate random variable with monte carlo simulation of a standard brownian motion

#n = n steps of size n*dt year
#dt = returnPeriod/252 (eg,1/252,5/252) 
#k = k paths/scenarios

bmsampset <- function(n,dt,k){

  w <- matrix(0,nrow = k,ncol = n)
  s <- matrix(0,nrow = k,ncol = n)
  for (i in 1:n){
    for(r in 1:k){
      w[r,i] <- sqrt(dt)*rnorm(1,mean=0,sd=1)
    }
  }
  
  if(n==1){
      s[,1] <- w[,1]
    }else{
      for (j in 2:n){
        s[,1] <- w[,1]
        s[,j] <- s[,j-1]+w[,j] 
      }
    }
    
  bm <- cbind(0,s)
}


# dt <- 5/252
# k <- 10
# m <- bmsampset(1,5/252,10)
# m <- bmsampset(3,5/252,10)
# View(m)
