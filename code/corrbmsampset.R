
#Inputs: 
  # n = n steps of size n*dt year
  # dt = returnPeriod/252 (eg,1/252,5/252)
#Returns: k sample paths for a pair of brownian motions, N steps of size DT years

bmsampset <- function(n,dt,k){
  x``
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