# create plot function for VaR
BTplot <- function(index,Dates){
  # set up the length of two variable
  if (class(index)=="data.frame"){
    minlen <- min(nrow(index),length(Dates))
    index <- index[1:minlen,]
  }else{
    minlen <- min(length(index),length(Dates))
    length(index) <- minlen
  }
  Dates <- Dates[1:minlen]
  
  # create a dataframe
  df <- cbind.data.frame(Dates,index)
  
  # plot
  p<-ggplot()+
    geom_line(aes(x=df$Dates,y=df$index))
  
  return(p+labs(x="",y="exceptions"))
}