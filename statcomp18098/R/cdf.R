#' @title  A discrete random variable x has probability mass function using R
#' @description  A discrete random variable x has probability mass function using R
#' @param x Variable Distribution Sequence
#' @return  the probability mass of random variable x
#' @examples
#' x=c(1,2,2,2,3)/10
#' cdf(x)
#' @export
cdf<-function(x){
  p=cumsum(x)#P(x<=i)=p[i+1]
  len=1000
  r=findInterval(runif(len),p)-1#generate random number
  print(table(r)/len)#relative table
  rsample=sample(0:4,len,T,x)#ort of sample(0:4,size = sample_len,replace = T,prob = x)
  return(table(rsample)/len)}
