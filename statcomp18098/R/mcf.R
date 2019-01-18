#' @title a function to compute a Monto Carlo estimate of the Beta(3,3) cdf
#' @description  a function to compute a Monto Carlo estimate of the Beta(3,3) cdf
#' @param a the endpoint of the intergret
#' @return  the solution of the function
#' @examples
#' a<-3
#' MCF(a)
#' @export
MCF<-function(a){
  m<-1e4
  t<-runif(m,0,a)# get the uniform sample
  theta.hat <- mean(30*a*t^2*(1-t)^2)# get the Monto Carlo estimate
  print(theta.hat)
}
