#' @title A  Dichotomy Method for Solving Roots of Equations using R
#' @description A  Dichotomy Method for Solving Roots of Equations using R
#' @param fun the function that needed to solving roots
#' @param a endpoint
#' @param b endpoint
#' @return  the roots of function
#' @examples
#' fun<-function(x){x^3-x-1}
#'dic(fun,3,2)
#' @export
dic<-function(fun,a,b,eps=1e-5){
  if(fun(a)*fun(b)>0)
  {list(fail="无根")}
  else{
    while((b-a)>1e-5)
    {if(fun((a+b)/2)*fun(a)<0)
    {b=(a+b)/2}
      else{a=(a+b)/2}
    }
  }
  return((a+b)/2)}

