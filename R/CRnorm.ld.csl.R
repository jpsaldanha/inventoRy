#' Normal Cycle Service Level ROP and SS Calculation from lead-time and demand data for the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time and a vector of demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P1
#' probability of no stockout (PNS) under the (s, Q) inventory control policy using the normal
#' approximation. The normal approximation assumes that lead time demand is distributed according
#' to the normal distribution. This function is available in popular textbooks e.g. Silver, Pyke
#' and Thomas (2016, 269-268, 284-285).
#'
#' The output is the single inventory items' normal ROP (s) if the roptru parameter is TRUE
#' or not entered (default) else if FALSE the SS for the target PNS P1 is returned.
#' The demand should be a time series vector in the same units of time as lead time e.g.
#' if lead times are in weeks time series of demand should be aggregated weekly.
#'
#' @param x vector of lead times
#' @param y time series of demands in the same unit of time as lead time
#' @param p1 item's cycle service level as p1-th PNS
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The normal approximation ROP or SS
#' @export
#'
#' @examples
#' CRnorm.ld.csl(c(rnorm(24,100,120)),c(runif(24,20,40)),0.95)
CRnorm.ld.csl<-function(x,y,p1,roptru = TRUE){

  muLT<-mean(x)
  sigmLT<-sd(x)
  muD<-mean(y)
  sigmD<-sd(y)
  muX<-muLT*muD
  sigmX<-sqrt(muLT*sigmD^2+muD^2*sigmLT^2)

  NormROP<-qnorm(p1,muX,sigmX)

  if(roptru == TRUE){
    return(NormROP)
  }else{
    return(NormROP-muX)
  }
}
