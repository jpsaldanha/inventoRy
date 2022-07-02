#' Normal Cycle Service Level ROP and SS Calculation from lead time demand data under the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P1
#' probability of no stockout (PNS) under the (s, Q) inventory control policy using the normal
#' approximation. The normal approximation assumes that lead time demand is distributed according
#' to the normal distribution. This function is available in popular textbooks e.g. Silver, Pyke
#' and Thomas (2016, 269-268).
#'
#' The output is the single inventory items' normal ROP (s) if the roptru parameter is TRUE
#' or not entered (default) else if FALSE the SS for the target PNS P1 is returned.
#'
#' @param x vector of lead time demands
#' @param p1 item's cycle service level as p1-th PNS
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The normal approximation ROP or SS
#' @export
#' @import stats
#'
#' @examples
#' CRnorm.x.csl(c(runif(24,200,400)),0.95)
CRnorm.x.csl<-function(x,p1,roptru = TRUE){

  muX<-mean(x)
  sigmX<-sd(x)

  NormROP<-qnorm(p1,muX,sigmX)

  if(roptru == TRUE){
    return(NormROP)
  }else{
    return(NormROP-muX)
  }
}
