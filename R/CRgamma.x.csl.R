#' Gamma Cycle Service Level ROP and SS Calculation from lead time demand data under the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P1
#' probability of no stockout (PNS) under the (s, Q) inventory control policy using the gamma
#' approximation. The gamma approximation assumes that lead time demand is distributed according
#' to the gamma distribution. This function is available in popular textbooks e.g. Silver, Pyke
#' and Thomas (2016, 311-312).
#'
#' The output is the single inventory items' gamma ROP (s) if the roptru parameter is TRUE
#' or not entered (default) else if FALSE the SS for the target PNS P1 is returned.
#'
#' @param x vector of lead time demands
#' @param p1 the cycle service level as PNS
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The gamma approximation ROP or SS
#' @export
#' @import stats
#'
#' @examples
#' CRgamma.x.csl(c(runif(24,200,400)),0.90)
CRgamma.x.csl<-function(x,p1,roptru=TRUE){

  muX<-mean(x)
  sigmX<-sd(x)

  Gshape<-muX^2/sigmX^2
  Gscale<-muX/sigmX^2

  GammROP<-qgamma(p1,Gshape,Gscale)

  if(roptru == TRUE){
    return(GammROP)
  }else{
    return(GammROP-muX)
  }
}
