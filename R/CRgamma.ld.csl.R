#' Gamma Fill Rate ROP and SS Calculation from lead-time and demand data for the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time and a vector of demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P1
#' probability of stockout (PNS) under the (s, Q) inventory control policy using the gamma
#' approximation. The gamma approximation assumes that lead time demand is distributed according
#' to the gamma distribution. This function is available in popular textbooks e.g. Silver, Pyke
#' and Thomas (2016, 311-312).
#'
#' The output is the single inventory items' gamma ROP (s) if the roptru parameter is TRUE
#' or not entered (default) else if FALSE the SS for the target PNS P1 is returned.
#' The demand should be a time series vector in the same units of time as lead time e.g.
#' if lead times are in weeks time series of demand should be aggregated weekly .
#'
#' @param x vector of lead times
#' @param y time series of demands in the same unit of time as lead time
#' @param p1 the cycle service level as PNS
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The gamma approximation ROP or SS
#' @export
#'
#' @examples
#' CRgamma.ld.csl(c(runif(24,2,8)),c(runif(24,20,40)),0.90)
CRgamma.ld.fr<-function(x,y,p1,roptru=TRUE){

  muLT<-mean(x)
  sigmLT<-sd(x)
  muD<-mean(y)
  sigmD<-sd(y)
  muX<-muLT*muD
  sigmX<-sqrt(muLT*sigmD^2+muD^2*sigmLT^2)

  Gshape<-muX^2/sigmX^2
  Gscale<-muX/sigmX^2

  GammROP<-qgamma(p1,Gshape,Gscale)

  if(roptru == TRUE){
    return(GammROP)
  }else{
    return(GammROP-muX)
  }
}
