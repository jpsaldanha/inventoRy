#' Uniform Fill Rate ROP and SS Calculation from lead-time and demand data for the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time and a vector of demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P2
#' proportion of demand fulfilled from available stock fill rate under the (s, Q) inventory
#' control policy using the uniform approximation. The uniform approximation assumes that lead
#' time demand is distributed according to the uniform distribution. This function includes
#' both the approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9. This approach is
#' available in popular textbooks e.g. Silver, Pyke and Thomas (2016, 311-312).
#' For P2<=0.9 see forthcoming paper by Saldanha (2022).
#'
#' The output is the single inventory items' uniform ROP (s) if the roptru parameter is TRUE
#' or not entered (default) else if FALSE the SS for the P2 fill rate target is returned.
#' The demand should be a time series vector in the same units of time as lead time e.g.
#' if lead times are in weeks time series of demand should be aggregated weekly .
#'
#' @param x vector of lead times
#' @param y time series of demands in the same unit of time as lead time
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The uniform approximation ROP or SS
#' @export
#' @import stats
#'
#' @examples
#' CRunif.ld.fr(c(runif(24,66,134)),c(runif(24,20,40)),0.95,50)
CRunif.ld.fr<-function(x,y,p2,qty,roptru=TRUE){

  muLT<-mean(x)
  sigmLT<-sd(x)
  muD<-mean(y)
  sigmD<-sd(y)
  muX<-muLT*muD
  sigmX<-sqrt(muLT*sigmD^2+muD^2*sigmLT^2)

  if(p2>0.9){

    UnifROP<-muX+sigmX*sqrt(3)-2*sqrt(qty*(1-p2)*sigmX*sqrt(3))

  } else {

    UnifROP<-sigmX*sqrt(3)*(2*p2-1)+muX-qty/2

  }
  if(roptru == TRUE){
    return(UnifROP)
  }else{
    return(UnifROP-muX)
  }
}
