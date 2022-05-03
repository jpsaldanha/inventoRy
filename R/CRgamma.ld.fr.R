#' Gamma Fill Rate ROP and SS Calculation from lead-time and demand data for the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time and a vector of demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P2
#' proportion of demand fulfilled from available stock fill rate under the (s, Q) inventory
#' control policy using the gamma approximation. The gamma approximation assumes that lead time
#' demand is distributed according to the gamma distribution. This function includes both the
#' approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9. For P2>0.9 the approach was
#' derived by Tyworth, Guo, and Ganeshan (1996) and is also available in popular textbooks e.g.
#'  Silver, Pyke and Thomas (2016, 311-312). For P2<=0.9 see forthcoming paper by
#'  Saldanha (2021).
#'
#' The output is the single inventory items' gamma ROP (s) if the roptru parameter is TRUE
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
#' @return The gamma approximation ROP or SS
#' @export
#'
#' @examples
#' CRgamma.ld.fr(c(runif(24,2,8)),c(runif(24,20,40)),0.90,50)
CRgamma.ld.fr<-function(x,y,p2,qty,roptru=TRUE){

  muLT<-mean(x)
  sigmLT<-sd(x)
  muD<-mean(y)
  sigmD<-sd(y)
  muX<-muLT*muD
  sigmX<-sqrt(muLT*sigmD^2+muD^2*sigmLT^2)

  Gshape<-muX^2/sigmX^2
  Gscale<-muX/sigmX^2

gESC<-qty*(1-p2)
  if(p2>0.9){

    GammROP<-optimize(function(s){abs((Gshape/Gscale*(1-pgamma(s,Gshape+1,
                      Gscale))-s*(1-pgamma(s,Gshape,Gscale)))-gESC)},
                      lower=0,upper=qgamma(0.9999,Gshape,Gscale))$minimum

  } else {

    GammROP<-optimize(function(s)
    {abs(Q-(qty*pgamma(s,Gshape,Gscale)+(s+qty)*(pgamma(s+qty,Gshape,Gscale)-
                                               pgamma(s,Gshape,Gscale))-Gshape/Gscale*
              (pgamma(s+qty,Gshape+1,Gscale)-pgamma(s,Gshape+1,Gscale)))-gESC)
    },lower=0,upper=qgamma(0.9999,Gshape,Gscale))$minimum

  }
    if(roptru == TRUE){
      return(GammROP)
    }else{
      return(GammROP-muX)
    }
}
