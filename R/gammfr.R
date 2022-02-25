#' Gamma Fill Rate ROP and SS Calculation for the Continuous Review (s, Q) Inventory Control Policy
#'
#' This function calculates the reorder point (ROP) or safety stock (SS) for a single inventory
#' item with a P2 proportion of demand fulfilled from available stock fill rate under the (s, Q)
#' inventory control policy using the gamma approximation. The gamma approximation
#' assumes that lead time demand is distributed according to the gamma distribution. This function
#' includes both the approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9.
#' For P2>0.9 the approach was derived by Tyworth, Guo, and Ganeshan (1996) and is also available
#' in popular textbooks e.g. Silver, Pyke and Thomas (2016, 311-312).
#' For P2<=0.9 see forthcoming paper by Foarth (2021).
#'
#' The output is the single inventory items' gamma ROP (s) or SS for the P2 fill rate target
#'
#' @param qty item's fixed order quantity
#' @param P2 item's fill rate target proportion demand fulfilled from available stock
#' @param muX mean of lead time demand
#' @param sigmX standard devation of lead time demand
#' @param Log if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The gamma approximation ROP or SS
#' @export
#'
#' @examples
#' gammfr(50,0.90,100,120)
gammfr<-function(qty,P2,muX,sigmX,Log){

  Gshape<-muX^2/sigmX^2
  Gscale<-muX/sigmX^2

gESC<-qty*(1-P2)
  if(P2>0.9){

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
    if(Log == TRUE){
      return(GammROP)
    }else{
      return(GammROP-muX)
    }
}
