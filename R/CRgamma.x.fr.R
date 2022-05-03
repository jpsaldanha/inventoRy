#' Gamma Fill Rate ROP and SS Calculation from LTD data for the Continuous Review
#' Inventory Control Policy
#'
#' This function uses the input of a single vector of lead time demands to calculate the
#' reorder point (ROP) or safety stock (SS) for a single inventory item with a P2 proportion
#' of demand fulfilled from available stock fill rate under the (s, Q) inventory control policy
#' using the gamma approximation. The gamma approximation assumes that lead time demand is
#' distributed according to the gamma distribution. This function includes both the approaches
#' outlined by Silver (1970) when P2<=0.9 and P2>0.9. For P2>0.9 the approach was derived by
#' Tyworth, Guo, and Ganeshan (1996) and is also available in popular textbooks e.g. Silver,
#' Pyke and Thomas (2016, 311-312). For P2<=0.9 see forthcoming paper by Saldanha (2021).
#'
#' The output is the single inventory items' gamma ROP (s) if the roptru parameter is TRUE
#' or not entered (default) else if FALSE the SS for the P2 fill rate target is returned.
#'
#' @param x vector of lead time demands
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The gamma approximation ROP or SS
#' @export
#'
#' @examples
#' CRgamma.x.fr(c(runif(24,100,200)),0.90,50)
CRgamma.x.fr<-function(x,p2,qty,roptru=TRUE){

  muX<-mean(x)
  sigmX<-sd(x)

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
