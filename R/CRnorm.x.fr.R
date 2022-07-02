#' Normal Fill Rate ROP or SS Calculation from LTD data for the Continuous Review Inventory
#' Control Policy
#'
#' This function uses the input of a single lead time demand vector to calculate the reorder
#' point (ROP) or safety stock (SS) for a single inventory item with a fill rate P2 proportion
#' of demand fulfilled from available stock under the continuous review (s, Q) inventory control
#' policy using the normal approximation. Note 's' is the ROP and Q is a fixed order quantity.
#' The normal approximation assumes that lead time demand is distributed according to the normal
#' distribution. This function includes both the approaches outlined by Silver (1970) when
#' P2<=0.9 and P2>0.9. These approaches are available in popular textbooks e.g.
#' Silver, Pyke and Thomas (2016, 271), equations 6.28 and 6.29.
#'
#' The output is the single inventory items' normal ROP or SS for the P2 fill rate target
#'
#' @param x vector of lead time demands
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return the normal approximation ROP or SS
#' @export
#' @import stats
#'
#' @examples
#' CRnorm.x.fr(c(rnorm(24,100,120)),0.95,50)
CRnorm.x.fr<-function(x,p2,qty,roptru = TRUE){

  muX<-mean(x)
  sigmX<-sd(x)

  if(p2 > 0.9){

    normloss<-qty/sigmX*(1-p2)
    NormROP<-optimize(function(z){abs((dnorm(z)-z*(1-pnorm(z)))-normloss)}
                                        ,lower=0,upper = 6)$minimum*sigmX+muX
  }else{

    normloss<-qty/sigmX*(1-p2)
    NormROP<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-(dnorm(z+qty/sigmX)-
              (z+qty/sigmX)*(1-pnorm(z+qty/sigmX))))-normloss)},
              lower=0,upper = 6)$minimum*sigmX+muX
  }
  if(roptru == TRUE){
    return(NormROP)
  }else{
    return(NormROP-muX)
  }
}
