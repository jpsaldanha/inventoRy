#' Normal Fill Rate ROP or SS Calculation from LTD data for the Continuous Review Inventory Control Policy
#'
#' This function calculates the reorder point (ROP) or safety stock (SS) for a single inventory
#' item with a fill rate P2 proportion of demand fulfilled from available stock under the
#' continuous review (s, Q) inventory control policy using the normal approximation. Note 's'
#' is the ROP and Q is a fixed order quantity. The normal approximation assumes that lead time
#' demand is distributed according to the normal distribution. This function includes both the
#' approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9. These approaches are available
#' in popular textbooks e.g. Silver, Pyke and Thomas (2016, 271), equations 6.28 and 6.29.
#'
#' The output is the single inventory items' normal ROP or SS for the P2 fill rate target
#'
#' @param qty item's fixed order quantity
#' @param P2 item's fill rate target proportion demand fulfilled from available stock
#' @param muX mean of lead time demand
#' @param sigmX standard devation of lead time demand
#' @param Log if TRUE returns the ROP else SS, default is TRUE
#'
#' @return the normal approximation ROP or SS
#' @export
#'
#' @examples
#' normfr(50,0.95,100,120)
normfr<-function(qty,P2,muX,sigmX,Log = TRUE){

  if(P2 > 0.9){

    normloss<-qty/sigmX*(1-P2)
    NormROP<-optimize(function(z){abs((dnorm(z)-z*(1-pnorm(z)))-normloss)}
                                        ,lower=0,upper = 6)$minimum*sigmX+muX
  }else{

    normloss<-qty/sigmX*(1-P2)
    NormROP<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-(dnorm(z+qty/sigmX)-
              (z+qty/sigmX)*(1-pnorm(z+qty/sigmX))))-normloss)},
              lower=0,upper = 6)$minimum*sigmX+muX
  }
  if(Log == TRUE){
    return(NormROP)
  }else{
    return(NormROP-muX)
  }
}
