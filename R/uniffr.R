#' Uniform Fill Rate ROP Calculation for the Continuous Review Inventory Control Policy
#'
#' This function calculates the ROP for a single inventory item with a P2 proportion
#' of demand fulfilled from available stock fill rate under the continuous review (s, Q)
#' inventory control policy using the unform approximation. The uniform approximation assumes
#' that lead time demand is distributed according to the uniform distribution. This function
#' includes both the approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9.
#' Both of these functions are derived in the forthcoming paper by Foarth (2021).
#'
#' @param qty item's fixed order quantity
#' @param P2 item's fill rate target proportion demand fulfilled from available stock
#' @param lb0 the lower bound of the uniform distribution
#' @param ub0 the upper bound of the uniform distribution
#'
#' @return The uniform approximation reorder point
#' @export
#'
#' @examples
#' uniffr(50,0.95,66,136)
uniffr<-function(qty,P2,lb0,ub0){

  if(P2>0.9){

    UnifROP<-ub0-sqrt(2*qty*(1-P2)*(ub0-lb0))

  } else {

    UnifROP<-P2*(ub0-lb0)+lb0-(qty/2)

  }
  return(UnifROP)
}
