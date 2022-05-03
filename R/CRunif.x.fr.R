#' Uniform Fill Rate ROP Calculation from LTD data for the Continuous Review Inventory
#' Control Policy
#'
#' This function uses the input of a single lead time demand vector to calculate the ROP
#' for a single inventory item with a P2 proportion of demand fulfilled from available stock
#' fill rate under the continuous review (s, Q) inventory control policy using the uniform
#' approximation. The uniform approximation assumes that lead time demand is distributed
#' according to the uniform distribution. This function includes both the approaches outlined
#' by Silver (1970) when P2<=0.9 and P2>0.9. Both of these functions are derived in the
#' forthcoming paper by Saldanha (2021).
#'
#' @param x vector of lead time demands
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The uniform approximation ROP or safety stock
#' @export
#'
#' @examples
#' CRunif.x.fr(c(runif(24,66,134)),0.95,50)
CRunif.x.fr<-function(x,p2,qty,roptru=TRUE){

  muX<-(x)
  sigmX<-(x)

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
