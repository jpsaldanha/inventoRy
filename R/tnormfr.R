#' Truncated Normal Fill Rate ROP Calculation for the Continuous Review Inventory Control Policy
#'
#' This function calculates the ROP for a single inventory item with a P2 proportion
#' of demand fulfilled from available stock fill rate under the continuous review (s, Q)
#' inventory control policy using the truncated normal approximation. The truncated normal
#' approximation assumer that lead time demand is distributed according to the truncated normal
#'  distribution. This function includes both the approaches outlined by Silver (1970)
#' when P2<=0.9 and P2>0.9. Both of these functions are derived in the forthcoming paper
#' by Foarth (2021).
#'
#' @param qty item's fixed order quantity
#' @param P2 item's fill rate target proportion demand fulfilled from available stock
#' @param muX mean of lead time demand
#' @param sigmX standard devation of lead time demand
#'
#' @return the truncated normal approximation reorder point (ROP)
#' @export
#'
#' @examples
#' tnormfr(50,0.95,100,120)
tnormfr<-function(qty,P2,muX,sigmX){

  if(P2>0.9){

    TNormROP<-optimize(function(z){abs(qty*(1-P2)-(((muX-z)*sqrt(pi/2)+sigmX*exp(-(1/2)*((z-muX)/
              sigmX)^2)+sigmX*sqrt(pi/2)*((z-muX)/sigmX)*erf((z-muX)/(sigmX*sqrt(2))))/(sqrt(2*pi)*
              (1-0.5*(1+erf(-muX/(sigmX*sqrt(2))))))))},lower=0,upper=(muX+6*sigmX))$minimum

  } else {

   TNormROP<-optimize(function(z){abs(P2-((1/(2*sqrt(pi)*(1-(1/2)*(1+erf(-muX/(sigmX*sqrt(2)))))))*
             (sigmX*sqrt(2)*(exp(-(qty+z-muX)^2/(2*sigmX^2))-exp(-(z-muX)^2/(2*sigmX^2)))+
             sqrt(pi)*((muX-z)*erf((z-muX)/(sigmX*sqrt(2)))+
             (qty+z-muX)*erf((qty+z-muX)/(sigmX*sqrt(2)))+qty*erf(muX/(sigmX*sqrt(2))))))/qty)},
             lower=0,upper=(muX+6*sigmX))$minimum

  }
  return(TNormROP)
}
