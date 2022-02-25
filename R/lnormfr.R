#' Lognormal Fill Rate ROP Calculation for the Continuous Review Inventory Control Policy
#'
#' This function calculates the ROP for a single inventory item with a P2 proportion
#' of demand fulfilled from available stock fill rate under the continuous review (s, Q)
#' inventory control policy using the lognormal approximation. The lognormal approximation assumes
#' that lead time demand is distributed according to the lognormal distribution. This function
#' includes both the approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9.
#' Both of these functions are derived in the forthcoming paper by Foarth (2021).
#'
#' This function is primarily provided for use in theoretical studies and analysts must make sure
#' to convert the standard means (mean) and standard deviations (sd) to the first and second moments
#' of the lognormal distribution.
#'
#' @param qty item's fixed order quantity
#' @param P2 item's fill rate target proportion demand fulfilled from available stock
#' @param muX first moment (mean) of lognormally distributed lead time demand
#' @param sigmX second moment (standard devation) of lognormally distributed lead time demand
#'
#' @return the lognormal approximation reorder point (ROP)
#' @export
#'
#' @examples
#' lnormfr(50,0.95,4.585559829,0.1980422)
#' This example returns a distributions with an average of 100 and corresponding std. dev. of 120
lnormfr<-function(qty,P2,muX,sigmX){

  if(P2>0.9){

    LNormROP<-optimize(function(z){abs(abs(qty*(1-P2))-(1/2*(exp(muX+sigmX^2/2)-z-z*
              erf((muX-log(z))/(sigmX*sqrt(2)))+exp(muX+sigmX^2/2)*erf((muX+sigmX^2-log(z))/
              (sigmX*sqrt(2))))))},lower=0,upper=(100+6*sigmX))$minimum

  } else {

    LNormROP<-optimize(function(z){abs(P2-(qty/2*(1-erf((muX-log(z))/(sigmX*sqrt(2))))+(z+qty)/2*
              (erf((muX-log(z))/(sigmX*sqrt(2)))-erf((muX-log(z+qty))/(sigmX*sqrt(2))))-
              exp(muX+sigmX^2/2)/2*(erf((muX+sigmX^2-log(z))/(sigmX*sqrt(2)))-erf((muX+sigmX^2-
              log(z+qty))/(sigmX*sqrt(2)))))/qty)},lower=0,upper=(100+6*sigmX))$minimum

  }
  return(LNormROP)
}
