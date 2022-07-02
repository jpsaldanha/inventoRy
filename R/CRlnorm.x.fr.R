#' Lognormal Fill Rate ROP and SS Calculation from LTD data for the Continuous Review
#' Inventory Control Policy
#'
#' This function uses the input of a single lead time demand vector to calculate the ROP
#' for a single inventory item with a P2 proportion of demand fulfilled from available stock
#' fill rate under the continuous review (s, Q) inventory control policy using the lognormal
#' approximation. The lognormal approximation assumes that lead time demand is distributed
#' according to the lognormal distribution. This function includes both the approaches outlined
#' by Silver (1970) when P2<=0.9 and P2>0.9. Both of these functions are derived in the
#' forthcoming paper by Saldanha (2021).
#'
#' The output is the single inventory items' ROP or SS for the P2 fill rate target.
#' This function is primarily provided for use in theoretical studies and analysts must make
#' sure to ensure that the empirical data fits the lognormal distribution.
#'
#' @param x vector of lead time demands
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else the SS, default is the ROP
#'
#' @return the lognormal approximation reorder point (ROP)
#' @export
#' @import stats
#'
#' @examples
#' CRlnorm.x.fr(c(rlnorm(24,4.15917,0.944456)),0.95,50)
#' # This example returns a distributions with a mean of 100 and std. dev. of 120
#'
CRlnorm.x.fr<-function(x,p2,qty,roptru=TRUE){

# The error function
erf <- function(x0) 2 * pnorm(x0 * sqrt(2)) - 1

# Find the first and second moments of the lognormal distribution

  modl<-function(x0)
  {f1<-exp(x0[1]+(x0[2]^2)/2)-mean(x)
  f2<-sqrt((exp(x0[2]^2)-1)*exp(2*x0[1]+x0[2]^2))-sd(x)
  c(f1=f1,f2=f2)
  }
  (ss <- rootSolve::multiroot(f = modl, start = c(1, 1)))
  muX<-abs(ss$root[1])
  sigmX<-abs(ss$root[2])

  if(p2>0.9){

    LNormROP<-optimize(function(z){abs(abs(qty*(1-p2))-(1/2*(exp(muX+sigmX^2/2)-z-z*
              erf((muX-log(z))/(sigmX*sqrt(2)))+exp(muX+sigmX^2/2)*erf((muX+sigmX^2-log(z))/
              (sigmX*sqrt(2))))))},lower=0,upper=(100+6*sigmX))$minimum

  } else {

    LNormROP<-optimize(function(z){abs(p2-(qty/2*(1-erf((muX-log(z))/(sigmX*sqrt(2))))+(z+qty)/2*
              (erf((muX-log(z))/(sigmX*sqrt(2)))-erf((muX-log(z+qty))/(sigmX*sqrt(2))))-
              exp(muX+sigmX^2/2)/2*(erf((muX+sigmX^2-log(z))/(sigmX*sqrt(2)))-erf((muX+sigmX^2-
              log(z+qty))/(sigmX*sqrt(2)))))/qty)},lower=0,upper=(100+6*sigmX))$minimum

  }
  if(roptru == TRUE){
    return(LNormROP)
  }else{
    return(LNormROP-muX)
  }
}
