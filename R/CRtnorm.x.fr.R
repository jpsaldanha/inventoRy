#' Truncated Normal Fill Rate ROP Calculation from LTD data for the Continuous Review Inventory
#'  Control Policy
#'
#' This function uses a single lead time demand vector to calculate the reorder point (ROP)
#' for a single inventory item with a P2 proportion of demand fulfilled from available stock
#' fill rate under the continuous review (s, Q) inventory control policy using the truncated
#' normal approximation. The truncated normal approximation assumer that lead time demand is
#' distributed according to the truncated normal distribution. This function includes both the
#' approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9. Both of these functions are
#' derived in the forthcoming paper by Saldanha (2021).
#'
#' @param x vector of lead time demands
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return the truncated normal approximation ROP or safety stock
#' @export
#'
#' @examples
#' CRtnorm.x.fr(c(rnorm(24,100,120)),0.95, 50)
CRtnorm.x.fr<-function(x,p2,qty,roptru=TRUE){

  mu0<-mean(x)
  sig0<-sd(x)
  a0<-(0-mu0)/sig0
  z0<-1-pnorm(a0)
  muX<-mu0+(dnorm(a0)-1)*sig0/z0
  sigmX<-sig0*sqrt(1+(a0*pnorm(a0))/z0-(pnorm(a0)/z0)^2)

  if(p2>0.9){

    TNormROP<-optimize(function(z){abs(qty*(1-p2)-(((muX-z)*
                                                      sqrt(pi/2)+sigmX*exp(-(1/2)*((z-muX)/
              sigmX)^2)+sigmX*sqrt(pi/2)*((z-muX)/sigmX)*erf((z-muX)/
                                                               (sigmX*sqrt(2))))/(sqrt(2*pi)*
              (1-0.5*(1+erf(-muX/(sigmX*sqrt(2))))))))},lower=0,upper=(muX+6*sigmX))$minimum

  } else {

   TNormROP<-optimize(function(z){abs(p2-((1/(2*sqrt(pi)*(1-(1/2)*
                                                            (1+erf(-muX/(sigmX*sqrt(2)))))))*
             (sigmX*sqrt(2)*(exp(-(qty+z-muX)^2/(2*sigmX^2))-exp(-(z-muX)^2/(2*sigmX^2)))+
             sqrt(pi)*((muX-z)*erf((z-muX)/(sigmX*sqrt(2)))+
             (qty+z-muX)*erf((qty+z-muX)/(sigmX*sqrt(2)))+qty*erf(muX/(sigmX*sqrt(2))))))/qty)},
             lower=0,upper=(muX+6*sigmX))$minimum

  }
  if(roptru == TRUE){
    return(TNormROP)
  }else{
    return(TNormROP-muX)
  }
}
