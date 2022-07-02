#' Truncated normal Fill Rate ROP and SS Calculation from lead-time and demand data for the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time and a vector of demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P2
#' proportion of demand fulfilled from available stock fill rate under the (s, Q) inventory
#' control policy using the truncated normal approximation. The truncated normal approximation
#' assumes that lead time demand is distributed according to the truncated normal distribution.
#' This function includes both the approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9.
#' These approaches are available in popular textbooks e.g. Silver, Pyke and Thomas
#' (2016, 311-312). For P2<=0.9 see forthcoming paper by Saldanha (2022).
#'
#' The output is the single inventory items' truncated normal ROP (s) if the roptru parameter
#' is TRUE or not entered (default) else if FALSE the SS for the P2 fill rate target is
#' returned. The demand should be a time series vector in the same units of time as lead time
#'  e.g. if lead times are in weeks time series of demand should be aggregated weekly .
#'
#' @param x vector of lead times
#' @param y time series of demands in the same unit of time as lead time
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The truncated normal approximation ROP or SS
#' @export
#' @import stats
#'
#' @examples
#' CRtnorm.ld.fr(c(rnorm(24,100,120)),c(runif(24,20,40)),0.95, 50)
CRtnorm.ld.fr<-function(x,y,p2,qty,roptru=TRUE){

# The error function
erf <- function(x0) 2 * pnorm(x0 * sqrt(2)) - 1

  muLT<-mean(x)
  sigmLT<-sd(x)
  muD<-mean(y)
  sigmD<-sd(y)
  muX0<-muLT*muD
  sigmX0<-sqrt(muLT*sigmD^2+muD^2*sigmLT^2)

  mu0<-muX0
  sig0<-sigmX0
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
