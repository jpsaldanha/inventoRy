#' Lognormal Fill Rate ROP and SS Calculation from lead-time and demand data for the
#' Continuous Review Inventory Control Policy
#'
#' This function uses the input of a vector of lead time and a vector of demand to calculate
#' the reorder point (ROP) or safety stock (SS) for a single inventory item with a P2
#' proportion of demand fulfilled from available stock fill rate under the (s, Q) inventory
#' control policy using the lognormal approximation. The lognormal approximation assumes that
#' lead time demand is distributed according to the lognormal distribution. This function
#' includes both the approaches outlined by Silver (1970) when P2<=0.9 and P2>0.9. These
#' approaches are available in popular textbooks e.g. Silver, Pyke and Thomas (2016, 311-312).
#' For P2<=0.9 see forthcoming paper by Saldanha (2022).
#'
#' The output is the single inventory items' lognormal ROP (s) if the roptru parameter is TRUE
#' or not entered (default) else if FALSE the SS for the P2 fill rate target is returned.
#' The demand should be a time series vector in the same units of time as lead time e.g.
#' if lead times are in weeks time series of demand should be aggregated weekly .
#'
#' @param x vector of lead times
#' @param y time series of demands in the same unit of time as lead time
#' @param p2 item's fill rate target proportion demand fulfilled from available stock
#' @param qty item's fixed order quantity
#' @param roptru if TRUE returns the ROP else SS, default is TRUE
#'
#' @return The lognormal approximation ROP or SS
#' @export
#'
#' @examples
#' CRlnorm.ld.fr(c(runif(24,2,8)),c(runif(24,20,40)),0.95,50)
#' This example returns a distributions with a mean of 100 and std. dev. of 120
#'
CRlnorm.ld.fr<-function(x,y,p2,qty,roptru=TRUE){

# Find the first and second moments of the lognormal distribution

  muLT<-mean(x)
  sigmLT<-sd(x)
  muD<-mean(y)
  sigmD<-sd(y)
  muX0<-muLT*muD
  sigmX0<-sqrt(muLT*sigmD^2+muD^2*sigmLT^2)

  modl<-function(x)
  {f1<-exp(x[1]+(x[2]^2)/2)-muX0
  f2<-sqrt((exp(x[2]^2)-1)*exp(2*x[1]+x[2]^2))-sigmX0
  c(f1=f1,f2=f2)
  }
  (ss <- multiroot(f = modl, start = c(1, 1)))
  muX<-abs(ss$root[1])
  sigmX<-abs(ss$root[2])

  if(p2>0.9){

    LNormROP<-optimize(function(z){abs(abs(qty*(1-p2))-(1/2*(exp(muX+sigmX^2/2)-z-z*
              erf((muX-log(z))/(sigmX*sqrt(2)))+exp(muX+sigmX^2/2)*erf((muX+sigmX^2-log(z))/
              (sigmX*sqrt(2))))))},lower=0,upper=(100+6*sigmX))$minimum

  } else {

    LNormROP<-optimize(function(z){abs(p2-(qty/2*(1-erf((muX-log(z))/(sigmX*sqrt(2))))+(z+qty)/
              2*(erf((muX-log(z))/(sigmX*sqrt(2)))-erf((muX-log(z+qty))/(sigmX*sqrt(2))))-
              exp(muX+sigmX^2/2)/2*(erf((muX+sigmX^2-log(z))/(sigmX*sqrt(2)))-erf((muX+sigmX^2-
              log(z+qty))/(sigmX*sqrt(2)))))/qty)},lower=0,upper=(100+6*sigmX))$minimum

  }
  if(roptru == TRUE){
    return(LNormROP)
  }else{
    return(LNormROP-muX)
  }
}
