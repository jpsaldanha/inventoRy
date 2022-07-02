#' Estimate Bootstrap ROP or SS from Lead-Time and Demand Data for a Continuous Review Policy
#' with a Target Cycle Service Level
#'
#' The function uses one input vector each of lead time and demand to determine the reorder
#' point (ROP) for a target P1 cycle service level i.e. probability of not stocking out
#' (PNS). for the continuous review inventory control policy. The full algorithm is
#' provided in Saldanha et al. (2021). The inputs must include lead time and demand vectors
#' and a P1 PNS. An optional number of B bootstrap resamples may be provided, the default is
#' set to 500. It is recommended not to set B<200 resamples. As B increases (e.g. B>1000) the
#' marginal accuracy gained is trivial and the run time will increase, especially if used in
#' batch runs to determine ROPs over hunderds of items.
#'
#' The ROP is the default value returned from the function but if safety stock is desired
#' then an optional input can be added using "roptru=FALSE" will return the safety stock.
#'
#' Note that the bootstrap approach utilizes a random resampling of the input lead time and
#' demand resamples with replacement. The random seed for the random resampling is set to the
#' current machine's system time. Hence, if replicability of the results are desired e.g., the
#' ROP and safety stock for a number of items need to be calculated then it is recommended to
#' use the input "seed=" (see examples).
#'
#' @param x a vector of lead-times
#' @param y a vector of demands
#' @param p1 the cycle service level as PNS
#' @param B the number of bootstrap resamples, default is 500
#' @param roptru if TRUE returns the ROP else the SS, default is the ROP
#' @param seed a random seed, default is the computer time
#'
#' @return
#' @export
#' @import stats
#'
#' @examples
#' a<-rnorm(24,5,1)
#' b<-rnorm(24,20,5) #Returns the ROP using 500 bootstrap resamples with a PNS of 95%
#' CRboot.ld.csl(a,b,.95,500) #Returns the SS using 500 bootstrap resamples with a PNS of 95%
#' CRboot.ld.csl(a,b,.95,500,FALSE) #Returns the ROP and SS from the same random numbers
#' CRboot.ld.csl(a,b,.95,500,TRUE,0)
#' CRboot.ld.csl(a,b,.95,500,FALSE,0)
CRboot.ld.csl<-function(x,y,p1,B=500,roptru = TRUE,seed = as.numeric(Sys.time())){

  set.seed(seed)
  nL<-length(x)

  # Define a function to construct LTD mixtures from empirical lead time and demand
  ltd<-function(p,q,n0){
    X0<-1:n0
    #    p0<-sample(p,n0,TRUE)
    for(i in 1:n0)
    {
      X0[i]<-sum(sample(q,floor(p[i]),TRUE))+((p[i]-floor(p[i]))*sample(q,1))
    }
    return(X0)
  }

  BtSmplX0<-replicate(B,sort(ltd(x,y,nL)))
  BtSmplMu0<-apply(BtSmplX0,2,mean)

  BtQuant<-apply(BtSmplX0,2,quantile,probs=p1,names=FALSE)

  if(roptru == TRUE){
    return(mean(BtQuant))
  }else{
    return(mean(BtQuant-BtSmplMu0))
  }
}
