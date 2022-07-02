#' Estimate Bootstrap ROP or SS from LTD Data for a Continuous Review Policy with a
#' Target Fill Rate
#'
#' The function uses an input vector of lead time demands (LTD) to determine the reorder
#' point (ROP) for a target P1 cycle service level i.e. probability of no stockouts
#' PNS. The full algorithm is provided in Bookbinder and Lordahl (1989). The inputs must
#' include a LTD vector and a P1 PNS. An optional number of B bootstrap resamples may be
#' provided, the default is set to 500. It is recommended not to set B<200 resamples. As B
#' increases (e.g. B>1000) the marginal accuracy gained is trivial and the run time will
#' increase, especially if used in batch runs to determine ROPs over hunderds of items.
#'
#' The ROP is the default value returned from the function but if safety stock is desired
#' then an optional input can be added where "roptru=FALSE" will return the safety stock.
#'
#' Note that the bootstrap approach utilizes a random resampling of the input LTD resamples
#' with replacement. The random seed for the random resampling is set to the current machine's
#' system time. Hence, if replicability of the results are desired e.g., the ROP and safety
#' stock for a number of items need to be calculated then it is recommended to use the input
#' "seed=" (see examples).
#'
#' @param x a vector of lead time demands
#' @param p1 the cycle service level as PNS
#' @param B the number of bootstrap resamples, default is 500
#' @param roptru if TRUE returns the ROP else the SS, default is the ROP
#' @param seed a random seed, default is the computer time
#'
#' @return the bootstrap estimate ROP or SS
#' @export
#' @import stats
#'
#' @examples
#' a<-rnorm(30,100,20) # Returns ROP with 500 bootstrap resamples for a PNS of 95%
#' CRboot.x.csl(a,.95,500) # Returns the SS with 500 bootstrap resamples for a PNS of 95%
#' CRboot.x.csl(a,.95,500,FALSE) # Returns the ROP and SS from the same random numbers
#' CRboot.x.csl(a,.95,500,TRUE,0)
#' CRboot.x.csl(a,.95,500,FALSE,0)
CRboot.x.csl<-function(x,p1,B=500,roptru = TRUE,seed = as.numeric(Sys.time())){

  set.seed(seed)
  nX<-length(x)

  BtSmplX0<-replicate(B,sort(sample(x,size=nX,replace = TRUE)))
  BtSmplMu0<-apply(BtSmplX0,2,mean)

  BtQuant<-apply(BtSmplX0,2,quantile,probs=p1,names=FALSE)

  if(roptru == TRUE){
    return(mean(BtQuant))
  }else{
    return(mean(BtQuant-BtSmplMu0))
  }
}
