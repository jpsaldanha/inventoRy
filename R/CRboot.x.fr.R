#' Estimate Bootstrap ROP or SS from LTD Data for a Continuous Review Policy with a
#' Target Fill Rate
#'
#' The function uses an input vector of lead time demands (LTD) to determine the reorder
#' point (ROP) for a target P2 proportion of demand fulfilled from available stock i.e.
#' fill rate. for the continuous review inventory control policy. The full algorithm is
#' provided in Saldanha (2022). The inputs must include a LTD vector a P2 fill rate and a
#' fixed order quantity (qty). An optional number of B bootstrap resamples may be provided,
#' the default is set to 500. It is recommended not to set B<200 resamples. As B increases
#' (e.g. B>1000) the marginal accuracy gained is trivial and the run time will increase,
#' especially if used in batch runs to determine ROPs over hunderds of items. As per
#' Saldanha (2022) the ROP is estimated automatically using the traditional fill rate
#' estimator when P2>0.9 and the Silver (1970) approach otherwise.
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
#' @param qty the fixed order quantity
#' @param p2 the target fill rate
#' @param B the number of bootstrap resamples, default is 500
#' @param roptru if TRUE returns the ROP else the SS, default is the ROP
#' @param seed a random seed, default is the computer time
#'
#' @return the bootstrap estimate ROP or SS
#' @export
#' @import stats
#'
#' @examples
#' a<-rnorm(30,100,20)
#' # Returns ROP for fixed order qty=50 and 500 bootstrap resamples with a fill rate of 95%
#' CRboot.x.fr(a,50,.95,500)
#' # Returns the SS for fixed order qty=50 and 500 bootstrap resamples with a fill rate of 95%
#' CRboot.x.fr(a,50,.95,500,FALSE) # Returns the ROP and SS from the same random numbers
#' CRboot.x.fr(a,50,.95,500,TRUE,0)
#' CRboot.x.fr(a,50,.95,500,FALSE,0)
CRboot.x.fr<-function(x,qty,p2,B=500,roptru = TRUE,seed = as.numeric(Sys.time())){

  set.seed(seed)
  nX<-length(x)

  ExpShort<-function(x0,n)
  {
    short<-1:n
    for (i in 1:n)
    {
      short[i]<-(sum(x0[i:n]-x0[i])/n)
    }
    return(short)
  }

  SMExpShort<-function(x,n)
  {
    modshort<-(1:n)*0
    for (i in 1:(n-1))
    {
      j<-max(which((x[i]+qty-x)>0))
      modshort[i]<-qty-qty*i/n-(sum(x[i]+qty-x[(i+1):j])/n)
    }
    return(modshort)
  }

  maxneg<-function(vec)
  {
    if(length(which(vec<0))>0){
      out<-max(which(vec==max(vec[vec<0])))
    }else{
      out<-99
    }
    return(out)
  }

  minpos<-function(vect)
  {
    out<-min(which(vect==min(vect[vect>=0])))
    return(out)
  }

  BtSmplX<-replicate(B,sort(sample(x,size=nX,replace = TRUE)))
  BtSmplMu<-apply(BtSmplX,2,mean)

  TS<-qty*(1-p2)

  if(p2>0.9){

    esMat<-apply(BtSmplX,2,ExpShort,n=nX)

    diff<-TS-esMat

    lo<-apply(diff,2,maxneg)

    hi<-apply(diff,2,minpos)

    s_hat<-1:B

    for (k in 1:B)
    {

      if(length(which(diff[,k]==TS))==nX)
      {
        s_hat[k]<-BtSmplX[1,k]
      }

      else if(length(which(diff[,k]<0))==0 && length(which(diff[,k]==TS))<nX)
      {

        m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
        s_hat[k]<-BtSmplX[1,k]-(((TS-esMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                  (esMat[1,k]-esMat[m0,k]))
      }

      else
      {
        s_hat[k]<-BtSmplX[hi[k],k]-(((TS-esMat[hi[k],k])*(BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                      (esMat[lo[k],k]-esMat[hi[k],k]))
      }
#      if (is.nan(s_hat[k])) browser() # used for tracing the origin of the NaN values
    }

   if(roptru == TRUE){
    return(mean(s_hat))
   }else{
     return(mean(s_hat-BtSmplMu))
   }

######                     END BOOTSTRAP ALGORITHM FOR P2>0.9                      ######

  }else{

######   SILVER MODIFIED FILL RATE BOOTSTRAP ALGORITHM FOR FIXED qty & P_2 <= 0.9  ######

    SMesMat<-apply(BtSmplX,2,SMExpShort,n=nX)

    TS<-qty*(1-p2)

    diff<-TS-SMesMat

    lo<-apply(diff,2,maxneg)

    hi<-apply(diff,2,minpos)

    SMs_hat<-1:B

    for (k in 1:B) # Iterate over all B resamples
    {

      if(length(which(diff[,k]==0))>0)
      {

        SMs_hat[k]<-BtSmplX[which(diff[,k]==0),k]
      }

      if(length(which(BtSmplX[,k]==BtSmplX[1,k]))==nX)
      {
        SMs_hat[k]<-BtSmplX[1,k]
      }

      else if(length(which(SMesMat[,k]>TS))==0 && length(which(BtSmplX[,k]==BtSmplX[1,k]))<nX)
      {

        m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
        SMs_hat[k]<-BtSmplX[1,k]-(((TS-SMesMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                    (SMesMat[1,k]-SMesMat[m0,k]))
      }

      else
      {
        SMs_hat[k]<-BtSmplX[hi[k],k]-(((TS-SMesMat[hi[k],k])*(BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                        (SMesMat[lo[k],k]-SMesMat[hi[k],k]))
      }
#      if (is.nan(SMs_hat[k])) browser()
    }

    if(roptru == TRUE){
      return(mean(SMs_hat))
    }else{
      return(mean(SMs_hat-BtSmplMu))
    }
  }
}
