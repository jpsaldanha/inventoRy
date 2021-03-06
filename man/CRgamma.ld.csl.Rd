% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CRgamma.ld.csl.R
\name{CRgamma.ld.csl}
\alias{CRgamma.ld.csl}
\title{Gamma Cycle Service Level ROP and SS Calculation from lead-time and demand data for the
Continuous Review Inventory Control Policy}
\usage{
CRgamma.ld.csl(x, y, p1, roptru = TRUE)
}
\arguments{
\item{x}{vector of lead times}

\item{y}{time series of demands in the same unit of time as lead time}

\item{p1}{the cycle service level as PNS}

\item{roptru}{if TRUE returns the ROP else SS, default is TRUE}
}
\value{
The gamma approximation ROP or SS
}
\description{
This function uses the input of a vector of lead time and a vector of demand to calculate
the reorder point (ROP) or safety stock (SS) for a single inventory item with a P1
probability of no stockout (PNS) under the (s, Q) inventory control policy using the gamma
approximation. The gamma approximation assumes that lead time demand is distributed according
to the gamma distribution. This function is available in popular textbooks e.g. Silver, Pyke
and Thomas (2016, 311-312).
}
\details{
The output is the single inventory items' gamma ROP (s) if the roptru parameter is TRUE
or not entered (default) else if FALSE the SS for the target PNS P1 is returned.
The demand should be a time series vector in the same units of time as lead time e.g.
if lead times are in weeks time series of demand should be aggregated weekly.
}
\examples{
CRgamma.ld.csl(c(runif(24,2,8)),c(runif(24,20,40)),0.90)
}
