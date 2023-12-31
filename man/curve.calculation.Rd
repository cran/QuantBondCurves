% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CurveCalibration.R
\name{curve.calculation}
\alias{curve.calculation}
\title{Curve calculation}
\usage{
curve.calculation(
  serie,
  market.assets = NULL,
  noSpots = 1,
  previous.curve = NULL,
  asset.type = "TES",
  freq = 1,
  rate.type = 1,
  daycount = NULL,
  fwd = 0,
  npieces = NULL,
  obj = "Price",
  Weights = NULL,
  nsimul = 1,
  piece.term = NULL,
  nodes = seq(0, 10, 0.001),
  approximation = "constant"
)
}
\arguments{
\item{serie}{A time series matrix that encompasses a sequence of IRR's emitted on distinct dates.
The columns correspond to different maturity periods, expressed in years,
while the row names indicate the precise dates when the rates were emitted.}

\item{market.assets}{A matrix containing market assets data, where the first column represents
the coupon rates of the assets and the second column represents their corresponding maturities as dates.
This input is required only if the IRR's of assets differs from their coupon rates.}

\item{noSpots}{Number of spot interest rates introduced in the \code{serie} input per row.
Function assumes spots are the first entries on the \code{serie} vector for every row.}

\item{previous.curve}{A matrix that stores historical curve values up to the earliest calibration date
on \code{serie}. The row names correspond to the dates on which the curve was calculated,
while the columns represent maturities as years.}

\item{asset.type}{String that determines the asset type to value. See also
'Details'.}

\item{freq}{Frequency of payments of a given asset in a year. For LIBOR and
IBR the default frequency is four (quarterly payments). TES has a default
frequency of one (annual payments).}

\item{rate.type}{(1) for annual compounded discount rates and (0) for continuosly
compounded discount rates. By default, rates are assumed to be the former.}

\item{daycount}{Day count convention. See also 'Details'.}

\item{fwd}{Numeric value that determines if the desired output curve is a
forward or a spot curve. Set \code{0} for spot curve (default), \code{1}
otherwise.}

\item{npieces}{Number of constant or linear segments for the curve to incorporate. By
default \code{NULL}, and bootstrapping method is used, otherwise, minimization of
RSS is used.}

\item{obj}{String related to the definition of the error in the RSS methodology.
Set \code{"Price"} for minimization of error by price or \code{"Rate"} for
minimization of error by rate.}

\item{Weights}{Vector of weights used to dot product with residual squares in order to
calculate residual sum of squares. By default, each residual is assigned the same weight.}

\item{nsimul}{Number of simulations for the terms of the pieces. The more simulations,
the more likely to find a better local solution. By default \code{1}, and terms are
defined in such way each piece occupies the same length in the abscissa axis.}

\item{piece.term}{Vector that establishes a unique term structure for optimization to take place.
Each piece or segment must have a unique maturity, as numeric value in years,
that signifies the end of the segment. Last segment maturity must not be introduced, it is assumed to be equivalent to
the last term introduced on analysis date. Therefore, the \code{piece.term} vector must always have a length equal to \code{npieces} - 1.}

\item{nodes}{Desired output nodes of the curve.}

\item{approximation}{String that establish the approximation. Set
\code{'linear'} for a piecewise linear approximation, or \code{'constant'} for a
piecewise constant curve.}
}
\value{
Zero Coupon curves for the corresponding analysis dates.
If \code{fwd = 1}, returns forward curves.
}
\description{
Function that calculates zero coupon or instantaneous
forward curves for multiple analysis dates, according to historical data of internal rates
of return (IRR) and coupon rates of assets.
Extends previous market rate curves by minimizing Mean Absolute Errors (MAE) following a
bootstrapping recursive method. Alternatively, methodology of residual sum of squares (RSS)
can be employed.
}
\details{
\code{asset.type} makes reference to the following type of assets:
\itemize{
   \item "TES" for Colombian Treasury Bonds (default).
   \item "IBRSwaps" for swaps indexed to IBR rate.
   \item "LIBORSwaps" for Interest Rate Swaps (IRS) indexed to 3M LIBOR.
   \item "FixedIncome" for assets that are indexed to a fixed income with
   different frequency of payments.
     }

If \code{npieces = NULL} uses a recursive iteration process based in
bootstrapping where the curve is constructed through a minimization of the MAE
between the dirty price of historical assets and an approximation of the theoretical
price of assets of same maturity. Uses the "L-BFGS-B" optimization method
to minimize the expected MAE. Otherwise, curve is constructed through minimization
of RSS where the error can be defined via price or rate.
}
\examples{
# Previous curve input
previous.curve <- matrix(0.04,nrow = 2,ncol = 8)
rownames(previous.curve) <- c("2014-01-01","2015-01-01")
colnames(previous.curve) <- c(0, 0.25, 0.5, 1:5)
# IRR's input
serie <- matrix(NA,nrow = 4,ncol = 6)
rownames(serie) <- c("2014-01-01","2015-01-01","2016-01-01","2017-01-01")
colnames(serie) <- c(0, 0.08333, 0.25, 0.5, 1, 2)
serie[1,1] <- 0.040; serie[1,2] <- 0.050; serie[1,3] <- 0.060; serie[1,4] <- 0.065
serie[1,5] <- 0.070; serie[1,6] <- 0.075
serie[2,1] <- 0.030; serie[2,2] <- 0.040; serie[2,3] <- 0.050; serie[2,4] <- 0.063
serie[2,5] <- 0.074; serie[2,6] <- 0.080
serie[3,1] <- 0.060; serie[3,2] <- 0.065; serie[3,3] <- 0.070; serie[3,4] <- 0.080
serie[3,5] <- 0.084; serie[3,6] <- 0.090
serie[4,1] <- 0.020; serie[4,2] <- 0.030; serie[4,3] <- 0.040; serie[4,4] <- 0.042
serie[4,5] <- 0.045; serie[4,6] <- 0.050
# Market Assets input
market.assets <- matrix(NA,nrow = 10,ncol = 2)
market.assets[1,1]  <- 0.040 ; market.assets[2,1]  <- 0.05
market.assets[3,1]  <- 0.060 ; market.assets[4,1]  <- 0.07
market.assets[5,1]  <- 0.080 ; market.assets[6,1]  <- 0.09
market.assets[7,1]  <- 0.060 ; market.assets[8,1]  <- 0.07
market.assets[9,1]  <- 0.075 ; market.assets[10,1] <- 0.07
market.assets[1,2]  <- "2016-01-01" ; market.assets[2,2]  <- "2016-02-01"
market.assets[3,2]  <- "2016-04-01" ; market.assets[4,2]  <- "2016-07-01"
market.assets[5,2]  <- "2017-01-01" ; market.assets[6,2]  <- "2017-02-01"
market.assets[7,2]  <- "2017-04-01" ; market.assets[8,2]  <- "2017-07-01"
market.assets[9,2]  <- "2018-01-01" ; market.assets[10,2] <- "2019-01-01"
#Calculation
curve.calculation(serie = serie, market.assets = market.assets,
                  previous.curve = previous.curve, asset.type = "TES",
                  freq = 1, rate.type = 1, fwd = 0,
                  nodes = c(0, 0.25, 0.5, 1:5), approximation = "linear")


}
