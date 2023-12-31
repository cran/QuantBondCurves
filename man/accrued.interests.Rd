% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalFunctions.R
\name{accrued.interests}
\alias{accrued.interests}
\title{Accrued interest}
\usage{
accrued.interests(
  maturity,
  analysis.date = Sys.Date(),
  coupon.rate,
  principal = 1,
  asset.type = "TES",
  freq = NULL,
  daycount = "ACT/360"
)
}
\arguments{
\item{maturity}{Last day of the contract: YYYY-MM-DD.
Alternatively, it can be a numeric value that represents the duration of the contract in years.}

\item{analysis.date}{Date in which the asset is valued. By default, the
current date.}

\item{coupon.rate}{Coupon rate of the asset. Can be an unique numeric
value or a vector corresponding to each coupon payment date.}

\item{principal}{Notional amount for the asset.}

\item{asset.type}{String that determines the asset type to value. See also
'Details'.}

\item{freq}{Frequency of payments of a given asset in a year. For LIBOR and
IBR the default frequency is four (quarterly payments). TES has a default
frequency of one (annual payments).}

\item{daycount}{Day count convention. See also 'Details'.}
}
\value{
Accrued interest of the bond from the last coupon payment to the
liquidation (valuation date).
}
\description{
Calculates the accumulated coupon or accrued interests of the
asset, from its last coupon or cash flow payment.
}
\details{
\code{asset.type} makes reference to the following type of assets:
\itemize{
   \item "TES" for Colombian Treasury Bonds (default).
   \item "FixedIncome" for assets that are indexed to a fixed income with
   different frequency of payments.
   \item "IBR" for bonds and assets indexed to 3M IBR rate.
   \item "IBRSwaps" for swaps indexed to IBR rate.
   \item "LIBOR" for bonds and assets indexed to 3M LIBOR.
   \item "UVRSwaps" for cross-currency swaps indexed to UVR-IBR rate.
   \item "LIBORSwaps" for Interest Rate Swaps (IRS) indexed to 3M LIBOR.
     }

\code{daycount} convention accepts the following values:
\itemize{
    \item 30/360.
    \item ACT/365.
    \item ACT/360 (Default).
    \item ACT/365L.
    \item NL/365.
    \item ACT/ACT-ISDA
    \item ACT/ACT-AFB
}
}
\examples{
accrued.interests(coupon.rate = 0.04, maturity = '2029-08-10',
                  asset.type = 'LIBOR', daycount = "30/360")
accrued.interests(coupon.rate = 0.04, maturity = '2029-08-10',
                  daycount = "NL/365")
accrued.interests(coupon.rate = 0.04, maturity = '2029-08-10',
                  asset.type= 'IBR', daycount = "ACT/360")
accrued.interests(coupon.rate = 0.04, maturity = '2029-08-10', freq= 2,
                  asset.type= 'FixedIncome', daycount = "ACT/365")


}
