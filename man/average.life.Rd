% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bondValuation.R
\name{average.life}
\alias{average.life}
\title{Weighted Average Life}
\usage{
average.life(
  input,
  price,
  maturity,
  analysis.date = Sys.Date(),
  coupon.rate,
  principal = 1,
  asset.type = "TES",
  freq = 1,
  rate.type = 1,
  spread = 0,
  daycount = "ACT/365",
  dirty = 1,
  convention = "F",
  trade.date = NULL,
  coupon.schedule = "SF"
)
}
\arguments{
\item{input}{String that establishes if the price input corresponds to the
Internal Rate of Return (IRR) of the bond or the market price. Set
\code{"rate"} for the IRR. Otherwise, \code{"price"}.}

\item{price}{Numeric value of either market price or Internal Rate of Return (IRR) of a
given bond. Instead of IRR, can also be a rates vector that corresponds to coupon dates.}

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

\item{rate.type}{(1) for discrete compounded discount rates and (0) for continuosly
compounded discount rates. By default rates are assumed to be discrete.}

\item{spread}{Decimal value of spread added to coupon payment rate. By
default, \code{0}.}

\item{daycount}{Day count convention. See also 'Details'.}

\item{dirty}{Numeric value to determine if the calculated price is dirty or
clean. To calculate dirty price, set \code{dirty = 1}. Otherwise,
\code{dirty = 0}.}

\item{convention}{String that establishes if the effective dates are
calculated using Following, Modified Following, Backward or Backward Following.
See also 'Details'.}

\item{trade.date}{The date on which the transaction occurs. It is used to calculate
maturity as a date, when given in years. Also required for non-trivial cases such as
bonds with long first coupon.}

\item{coupon.schedule}{String that establishes if a bond first coupon period is a long
first coupon or a short first coupon. On the contrary, establishes if last coupon period
is long last coupon or a short last coupon. See also 'Details'.}
}
\value{
Weighted average life of given bond
}
\description{
Calculates the weighted average life of a given bond by dividing the weighted
total payments by the total payments.
}
\details{
\code{asset.type} makes reference to the following type of assets:
\itemize{
   \item "TES" for Colombian Treasury Bonds (default).
   \item "FixedIncome" for assets that are indexed to a fixed income with
   different frequency of payments.
   \item "IBR" for bonds and assets indexed to 3M IBR rate.
   \item "LIBOR" for bonds and assets indexed to 3M LIBOR.
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

\code{convention} makes reference to the following type of business day conventions:
\itemize{
   \item "F" for Following business day convention.
   \item "MF" for Modified Following business day convention.
   \item "B" for Backward business day convention.
   \item "MB" for Modified Backward business day convention.
     }

\code{coupon.schedule} makes reference to the following type of coupon payment schedule
of a bond:
\itemize{
   \item "LF" for Long First coupon payment.
   \item "LL" for Long Last coupon payment.
   \item "SF" for Short First coupon payment.
   \item "SL" for Short Last coupon payment.
     }
}
\examples{
average.life(input = c("rate"), price = 0.08, maturity = "2026-06-01",
             analysis.date = "2025-06-01", coupon.rate = 0.06, principal = 1000,
             asset.type = "IBR", freq = 4)
average.life(input = c("rate"), price = c(0.043,0.05), maturity = "2023-01-03",
             analysis.date = "2021-01-03", coupon.rate = 0.04, principal = 1,
             asset.type = "FixedIncome", freq = 1, rate.type = 0)

}
