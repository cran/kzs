\name{argskzs}
\alias{argskzs}
\title{ Argument Limits for SKZS }
\description{
  This function calculates the values for which the arguments \code{delta1}, 
  \code{delta2} and \code{h1}, \code{h2} in SKZS are bounded above or below by.
}
\usage{
argskzs(data, x1, x2)
}
\arguments{
  \item{data}{ 
    a data frame to be used with SKZS.  Only the columns corresponding the input variables 
    X = (\code{x1}, \code{x2}) are needed; the column corresponding to the response variable is 
    optional, but plays no part in the use of this function.
  }
  \item{x1}{ 
    an integer specifying the position of the column in the data frame containing \code{x1} values.
  }
  \item{x2}{ 
    an integer specifying the position of the column in the data frame containing \code{x2} values.
  }
}
\details{
  In the SKZS function (similarly to the \code{\link{kzs}} function), the arguments \code{delta1} and 
  \code{delta2} are the physical ranges of smoothing in terms of the unit values of the input variables 
  \code{x1} and \code{x2}; the arguments \code{h1} and \code{h2} are scale readings of all outcomes of 
  the algorithm; more specifically, \code{h1} and \code{h2} are values denoting the interval widths of 
  two uniform scales overlapping the \code{x1} and \code{x2} axes.  The restrictions on the arguments 
  are the same as for the one dimensional input variable in KZS, only here, the restrictions are extended 
  to the two-dimensional input variables \code{x1} and \code{x2}.  The purpose of this function is to give 
  an upper bound on the values \code{delta1, delta2} and \code{h1, h2} so that users may select appropriate 
  values that satisfy all restrictions.  This function eliminates any guess-work involved in choosing a 
  satisfying value for the arguments and should be used prior to using SKZS in order to save time and 
  increase efficiency of use. 
}
\value{
  a list containing the following:
  \item{delta1 }{the bounding value for the argument \code{delta1}}
  \item{delta2 }{the bounding value for the argument \code{delta2}}
  \item{h1 }{the bounding value for the argument \code{h1}}
  \item{h2 }{the bounding value for the argument \code{h2}}
}
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} }
\seealso{ \code{\link{skzs}} }
\examples{
### Recall the SKZS example of the Sinc function

# Setup the data
u <- seq(-3*pi, 3*pi, 3*pi/100)
v <- u
x1 <- sample(u, size = 4000, replace = TRUE)
x2 <- sample(v, size = 4000, replace = TRUE)
d <- data.frame(cbind(x1,x2))
df <- unique(d)
df$z <- sin(sqrt(df$x1^2 + df$x2^2)) / sqrt(df$x1^2 + df$x2^2)
df$z[is.na(df$z)] <- 1

# Return the bounding values for each argument
argskzs(df, 1, 2)
}
\keyword{ smooth }
\keyword{ nonparametric }