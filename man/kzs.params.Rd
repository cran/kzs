\name{kzs.params}
\alias{kzs.params}
\title{ Boundary Values for KZS Parameters }
\description{
  For an \emph{n}-dimensional input data set, this function will calculate the values by
  which the parameters \code{delta} and \code{d} are bounded by. 
}
\usage{
kzs.params(x, dimension)
}
\arguments{
  \item{x}{ 
    a matrix or data frame containing the input data that is to be used in \code{kzs}, \code{kzs.2d}, 
    or \code{kzs.md}.
  }
  \item{dimension}{ 
    an integer specifying the dimensionality of \code{x}.
  }
}
\details{
  The compilation of functions within the \pkg{kzs} package requires the specification of two 
  parameters: the first is \code{delta}, the physical range of smoothing along each variable 
  in \code{x}; the second is \code{d}, a scale reading of each corresponding input variable
  in \code{x}. Each parameter is subject to two restrictions; \code{delta} and \code{d} must both
  be positive real numbers, each \code{d} must be less than or equal to the difference of sorted, 
  consecutive \code{x} values and each \code{delta} must be much less than the difference of the
  maximum and minimum values for each corresponding input variable in \code{x}. Hence, for each 
  input variable in \code{x}, there must be a corresponding \code{delta} and \code{d}. This function
  was developed to be used prior to any of the functions within \pkg{kzs} in order to increase the 
  efficiency of use.
}
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} }
\examples{
# Generate 3 random sequences of numbers that would act as the input data set 
x1 <- rnorm(100, 3, 6)
x2 <- rnorm(100, 4, 5)
x3 <- runif(100, 0, 1)

# A matrix or a data frame will work 
mat <- matrix(c(x1, x2, x3), nrow = 100, ncol = 3)

# Dimensionality is 3 since there are 3 input variables
kzs.params(x = mat, dimension = 3)
}
\keyword{ smooth }
\keyword{ nonparametric }