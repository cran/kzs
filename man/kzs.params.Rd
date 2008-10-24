\name{kzs.params}
\alias{kzs.params}
\title{ Restrictions for KZS Parameters }
\description{
  For a \emph{d}-dimensional vector of input variables, this function will calculate the values by
  which the parameters \code{smooth} and \code{scale} are bounded by. 
}
\usage{
kzs.params(x, dimension)
}
\arguments{
  \item{x}{ 
    a matrix or data frame containing the input variable(s) that is to be used in \code{kzs}, \code{kzs.2d}, 
    or \code{kzs.md}.
  }
  \item{dimension}{ 
    an integer specifying the dimensionality of \code{x}; i.e, the number of columns in \code{x}.
  }
}
\details{
  The compilation of functions within the \pkg{kzs} package requires the specification of two 
  parameters: the first is \code{smooth}, the range of smoothing along each variable in \code{x}; 
  the second is \code{scale}, a scale reading of each corresponding input variable in \code{x}. 
  Each parameter is subject to two restrictions; \code{smooth[i]} and \code{scale[i]} must be 
  positive real numbers; \code{scale[i]} must be less than or equal to the difference of sorted, 
  consecutive \code{x[,i]} values and \code{smooth[i]} must be much less than the difference of the
  maximum and minimum values for its corresponding input variable, \code{x[,i]}. For each input 
  variable in \code{x}, there must be a corresponding \code{smooth} and \code{scale}. This function
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