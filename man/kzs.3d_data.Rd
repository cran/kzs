\name{kzs.3d_data}
\alias{kzs.3d_data}
\docType{data}
\title{4-dimensional KZS Output}
\description{
  This data set contains the output from a KZS operation consisting of 3
  input variables, X = (X1, X2, X3), and the single outcome variable Y,
  which is buried in noise. See the \sQuote{Details} for more information.
}
\usage{data(kzs.3d_data)}
\format{
  A data frame with 9025 observations on 52 variables. The first two variables are the coordinates of a
  two-dimensional grid (X1, X2). The remaining 50 variables are KZS output.  See more details below.
}
\details{
  This data set is based on the example documented in \code{kzs.2d}. A 2D grid of points, (X1, X2), is 
  constructed over the range [-1.5pi, 1.5pi] and acts as two input variables. The third input variable 
  is time, and has values, 1, 2, ..., 50. For each of the 50 time points, there is a corresponding amplitude,
  that ranges from 0 to 1. For each unique amplitude, the outcome variable, Y, is calculated by applying the 
  Sinc function to the \code{(x1, x2)} grid over each of the 50 time points. This process results in 50 columns
  of data, 1 for each time point and amplitude. As stated in the \code{kzs.md} documentation, KZS is a linear 
  operation, and thus is commutative (that is, we can change the order of the operations, without changing the 
  end result). For example, \code{kzs.3d} = \code{kzs.1d} + \code{kzs.2d} = \code{kzs.2d} + \code{kzs.1d}.  This 
  property of KZS was used (instead of \code{kzs.md}) to receive the 4-dimensional result. Using the data described 
  above, \code{kzs.2d} was first applied to each of the 50 columns of input using \code{k = 2} iterations, 
  \code{delta} = (1.5, 1.5) and \code{d} = (0.1, 0.1), which were chosen arbitrarily. Using the resulting data from
  the \code{kzs.2d} operation, \code{kzs.1d} was applied across time. The result of this operation is 4-dimensional 
  data, which can be visualized as a 2-dimensional map with color (blue for low amplitudes near 0 and red for high
  amplitudes close to 1).  Incorporating time, this result can be visualized as a \dQuote{movie} of the 50 2-dimensional
  images, where the amplitude (color) is changing from 0 to 1 (blue to red).\cr

  This data set has been included in this package due to the significant amount of computer time it took to run. Including
  this process as an example would not be efficient. Using this data set, the example in the \code{kzs.md} documentation
  is constructed.  
}
\keyword{datasets}
