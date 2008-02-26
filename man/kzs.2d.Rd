\name{kzs.2d}
\alias{kzs.2d}
\title{ Spatial Kolmogorov-Zurbenko Spline }
\description{
  The \code{kzs.2d} function is an extension of the \code{kzs} function, in which splines are utilized 
  to smooth a three-dimensional spatial data set consisting of two-dimensional input variables X = (X1, X2)
  and the single outcome variable, Y, which is contaminated with noise.
}
\usage{
kzs.2d(y, x, delta, d, k = 1, edges = FALSE, plot = TRUE)
}
\arguments{
  \item{y}{
    a 1-dimensional vector of real values representing the response variable that is to be smoothed.
  }
  \item{x}{ 
    a two-dimensional (or, \emph{n} x 2) matrix of real values containing the input variables X = (X1, X2). These
    vectors, along with \code{y}, must be of equal length. 
  }
  \item{delta}{ 
    a real-valued vector of size two in which each element of \code{delta} defines the physical range of 
    smoothing for each corresponding variable in \code{x}, in terms of unit values of each \code{x}. This algorithm
    is designed to smooth the \code{y} that lies within each rectangular range made up by \code{delta[1]} and 
    \code{delta[2]}, while leaving data beyond this range untouched.
  }
  \item{d}{ 
    a real-valued vector of size two in which each element of \code{d} represents a scale reading of each
    corresponding input variable in \code{x}. Each \code{d} defines a uniform scale along each respective variable
    in \code{x} for which each rectangular \code{delta}-range is based on.
  }
  \item{k}{ 
    an integer specifying the number of iterations \code{kzs.2d} will execute. By default, \code{k = 1}.
  }
  \item{edges}{ 
    a logical indicating whether or not to display the outcome data beyond the rectangular range of the two
    input variables in \code{x}. By default, \code{edges = FALSE}. Further details on this argument will be
    documented.
  }
  \item{plot}{ 
    a logical indicating whether or not to produce a 3-dimensional plot of the \code{kzs.2d} outcome. By 
    default, this argument is set to \code{TRUE}.
  }
}
\details{
  The relation between variables Y and X = (X1, X2) as a function of a current value of X [namely, Y(X1, X2)],
  is often desired as a result of practical research.  Usually we search for some simple function Y(X1, X2) 
  when given a data set of 3-dimensional points (Y, x1, x2). When plotted, these points frequently resemble a 
  noisy plot, and thus Y(X1, X2) is desired to be a smooth outcome from the original data, capturing important 
  patterns in the data, while leaving out the noise. The \code{kzs.2d} function estimates a solution to this 
  problem through use of splines, a particular nonparametric estimator of a function. Given a data set of 
  3-dimensional points, splines will estimate the smooth values of the response variable Y from the two 
  dimensional input variables within \code{x}. \code{kzs.2d} averages all values of Y contained in a rectangle 
  made up of sides \code{delta[1]} and \code{delta[2]}, which is centered at (xk1, xk2), a point on the lattice
  of uniform scales that is defined by \code{d[1]} and \code{d[2]}, which are overlaying the x1 and x2 axes. The 
  \code{kzs.2d} algorithm is designed to smooth all fast fluctuations in Y within the rectangular \code{delta}-range
  of X1 and X2, while keeping ranges more then \code{delta[1]} and \code{delta[2]} untouched. 
}
\value{
  a three column data frame of the form \code{(x1, x2, yk)}:
  \item{x1 }{
    the \code{x1} coordinates of a two-dimensional grid \code{(x1, x2)} in increments of \code{h[1]}.
  }
  \item{x2 }{
    the \code{x2} coordinates of a two-dimensional grid \code{(x1, x2)} in increments of \code{h[2]}.
  }
  \item{yk }{
    the smoothed response values resulting from \code{k} iterations of \code{kzs.2d}.
  }
}
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} }
\note{ 
  Data set (Y, X1, X2) must be provided, usually as 3-dimensional observations that occur in time or
  space; \code{kzs.2d} is designed for the general situation, including time series data. In many applications 
  where an input variable can be time, \code{kzs.2d} can resolve the problem of missing values in time series or
  or irregularly observed values in Geographical Information Systems (GIS) data analysis. The name of this 
  function, \code{kzs.2d}, simply means that there are two input variables required for use. For each of the two
  input variables in \code{x}, there must be a corresponding \code{delta} and \code{d}.\cr

  The graphical output of \code{kzs.2d} is a result of the \code{wireframe()} function within the \pkg{lattice} package.
}
\seealso{ For more on the parameter restrictions, see \code{\link{kzs.params}} }
\examples{
# EXAMPLE - Estimating the Sinc function in the interval (-3pi, 3pi)
#           Load the LATTICE package 


# Gridded data for X = (x1, x2) input variables
x1 <- seq(-3*pi, 3*pi, length = 60)
x2 <- x1			
df <- expand.grid(x1 = x1, x2 = x2)
  
# Apply the Sinc function to the (x1, x2) coordinates
df$z <- sin(sqrt(df$x1^2 + df$x2^2)) / sqrt(df$x1^2 + df$x2^2)
df$z[is.na(df$z)] <- 1

# Any point outside the circle of radius 3pi is set to 0. This provides
# a better picture of the outcome solely for the purposes of this example.
dst <- sqrt((df$x1 - 0)^2 + (df$x2 - 0)^2)
df$dist <- dst	
df$z[df$dist > 3*pi] <- 0

# Add noise to distort the signal
ez <- rnorm(length(df$z), mean = 0, sd = 1) * 1/4    
df$zn <- ez + df$z

### (1) 3D plot of the signal to be estimated by kzs.2d()
wireframe(z ~ x1 * x2, df, main = "Signal to be estimated", drape = TRUE, 
colorkey = TRUE, scales = list(arrows = FALSE))

### (2) 3D plot of the signal buried in noise
wireframe(zn ~ x1 * x2, df, main = "Signal buried in noise", drape = TRUE, 
colorkey = TRUE, scales = list(arrows = FALSE))
  
### (3) Execute kzs.2d()   
# kzs.2d() may take time to run; k = 1 iteration is used here, but k = 2
# will provide a smoother outcome.
dt <- c(1, 1)
ht <- c(0.2, 0.2)
kzs.2d(y = df[,5], x = df[,1:2], delta = dt, d = ht, k = 1, edges = FALSE, 
plot = TRUE)  
}
\keyword{ smooth }
\keyword{ ts }
\keyword{ nonparametric }