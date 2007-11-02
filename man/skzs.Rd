\name{skzs}
\alias{skzs}
\title{ Spatial Kolmogorov-Zurbenko Spline }
\description{
  SKZS utilizes splines to construct a smooth estimate of a single outcome variable over 
  two dimensional input variables.
}
\usage{
skzs(data, y, x1, x2, delta1, delta2, h1, h2, k = 1, show.edges = FALSE, plot = TRUE)
}
\arguments{
  \item{data}{ 
    a data frame of 3-dimensional points (X1, X2, Y) where Y is a one dimensional response 
    variable and X = (X1, X2) are two-dimensional real values in the space R2.  The data 
    frame must contain a minimum of 3 columns (2 input variables and 1 response variable) 
    and can contain up to \emph{n} columns in which any two can be the input variables and 
    any column can be the response.
  }
  \item{y}{
    an integer specifying the column in the data frame containing the Y values; the values 
    to be used as the response variable.
  }
  \item{x1}{ 
    an integer specifying the column in the data frame containing the X1 values; the first 
   input variable.
  }
  \item{x2}{ 
    an integer specifying the column in the data frame containing the X2 values; the second 
    input variable.
  }
  \item{delta1}{ 
    is the physical range of smoothing in terms of unit values of the input variable \code{x1}.  
  }
  \item{delta2}{ 
    is the physical range of smoothing in terms of unit values of the input variable \code{x2}. 
  }
  \item{h1}{ 
    a scale reading of all outcomes of the algorithm. More specifically, \code{h1} is the interval 
    width of a uniform scale overlapping the \code{x1} input variable.  
  }
  \item{h2}{ 
    a scale reading of all outcomes of the algorithm. More specifically, \code{h2} is the interval 
    width of a uniform scale overlapping the \code{x2} input variable.  
  }
  \item{k}{ 
    the number of iterations SKZS will execute; \code{k} may also be interpreted as the order of 
    smoothness (as a polynomial of degree \code{k-1}). By default, \code{k} is set to perform a 
    single iteration.
  }
  \item{show.edges}{ 
    a logical indicating whether or not to display the resulting data beyond the range of \code{x1} 
    and \code{x2} values of the input data frame.  If \code{false}, then the extended edges are 
    suppressed. By default, this argument is set to \code{false}. 
  }
  \item{plot}{ 
    a logical indicating whether or not to return a 3-dimensional plot of the SKZS outcome.  By 
    default, this argument is set to \code{true}.
  }
}
\details{
  The relation between variables Y and X = (x1, x2) as a function of a current value of X is often 
  desired as a result of practical research.  Usually we search for some simple function Y(x1, x2) 
  when given a data set of 3-dimensional points (Y, x1, x2).  When plotted, these points frequently 
  resemble a noisy plot, and thus Y(x1, x2) is desired to be a smooth outcome from the original data 
  to capture important patterns in the data, while leaving out the noise.  SKZS estimates a solution to 
  this problem through use of splines, a particular nonparametric estimator of a function.  Given a data 
  set of 3-dimensional points, splines will estimate the smooth values of the response Y from the two 
  dimensional input variables \code{x1} and \code{x2}.  SKZS averages all values of Y contained in a 
  rectangle made up of sides \code{delta1} and \code{delta2} and centered at the point (xk1, xk2), a 
  particular point on the uniform scale overlaying the \code{x1} and \code{x2} axes.  The SKZS algorithm 
  is designed to smooth all fast fluctuations in Y within the \code{delta}-range (\code{x1}, \code{x2}), 
  while keeping ranges more then \code{delta1} and \code{delta2} untouched. The separation of short scales 
  less than \code{delta1, delta2} and long scales more than \code{delta1, delta2} is becoming more effective 
  with higher \code{k}, while effective range of separation is becoming \eqn{\code{delta(j)}*sqrt(\code{k})}. 
}
\value{
  a three column data frame containing:
  \item{x1 }{
    the \code{x1} coordinates of a grid determined by a uniform scale (defined by \code{h1}) overlaying the 
    input \code{x1} variable.
  }
  \item{x2 }{
    the \code{x2} coordinates of a grid determined by a uniform scale (defined by \code{h2}) overlaying the 
   input \code{x2} variable.
  }
  \item{zk }{
    the estimated values of the one dimensional response variable, Y(\code{x1}, \code{x2}) after \code{k} 
    iterations. 
  }
}
\references{ "Spline Smoothing." \url{http://economics.about.com/od/economicsglossary/g/splines.htm}}
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} }
\note{ 
  SKZS is designed for the general situation, including time series data. In many applications where the 
  variable Xj can be time, SKZS is resolving the problem of missing values in time series or irregularly 
  observed values in longitudinal data analysis.\cr

  Certain arguments in SKZS have restrictions on what their value may be.  These restrictions are 
  (for more details, see \code{\link{argskzs}}):\cr
  \eqn{\code{delta1} << max(\code{x1}) - min(\code{x1})}\cr
  \eqn{\code{delta2} << max(\code{x2}) - min(\code{x2})}\cr
  \eqn{\code{h1} > 0} and \eqn{\code{h1} < min{\code{x1(i+1)} - \code{x1(i)}}} where i = 1,..., n\cr
  \eqn{\code{h2} > 0} and \eqn{\code{h2} < min{\code{x2(i+1)} - \code{x2(i)}}} where i = 1,..., n\cr
	
  The graphical output of SKZS is a result of the \code{wireframe()} function within the \emph{lattice} package.
}
\seealso{ \code{\link{argskzs}}, \code{\link{kzs}} }
\examples{
  # EXAMPLE - Estimating the Sinc function in the interval (-3pi, 3pi)
  #           Load the LATTICE package 
  require(lattice)
  ### (1) Create a 3D plot of the signal to be estimated by SKZS
  
  # Create a random sample of size 250 for X = (X1, X2)
  u <- seq(-3*pi, 3*pi, 3*pi/50)
  v <- u			
  x1 <- sample(u, size = 250, replace=TRUE)  
  x2 <- sample(v, size = 250, replace=TRUE)
  
  # Store x1 and x2 into a data frame
  d <- data.frame(cbind(x1,x2))

  # Keep only the unique (x1,x2) data points
  df <- unique(d) 

  # Create the lattice of points
  df <- expand.grid(x1 = x1, x2 = x2)
  
  # Apply the Sinc function to x1 and x2 and store the result as z
  df$z <- sin(sqrt(df$x1^2 + df$x2^2)) / sqrt(df$x1^2 + df$x2^2)
  df$z[is.na(df$z)] <- 1
  
  # Any point outside the circle of radius 3pi is set to 0. This 
  # provides a better picture of the outcome solely for the purposes
  # of this example.
  dst <- sqrt((df$x1 - 0)^2 + (df$x2 - 0)^2)
  df$dist <- dst	
  df$z[df$dist > 3*pi] <- 0

  # 3D plot of signal to be estimated
  wireframe(z ~ x1 * x2, df, main = "Signal to be estimated", drape = TRUE, 
  colorkey = TRUE, scales = list(arrows = FALSE))
  
  ### (2) Create a 3D plot of the signal buried in noise
  ez <- rnorm(length(df$z), mean = 0, sd = 1) * 1/2 
  df$z_noisy <- ez + df$z
  wireframe(z_noisy ~ x1 * x2, df, main = "Signal buried in noise", drape = TRUE, 
  colorkey = TRUE, scales = list(arrows = FALSE))
  
  ### (3) Create the data set to be used in SKZS --- n = 4000
  #	  same process as in (1)
  x1 <- sample(u, size = 4000, replace = TRUE)
  x2 <- sample(v, size = 4000, replace = TRUE)
  d <- data.frame(cbind(x1,x2))
  df <- unique(d)
  df$z <- sin(sqrt(df$x1^2 + df$x2^2)) / sqrt(df$x1^2 + df$x2^2)
  df$z[is.na(df$z)] <- 1
  
  dst <- sqrt((df$x1 - 0)^2 + (df$x2 - 0)^2)
  df$dist <- dst
  df$z[df$dist > 3*pi] <- 0
  
  ez <- rnorm(length(df$z),mean=0,sd=1)*1/2
  df$z_noisy <- ez + df$z
  dfn <- df[,-3:-4]  
  
  ### (4) Create a 2D view of the 3D plots above
  par(mfrow = c(2,1))
  plot(df$z ~ df$x1, main = "2D plot of the signal to be estimated\nn = 4000", 
  xlab = "x", ylab = "Z(x)")
  plot(df$z_noisy ~ df$x1, main = "2D plot of the signal buried in noise\nn = 4000", 
  xlab = "x", ylab = "Z(x)")
  
  ### (5) Execute SKZS on the data...arguments were chosen arbitrarily.  
  #	  Try other argument values to test the outcome
  skzs(data=dfn, y=3, x1=1, x2=2, delta1=3, delta2=3, h1=3*pi/60, h2=3*pi/60, k=1, 
  show.edges=FALSE, plot=TRUE)
}
\keyword{ smooth }
\keyword{ nonparametric }