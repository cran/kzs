\name{kzs.2d}
\alias{kzs.2d}
\title{ Spatial Kolmogorov-Zurbenko Spline }
\description{
  The \code{kzs.2d} function is a spatial extension of the \code{kzs} function for two input variables.
}
\usage{
kzs.2d(y, x, smooth, scale, k = 1, edges = TRUE, plot = TRUE)
}
\arguments{
  \item{y}{
    a one-dimensional vector of real values representing the response variable to be smoothed.
  }
  \item{x}{ 
    a two-dimensional matrix of real values containing the input variables X = (X1, X2). Each column
    represents an input variable.
  }
  \item{smooth}{ 
    a vector of size two that defines the width of the smoothing window along each input variable.
  }
  \item{scale}{ 
    a vector of size two in which each element will define a uniformly spaced scale along its respective
    input variable.
  }
  \item{k}{ 
    an integer specifying the number of iterations \code{kzs.2d} will execute. By default, \code{k = 1}.
  }
  \item{edges}{ 
    a logical indicating whether or not to display the outcome data beyond the rectangular range of the two
    input variables. By default, \code{edges = TRUE}.
  }
  \item{plot}{ 
    a logical indicating whether or not to produce a 3-dimensional plot of the \code{kzs.2d} outcome. By 
    default, this argument is set to \code{TRUE}.
  }
}
\details{
  The details for this function are nearly identical to that of \code{\link{kzs}}, except now extended to
  three dimensional space. The only difference is that the \code{kzs.2d} function averages all \code{y} that
  are contained within a rectangular window made up of sides \code{smooth[1]} and \code{smooth[2]}.   
}
\value{
  a three column data frame of the form \code{(x1, x2, yk)}:
  \item{x1 }{
    the \code{x1} coordinates of a two-dimensional grid.
  }
  \item{x2 }{
    the \code{x2} coordinates of a two-dimensional grid.
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
  function, \code{kzs.2d}, simply means that there are two input variables required for use.\cr

  The graphical output of \code{kzs.2d} is a result of the \code{wireframe()} function within the \pkg{lattice} package.
}
\seealso{ \code{\link{kzs}}; For more on the parameter restrictions, see \code{\link{kzs.params}} }
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
sw <- c(1, 1)
sc <- c(0.2, 0.2)
kzs.2d(y = df[,5], x = df[,1:2], smooth = sw, scale = sc, k = 1, edges = TRUE, 
plot = TRUE)  
}
\keyword{ smooth }
\keyword{ ts }
\keyword{ nonparametric }