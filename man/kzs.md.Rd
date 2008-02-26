\name{kzs.md}
\alias{kzs.md}
\title{ Spatial Kolmogorov-Zurbenko Spline }
\description{
  The \code{kzs.md} function is another extension of the \code{kzs} function, in which splines are utilized 
  to smooth a multi-dimensional spatial data set consisting of \emph{m}-dimensional input variables 
  X = (X1, X2, ..., Xm) and the single outcome variable, Y, which is contaminated with noise.
}
\usage{
kzs.md(y, x, delta, d, k = 1, edges = FALSE)
}
\arguments{
  \item{y}{
    a 1-dimensional vector of real values representing the response variable that is to be smoothed.
  }
  \item{x}{ 
    a \emph{m}-dimensional (or, \emph{n} x \emph{m}) matrix of real values containing the input variables 
    X = (X1, X2, ..., Xm).
  }
  \item{delta}{ 
    a real-valued vector of size \emph{m} in which each element of \code{delta} defines the physical range of 
    smoothing for each corresponding variable in \code{x}, in terms of unit values of each \code{x}. 
  }
  \item{d}{ 
    a real-valued vector of size \emph{m} in which each element of \code{d} represents a scale reading of each
    corresponding input variable in \code{x}.
  }
  \item{k}{ 
    an integer specifying the number of iterations \code{kzs.md} will execute. By default, \code{k = 1}.
  }
  \item{edges}{ 
    a logical indicating whether or not to display the outcome data beyond the ranges of the \emph{m}
    input variables in \code{x}. By default, \code{edges = FALSE}.
  }
}
\details{
  The relation between variables Y and X = (X1, X2, ..., Xm) as a function of a current value of X 
  [namely, Y(X1, X2, ..., Xm)], is often desired as a result of practical research.  Usually we search for some 
  simple function Y(X1, X2, ..., Xm) when given a data set of (\emph{m}+1)-dimensional points (Y, X1, X2, ..., Xm). 
  When plotted (if possible), these points frequently resemble a noisy plot, and thus Y(X1, X2, ..., Xm) is desired 
  to be a smooth outcome from the original data, capturing important patterns in the data, while leaving out the 
  noise. The \code{kzs.md} function estimates a solution to this problem through use of splines, a particular 
  nonparametric estimator of a function. Given a data set of (\emph{m}+1)-dimensional points, splines will estimate 
  the smooth values of the response variable Y from the \emph{m} input variables within \code{x}. 
}
\value{
  a (\emph{m}+1)-column data frame of the form \code{(x1, x2, ..., yk)}. See \code{\link{kzs.2d}} for the general
  interpretations of these results.
}
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} }
\note{ 
  \code{kzs.md} is simply an extension of \code{kzs.2d}, now allowing for \emph{m} input variables.\cr

  Data set (Y, X1, X2, ..., Xm) must be provided, usually as observations that occur in time or space; \code{kzs.md} 
  is designed for the general situation, including time series data. In many applications where input variables can 
  be space, \code{kzs.md} can resolve the problem of missing values in time series or or irregularly observed values
  in Geographical Information Systems (GIS) data analysis. Additionally, \code{kzs.md} can be recommended as a 
  diagnostic tool before applying multiple linear regression analysis due to its capability of displaying nonlinearities
  of the outcome over the input variables.\cr

  As in \code{kzs.2d}, for each of the \emph{m} input variables, there must be a corresponding \code{delta} and 
  \code{d}.\cr

  There is no graphical output for this function; for two input variables, \code{kzs.2d} will produce a 3-dimensional
  plot. For three input variables, a 4-dimensional movie can be constructed over time.\cr

  In general, \code{kzs}, \code{kzs.2d} and \code{kzs.md} are all linear operations, and linear operations are
  commutative. Thus, for example, the outcome of a \code{kzs.2d} operation is equivalent to \code{kzs.1d} + \code{kzs.1d}; 
  likewise, the outcome of a \code{kzs.3d} operation is equivalent to a \code{kzs.2d} + \code{kzs.1d}, etc...  
}
\seealso{ For more on the parameter restrictions, see \code{\link{kzs.params}} }
\examples{
# This example is an extension of the example documented in kzs.2d. We make 
# use of the Sinc function to filter a signal buried in noise over 3-dimensional 
# input variables. See the "Details" section of the "kzs.3d_data" data frame 
# documentation for specific details. 
require(lattice)

# Gridded data for X = (X1, X2) input variables
x1 <- seq(-1.5*pi, 1.5*pi, length = 50)
x2 <- x1	
df <- expand.grid(x1 = x1, x2 = x2)
	
# Time dimension
time <- 1:50

# Change the amplitude of the original function to change from 0 to 1 along time
amplitude <- sort(round(seq(0.02, 1, 0.02), digits = 2))

# Store the time and amplitude together in a data frame
t_amp <- data.frame(cbind(time, amplitude))

# Create the data set of Sinc function outcomes for each amplitude
sinc <- array(0, dim = c(nrow(df), length(amplitude)))
for (i in 1:length(amplitude)) {
	sinc[,i] <- round(amplitude[i]*sin(sqrt(df$x1^2 + df$x2^2)) / 
		    	sqrt(df$x1^2 + df$x2^2)) 
	sinc[,i][is.na(sinc[,i])] <- amplitude[i]
}

# Add noise to distort the signal 
for (j in 1:ncol(sinc)) {
	ez <- rnorm(nrow(sinc), mean = 0, sd = 1)
	sinc[,j] <- sinc[,j] + ez
}

# Change to a data frame and add the gridded input data 
kzs.2d_data <- as.data.frame(cbind(df, sinc)) 

### Movie of the signal buried in noise
grayscale = colorRampPalette(c("white", "gray", "black"))
for (u in 1:50) {
	plot(levelplot(kzs.2d_data[,u+2] ~ x1*x2, kzs.2d_data, 
		col.regions = grayscale, colorkey = FALSE))
}

### Movie of KZS 4-dimensional KZS outcome
data(kzs.3d_data)
bluered = colorRampPalette(c("blue", "cyan2", "green", 
			     "yellow", "red", "firebrick"), space = "rgb")
for (j in 1:50) {
	plot(levelplot(kzs.3d_data[,j+2] ~ x1*x2, kzs.3d_data, 
           at = do.breaks(c(-0.3, 1.0), 100), col.regions = bluered))
} 
}
\keyword{ smooth }
\keyword{ ts }
\keyword{ nonparametric }