\name{kzs}
\alias{kzs}
\title{ Kolmogorov-Zurbenko Spline }
\description{
  This is a one-dimensional iterative smoothing algorithm based on convolutions of rectangular kernels.
}
\usage{
kzs(y, x, smooth, scale, k = 1, edges = TRUE, plot = TRUE)
}
\arguments{
  \item{y}{  
    a one-dimensional vector of real values representing the response variable to be smoothed.
  }
  \item{x}{ 
    a one-dimensional vector of real values representing the input variable.
  }
  \item{smooth}{ 
    a real number defining the width of the smoothing window, i.e., the width of the rectangular kernel.   
  }
  \item{scale}{ 
    for an irregularly spaced \code{x}, \code{scale} is a positive real number that will define a uniform
    scale along \code{x}.
  }
  \item{k}{ 
    an integer specifying the number of iterations \code{kzs} will execute; \code{k} may also be 
    interpreted as the order of smoothness (as a polynomial of degree \code{k-1}). By default, \code{k = 1}.
  }
  \item{edges}{ 
    a logical indicating whether or not to display the outcome data beyond the initial range of \code{x}. By 
    default, \code{edges = TRUE}. 
  }
  \item{plot}{  
    a logical indicating whether or not to produce a plot of the \code{kzs} outcome. This is \code{TRUE}
    by default.
  }
}
\details{
  The relation between variables Y and X as a function of a current value of X = x [namely, Y(x)] is 
  often desired as a result of practical research. Usually we search for some simple function, Y(x), 
  when given a data set of pairs (Xi, Yi). When plotted, these pairs frequently resemble a noisy plot, 
  and thus Y(x) is desired to be a smooth outcome that captures patterns or long-term trends in the 
  original data, while suppressing the noise. The \code{kzs} function is based on convolutions of the
  rectangular kernel, which is equilvalent to repeated applications of a moving average. According to 
  the Central Limit Theorem, repeated convolutions with rectangular kernels will converge to the Gaussian
  kernel; the resulting kernel will have finite support equal to \code{smooth*k}, which will result in a 
  smooth outcome with diminished noise leakage, which is a feature that the standard Gaussian kernel does
  not exhibit. 
}
\value{
  a two-column data frame of paired values \code{(xk, yk)}:
  \item{xk }{\code{x} values in increments of \code{scale} }
  \item{yk }{smoothed response values resulting from \code{k} iterations of \code{kzs} }
}
\references{ Zurbenko, I.G. (1986). \emph{The Spectral Analysis of Time Series}. North Holland Series in 
 Statistics and Probability, Elsevier Science, Amsterdam. }
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com}}
\seealso{ \code{\link{kzs.params}}, \code{\link{kzs.2d}}, \code{\link{kzs.md}} }
\note{ 
  Data set (Xi, Yi) must be provided, usually as some observations that occur at certain times; \code{kzs}
  is designed for the general situation, including time series data. In many applications where the input 
  variable, \code{x}, can be time, \code{kzs} is resolving the problem of missing values in time series or 
  irregularly observed values in longitudinal data analysis.\cr 

  \code{kzs} may take time to completely run depending on the size of the data set used and the number of 
  iterations specified.\cr

  For more information on the restrictions imposed on \code{delta} and \code{d}, consult \code{\link{kzs.params}}.
}
\examples{

# Total time t
t <- seq(from = -round(400*pi), to = round(400*pi), by = .25) 

# Construct the signal over time
ts <- 0.5*sin(sqrt((2*pi*abs(t))/200))
signal <- ifelse(t < 0, -ts, ts)

# Bury the signal in noise [randomly, from N(0, 1)]
et <- rnorm(length(t), mean = 0, sd = 1)
yt <- et + signal

# Data frame of (t, yt) 
pts <- data.frame(cbind(t, yt))


### EXAMPLE 1 - Apply kzs to the signal buried in noise                 

# Plot of the true signal
plot(signal ~ t, xlab = "t", ylab = "Signal", main = "True Signal",
type = "l")

# Plot of signal + noise
plot(yt ~ t, ylab = "yt", main = "Signal buried in noise", type = "p")

# Apply 3 iterations of kzs
kzs(y = pts[,2], x = pts[,1], smooth = 80, scale = .2, k = 3, edges = TRUE,
plot = TRUE)
lines(signal ~ t, col = "red")
title(main = "kzs(smooth = 80, scale = .2, k = 3, edges = TRUE)")
legend("topright", c("True signal","kzs estimate"), cex = 0.8,
col = c("red", "black"), lty = 1:1, lwd = 2, bty = "n")

### EXAMPLE 2 - Irregularly observed data over time

# Cancel a random 20 percent of (t, yt) leaving irregularly observed time points
obs <- seq(1:length(t))
t20 <- sample(obs, size = length(obs)/5)
pts20 <- pts[-t20,]        

# Plot of (t,yt) with 20 percent of the data removed
plot(pts20$yt ~ pts20$t, main = "Signal buried in noise\n20 percent of 
(t, yt) deleted", xlab = "t", ylab = "yt", type = "p")

# Apply 3 iterations of kzs
kzs(y = pts20[,2], x = pts20[,1], smooth = 80, scale = .2, k = 3, edges = TRUE, 
plot = TRUE)
lines(signal ~ t, col = "red")
title(main = "kzs(smooth = 80, scale = .2, k = 3, edges = TRUE)")
legend("topright", c("True signal","kzs estimate"), cex = 0.8, 
col = c("red", "black"), lty = 1:1, lwd = 2, bty = "n")  
}
\keyword{ smooth }
\keyword{ ts }
\keyword{ nonparametric }