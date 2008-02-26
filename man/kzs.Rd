\name{kzs}
\alias{kzs}
\title{ Kolmogorov-Zurbenko Spline }
\description{
  The \code{kzs} function is designed to smooth a data set of paired values (Xi, Yi), in
  which the response variable, Y, is contaminated with noise.
}
\usage{
kzs(y, x, delta, d, k = 1, edges = FALSE, plot = TRUE)
}
\arguments{
  \item{y}{  
    a 1-dimensional vector of real values representing the response variable that is to be smoothed.
  }
  \item{x}{ 
    a 1-dimensional vector of real values representing the input variable. This vector must be the same length
    as the response vector, \code{y}.
  }
  \item{delta}{ 
    the physical range of smoothing in terms of unit values of \code{x}. The algorithm is designed to smooth Only 
    the points that lie within this range, while leaving points outside of this range untouched.   
  }
  \item{d}{ 
    a positive real number denoting a scale reading along \code{x}. This value defines a uniform scale overlapping
    \code{x} for which each \code{delta}-range is based on.
  }
  \item{k}{ 
    an integer specifying the number of iterations \code{kzs} will execute; \code{k} may also be 
    interpreted as the order of smoothness (as a polynomial of degree \code{k-1}).  By default, \code{k = 1}.
  }
  \item{edges}{ 
    a logical indicating whether or not to display the outcome data beyond the initial range of \code{x}. By 
    default, \code{edges = FALSE}. Further details on this will be documented. 
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
  and thus Y(x) is desired to be a smooth outcome from the original data, capturing significant patterns
  in the data, while leaving out the noise. The \code{kzs} function estimates a solution to this problem 
  through use of splines, a particular nonparametric estimator of a function. Given a data set of pairs 
  (Xi, Yi), splines estimate the smooth values of Y from X's. More specifically, \code{kzs} averages all 
  values of Y for all X within the range \code{delta} around each scale reading \code{di}, along X. The 
  \code{kzs} algorithm is designed to smooth all fast fluctuations in Y within the \code{delta}-range in X, 
  while keeping ranges more then \code{delta} untouched. The separation of short scales less than \code{delta} 
  and long scales more than \code{delta} is becoming more effective with a higher \code{k}, while the effective 
  range of separation is becoming \eqn{delta*sqrt(k)}. 
}
\value{
  a two-column data frame of paired values \code{(xk, yk)}:
  \item{xk }{\code{x} values in increments of \code{d} }
  \item{yk }{smoothed response values resulting from \code{k} iterations of \code{kzs} }
}
\references{ "Spline Smoothing." \url{http://economics.about.com/od/economicsglossary/g/splines.htm} }
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com}}
\seealso{ \code{\link{kzs.2d}}, \code{\link{kzs.md}} }
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
# This example was created with the intent to push the limits of kzs. The 
# function has a wide peak and a sharp peak; for a wide peak, you may permit 
# stronger smoothing and for a sharp peak you may not (you would be over-
# smoothing). Try various values for delta and d to see how the outcome may vary.


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
kzs(y = pts[,2], x = pts[,1], delta = 80, d = .2, k = 3, edges = FALSE,
plot = TRUE)
lines(signal ~ t, col = "red")
title(main = "kzs(delta = 80, d = .2, k = 3, edges = FALSE)")
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
kzs(y = pts20[,2], x = pts20[,1], delta = 80, d = .2, k = 3, edges = FALSE, 
plot = TRUE)
lines(signal ~ t, col = "red")
title(main = "kzs(delta = 80, d = .2, k = 3, edges = FALSE)")
legend("topright", c("True signal","kzs estimate"), cex = 0.8, 
col = c("red", "black"), lty = 1:1, lwd = 2, bty = "n")  
}
\keyword{ smooth }
\keyword{ ts }
\keyword{ nonparametric }