\name{kzs}
\alias{kzs}
\title{ Kolmogorov-Zurbenko Spline }
\description{
  The KZS utilizes splines to construct a smooth estimate of a signal buried in noise.
}
\usage{
kzs(x, delta, h, k = 1, show.edges = FALSE)
}
\arguments{
  \item{x}{ 
    a data frame of paired values X and Y. The data frame needs two columns of data 
    representing pairs (Xi, Yi), i = 1,..., \emph{n} and X, Y are real values; the first
    column of data represents the one dimensional input variable, X, and the second 
    column, Y, denotes the variable to be used as the response.
  }
  \item{delta}{ 
    the physical range of smoothing in terms of unit values of \code{X}.\cr
	\emph{Restriction:} \eqn{\code{delta << (max(X) - min(X))}}  
  }
  \item{h}{ 
    a scale reading of all outcomes of the algorithm. More specifically, \code{h} is the interval
    width of a uniform scale covering the interval \eqn{\code{(Xn - delta/2, Xn + delta/2)}}.\cr
	\emph{Restriction:} \eqn{\code{h < min{X(i+1) - X(i)}}} and \eqn{\code{h > 0}}
  }
  \item{k}{ 
    the number of iterations the function will execute; \code{k} may also be interpreted as
    the order of smoothness (as a polynomial of degree \code{k-1}).  By default, \code{k} is set to perform
    a single iteration.
  }
  \item{show.edges}{ 
    a logical indicating whether or not to display the resulting data beyond the range of \code{x}
    values of the user-supplied data. If \code{false}, then the extended edges are suppressed. By
    default, this parameter is set to \code{false}.
  }
}
\details{
  The relation between variables Y and X as a function [namely, Y(x)] of a current value of 
  X = x is often desired as a result of practical research. Usually we search for some simple 
  function Y(x) when given a data set of pairs (Xi, Yi). These pairs frequently resemble a 
  noisy plot, and thus Y(x) is desired to be a smooth outcome from the original data to capture 
  important patterns in the data, while leaving out the noise. The KZS estimates a 
  solution to this problem through use of splines, a particular nonparametric estimator of a function.  
  Given a data set of pairs (Xi, Yi), splines estimate the smooth values of Y from X's. The KZS 
  averages all values of Y for all X within the range \code{delta} around each scale reading 
  \code{hi} along the variable X. The KZS algorithm is designed to smooth all fast fluctuations 
  in Y within the \code{delta}-range in X, while keeping ranges more then \code{delta} untouched. The 
  separation of short scales less than \code{delta} and long scales more than \code{delta} is becoming 
  more effective with higher \code{k}, while effective range of separation is becoming \eqn{\code{delta}*sqrt(\code{k})}. 
}
\value{
  a two-column data frame containing:
  \item{Xk }{X values resulting from execution of algorithm}
  \item{Y(Xk) }{Response values resulting from execution of algorithm}
}
\references{ "Spline Smoothing." \url{http://economics.about.com/od/economicsglossary/g/splines.htm}}
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} }
\seealso{ \code{\link{argkzs}} }
\note{ 
  KZS is designed for the general situation, including time series data. In many 
  applications where the variable X can be time, KZS can resolve the problem of missing values in
  time series or irregularly observed values in longitudinal data analysis.  KZS may take time to 
  completely run depending on the size of the data set used and the number of iterations specified.
}
\examples{
  # This example was created with the intent to push the limits of KZS. The 
  # function has a wide peak and a sharp peak; for a wide peak, you may permit 
  # stronger smoothing and for a sharp peak you may not (you would be over- 
  # smoothing). The key here is to find satisfying values for the parameters.
  
  # EXAMPLE 1
  
  t <- seq(from=-round(400*pi),to=round(400*pi),by=.25) #Total time t
  tp <- seq(from=0,to=round(400*pi),by=.25)		#Positive t (includes t=0)
  tn <- seq(from=-round(400*pi),to=-.25,by=.25)         #Negative t
  nobs <- 1:length(t)                                   #Sequence of obs            
  
  # True signal
  signalp <- 0.5*sin(sqrt((2*pi*abs(tp))/200))          #Positive side of signal
  signaln <- 0.5*sin(-sqrt((2*pi*abs(tn))/200))         #Negative side of signal
  signal <- append(signaln,signalp,after=length(tn))    #Appending into one signal
  	
  # Randomly generate noise from the standard normal distribution
  et <- rnorm(length(t),mean=0,sd=1)
  
  # Add the noise to the true signal
  yt <- et + signal
  
  # Data frame of (t,yt) 
  pts <- data.frame(cbind(t,yt))
  
  # Plot of the true signal
  plot(signal~t,xlab='t',ylab='Signal',main='True Signal',type="l")
  
  # Plot of yt (signal + noise)
  plot(yt~t,ylab=expression(paste(Y[t])),main='Signal buried in noise',type="p")
  
  # Apply KZS function - 3 iterations
  kzs(pts,delta=80,h=.2,k=3,show.edges=FALSE)
  lines(signal~t,col="red")
  title(main="KZS(delta=80, h=0.20, k=3, show.edges=false)")
  legend("topright", c("True signal","KZS estimate"), cex=0.8, col=c("red","black"), 
  lty=1:1, lwd=2, bty="n")
  
  
  
  # EXAMPLE 2 - Rerun KZS on the same function after removing 20\% of the data 
  #             points. This provides an opportunity to create a random scale 
  #             along the variable X.
  
  # Generate and remove a random 20\% of t 
  t20 <- sample(nobs,size=length(nobs)/5)        #Random 20\% of (t,yt)
  pts20 <- pts[-t20,]                            #Remove the 20\%
  
  # Plot of (t,yt) with 20\% removal
  plot(pts20$yt~pts20$t,xlab='t',ylab=expression(paste(Y[t])),main='Signal buried 
  in noise - 20\% removal',type="p")
  
  # Apply KZS function - 3 iterations
  kzs(pts20,delta=80,h=.20,k=3,show.edges=FALSE)
  lines(signal~t,col="red")
  title(main="KZS(delta=80, h=0.20, k=3, show.edges=false) - 20\% removal")
  legend("topright", c("True signal","KZS estimate"), cex=0.8, col=c("red","black"), 
  lty=1:1, lwd=2, bty="n")
}
\keyword{ smooth }
\keyword{ ts }
\keyword{ nonparametric }