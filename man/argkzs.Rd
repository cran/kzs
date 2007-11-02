\name{argkzs}
\alias{argkzs}
\title{ Argument Limits for KZS }
\description{
  This function calculates the value for which the arguments \code{delta} and \code{h} 
  in the KZS function are bounded above or below by.
}
\usage{
argkzs(data, x)
}
\arguments{
  \item{data}{ 
    a data frame of paired values X and Y representing pairs (Xi, Yi ), i = 1,... ,n and 
    X, Y are real values.  This should be the data frame that is to be used with KZS.
  }
  \item{x}{ 
    an integer specifying the position of the column in the data frame containing the one 
    dimensional input variable, X, coordinates.
  }
}
\details{
  In the KZS function, the argument \code{delta} is the physical range of smoothing in terms of 
  unit values of X; the argument \code{h} is a scale reading of all outcomes of the algorithm.  
  More specifically, \code{h} is the interval width of a uniform scale overlaying the X axis.    
  The purpose of this function is to give an upper and/or lower bound on the values of \code{delta} 
  and \code{h} so that users may select appropriate values that satisfy all restrictions.  This 
  function eliminates any guess-work involved in choosing a satisfying value for \code{delta} and 
  \code{h} and should be used prior to KZS in order to save time and increase efficiency of use. 
}
\value{
  a list containing two elements:
  \item{delta }{the bounding value for the argument \code{delta}}
  \item{h }{the bounding value for the argument \code{h}}
}
\author{ Derek Cyr \email{cyr.derek@gmail.com} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} }
\seealso{ \code{\link{kzs}} }
\examples{
  #This example uses the same data from the KZS example

  # Define the time sequence
  t <- seq(from = -round(400*pi), to = round(400*pi), by = .25) 

  # Positive t (includes time = 0)
  tp <- seq(from = 0, to = round(400*pi), by = .25)	

  # Negative t	      
  tn <- seq(from = -round(400*pi), to = -.25, by = .25)                                                       

  # Positive side of signal
  signalp <- 0.5*sin(sqrt((2*pi*abs(tp))/200))

  # Negative side of signal             
  signaln <- 0.5*sin(-sqrt((2*pi*abs(tn))/200))

  # Appending into one signal           
  signal <- append(signaln, signalp, after = length(tn))    

  # Randomly generate noise from the standard normal distribution
  et <- rnorm(length(t), mean = 0, sd = 1)

  # Add the noise to the signal
  yt <- et + signal

  # Data frame of (t,yt) 
  pts <- data.frame(cbind(t,yt))

  argkzs(pts, 1)
}
\keyword{ smooth }
\keyword{ nonparametric }