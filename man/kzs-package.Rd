\name{kzs-package}
\alias{kzs-package}
\docType{package}
\title{ Kolmogorov-Zurbenko Spline }
\description{
A collection of functions utilizing splines to estimate the underlying 
signal of the given input data.
}
\details{
\tabular{ll}{
Package: \tab kzs\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-06-26\cr
License: \tab GPL Version 2 or later\cr
}
The relation between variables Y and X as a function [namely, Y(x)] of a current value of 
X = x is often desired as a result of practical research. Usually we search for some simple 
function Y(x) when given a data set of pairs (Xi, Yi). These pairs frequently resemble a 
noisy plot, and thus Y(x) is desired to be a smooth outcome from the original data to capture 
important patterns in the data, while leaving out the noise.\cr 
The \code{KZS} function estimates a solution to this problem through use of splines, which is 
a nonparametric estimator of a function.  Given a data set of pairs (Xi, Yi), splines estimate 
the smooth values of Y from X's.
}
\author{
Derek Cyr \email{dc896148@albany.edu} and Igor Zurbenko \email{igorg.zurbenko@gmail.com} 

Maintainer: Derek Cyr <dc896148@albany.edu>
}
\references{
"Spline Smoothing." \url{http://economics.about.com/od/economicsglossary/g/splines.htm}
}
\keyword{ package }
\seealso{
\code{\link[kzs:kzs]{kzs}}
}
