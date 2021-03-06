% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printIntegral.R
\docType{methods}
\name{print,Integral-method}
\alias{print,Integral-method}
\title{Printing an Integral}
\usage{
\S4method{print}{Integral}(x)
}
\value{
The answer
 \item{Answer}{The value calculated using Trapezoid or Simpson's Rule}
}
\description{
Uses Trapezoid or Simpson's Rule to approximate the area under a set of points and prints the output
}
\section{Slots}{

\describe{
\item{\code{x}}{An object of class Trapezoid or Integral}
}}

\examples{

printIntegral(newTrapezoid(1:4, c(3,6,12,13), "Trapezoid"))
}
\seealso{
newTrapezoid, newSimpson, newIntegral, integrateIt
}
\author{
Emily Garner<\email{emily.garner@wustl.edu}>
}
