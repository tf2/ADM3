\name{dLRs}
\alias{dLRs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Caculate the DLR Spread. }
\description{
Calculates the "DLR spread" - derivative log2 ratio spread.
}
\usage{
dLRs(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{		- vector of log ratio values.}
}
\details{
Calculates the probe-to-probe log ratio difference of an array. This is a noise estimation which is robust to outliers.
}
\value{
The derivative log2 ratio spread.
}
\references{ Agilent }
\author{ Tomas William Fitzgerald }
\note{  A value used to compute the minimum log ratio difference needed to detect an outlier.\cr
}
\seealso{ \url{http://www.home.agilent.com/agilent/home.jspx} }
\examples{
r1 = rnorm(100, 0, 3)
dLRs(r1)
r2 = rnorm(100, 0, 5)
dLRs(r2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
