\name{getCut}
\alias{getCut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Find a cut-off. }
\description{
Find a absolute cut-of on mean ratio.
}
\usage{
getCut(dlrss, fa, mi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dlrss}{ - the dLRs of a segment. }
  \item{fa}{ - a factor for scaling. }
  \item{mi}{ - the mimimum expected dLRs of the sample. }
}
\details{
Can define cut-offs based on local data noise.
}   
\value{
A cut-of value.
}
\author{ Tomas William Fitzgerald }
\note{ note. }
\examples{
getCut(0.12, 0.3, 0.08)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
