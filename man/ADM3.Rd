\name{ADM3}
\alias{ADM3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Automated Detection Algorithm Interpretation. }
\description{
Performs change point detection on a "bed" formatted file.
}
\usage{
ADM3(file, outfile, t=6, np=3, mr=0.3, autoCut=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ - "bed" formatted input file (chr, start, stop, value, score).}
  \item{outfile}{ - output file location (a report of detected features). }
  \item{t}{ - the main detection threshold - controls sensitivity - not recommended below 4. }
  \item{np}{ - number of data points required to detect a feature. }
  \item{mr}{ - minimum mean ratio to detect a segemnt. }
  \item{autoCut}{ - attempt noise dependant auto filtering. }
}
\details{
Performs change point detection on a bed formatted file and allows an error value to be applied to each probe.\cr
This is a highly efficent implementation - takes less than 2 mins to process 1 million data points (most of which is spend reading the input file!).\cr
}
\value{
A list containing input data (with detection score and feature indices) and a report (detected features).
}
\references{ Agilent }
\author{ Tomas William Fitzgerald }
\note{  This algorihtm in now very robust - it is noise aware - and has several further modifications, improving its performance.\cr
}
\seealso{ \url{http://www.home.agilent.com/agilent/home.jspx} }
\examples{
#exampleADM3()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
