\name{scanText}
\alias{scanText}
\title{Scan a Character String}
\description{
  Useful for converting data entered via a HTML textarea, for example, 
  into a vector for further processing.
}
\usage{
scanText(string, ...)
}
\arguments{
  \item{string}{A character string, typically numbers or words 
  separated by white space}
  \item{\dots}{Other arguments to be passed to \code{scan}}
}
\value
  \code{scan(textConnection(string)}, a character vector.
}
\author{David Firth \email{david.firth@nuffield.ox.ac.uk}}
\examples{
scanText("A few short words")
as.numeric(scanText("1 2 3\n89 90"))
}
\keyword{interface}