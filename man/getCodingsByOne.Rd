\name{getCodingsByOne}
\alias{getCodingsByOne}
\title{
Return codings of one code.
}
\description{
get codings of a code.
}
\usage{
getCodingsByOne(cid, fid=NULL,codingTable=c("coding","coding2"))
}
\arguments{
  \item{cid}{code id, an integer.}
  \item{fid}{file id, an integer vector.}
  \item{codingTable}{name of coding table.}
}
\details{
It gets codings of a code with cid from files which are specified by fid.
}
\value{
a data frame with additional class of "codingsByOne".
}
\author{
Ronggui HUANG
}
\seealso{
\code{\link{\%and\%}}, \code{\link{\%or\%}}, \code{\link{\%not\%}}
}
\examples{
\dontrun{
getCodingsByOne(1)
}
}