\name{Ops.RQDA}
\alias{\%and\%}
\alias{\%or\%}
\alias{\%not\%}
\title{ Binary operations of some types of RQDA objects}
\description{
  Binary operations of RQDA.vector or codingsByOne.
}
\usage{
e1 \%and\% e2

e1 \%or\% e2

e1 \%not\% e2
}
\arguments{
  \item{e1}{a RQDA object.}
  \item{e2}{a RQDA object.}
}
\details{
e1 and e2 can be objects of class "RQDA.vector" includes classes of "fileId", "fileName", "caseId", "caseName". They can be objects of class "codingsByOne", see \code{\link{getCodingsByOne}}. e1 and e2 must be the same class.

For class of "RQDA.vector", \code{\%and\%} is the \code{\link{intersect}} of e1 and e2. \code{\%or\%} is the \code{\link{union}} of e1 and e2. \code{\%not\%} is the defined as \code{setdiff(e1, e2)}.
}
\value{
an object with the same structure and class of e1 and e2.
}
\author{ HUANG Ronggui}
\seealso{ \code{\link{intersect}}, \code{\link{union}}, \code{\link{setdiff}}}
\examples{
\dontrun{
filesCodeByAnd(1:2) \%and\% filesCodeByAnd(3) ## files coded by 1 and 2 as well as 3
filesCodeByAnd(1:2) \%or\% filesCodeByAnd(3) ## files coded by 1 and 2 or 3
filesCodeByAnd(1:2) \%not\% filesCodeByAnd(3) ## files coded by 1 and 2 but not 3

getCodingsByOne(1) \%or\% getCodingsByOne(2) ## codings of 1 or 2.
}
}