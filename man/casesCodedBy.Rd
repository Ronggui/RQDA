\name{casesCodedBy}
\alias{casesCodedByAnd}
\alias{casesCodedByOr}
\alias{casesCodedByNot}
\title{
Get cases coded by specific codes, by specifying the code IDs.
}
\description{
Get cases coded by specific codes, by specifying the code IDs.
}
\usage{
casesCodedByAnd(cid)

casesCodedByOr(cid)

casesCodedByNot(cid)
}
\arguments{
  \item{cid}{
   an integer vector of code IDs.
}
}
\details{
\code{casesCodedByAnd} returns case IDs which are coded by all codes from cid.

\code{casesCodedByOr} returns case IDs which are coded by any code from cid.

\code{casesCodedByNot} returns case IDs which are not coded by any code from cid.
}
\value{
a vector of file IDs, with class of "RQDA.vector" and "caseId".
}
\author{
Ronggui HUANG
}
\seealso{
\code{\link{getCaseNames}}
}
\examples{\dontrun{
filesCodedByAnd(1:2) ## coded by code 1 and 2

filesCodedByOr(1:2) ## coded by code 1 or 2

filesCodedByNot(1:2) ## coded by codes other than 1 and 2
}
}