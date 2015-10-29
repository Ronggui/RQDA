\name{filesCodedBy}
\alias{filesCodedByAnd}
\alias{filesCodedByOr}
\alias{filesCodedByNot}
\title{
Get files coded by specific codes, by specifying the code IDs.
}
\description{
Files coded by a specific set of codes.
}
\usage{
filesCodedByAnd(cid,codingTable=c("coding","coding2"))

filesCodedByOr(cid,codingTable=c("coding","coding2"))

filesCodedByNot(cid,codingTable=c("coding","coding2"))
}
\arguments{
  \item{cid}{an integer vector of code IDs.}
 \item{codingTable}{name of coding table.}
}
\details{
\code{filesCodedByAnd} returns file IDs which are coded by all codes from cid.

\code{filesCodedByOr} returns file IDs which are coded by any code from cid.

\code{filesCodedByNot} returns file IDs which are not coded by any code from cid.
}
\value{
a vector of file IDs, with class of "RQDA.vector" and "fileId".
}
\author{
Ronggui HUANG
}
\seealso{
\code{\link{getFileNames}}
}
\examples{
\dontrun{
filesCodedByAnd(1:2) ## coded by code 1 and 2

filesCodedByOr(1:2) ## coded by code 1 or 2

filesCodedByNot(1:2) ## coded by codes other than 1 and 2
}
}