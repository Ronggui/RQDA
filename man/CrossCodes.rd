\name{crossCodes}
\alias{crossCodes}
\alias{crossTwoCodes}
\title{ Inter-codes relationship}
\description{
 Return a matrix, give a summary of inter-codes relationship.
}
\usage{
crossCodes(relation=c("overlap","inclusion","exact","proximity"),codeList=NULL,
          data=GetCodingTable(),print=TRUE,...)

crossTwoCodes(cid1,cid2,data,relation=c("overlap","inclusion","exact","proximity"),...)
}
\arguments{
 \item{relation}{The relation between codes}
 \item{codeList}{A character vector, the codes list on which the inter-code relationship is based}
 \item{data}{Data frame return by \code{getCodingTable},may be subset of the full coding table}
 \item{print}{When TRUE, print the results automatically}
 \item{cid1}{Length-1 code id. It is numeric.}
 \item{cid2}{Length-1 code id. It is numeric.}
 \item{\dots}{ \code{\dots} is not used yet.}
}
\details{
 The inter-codes relationship calculation is based on the relationship
 between the associated codings of the codes. 

 Giving the code name list (a character list), \code{crossCodes} returns
 the inter-relationship of 2 or more than 2 codes. \code{crossCodes}
 make heavy use of for loops, so it may takes a while to get the result
 when the coding table is large.

 \code{crossTwoCodes} returns the summary of inter-codes relationship of two
 codes based on the code id (each code id is a length-1 integer vector).
 
}
\value{
For \code{crossCodes}, it is a matrix. The upper matrix contains the
number of codings fitting the relation between the respective two
codes. the lower matrix is all NA. rownames of the matrix is the name of
the codes , and the colnames of the matrix is the corresponding id of
codes.

For \code{crossCodes}, it is a numeric vector.
}
\seealso{\code{\link{relation}}}
\examples{
\dontrun{
crossCodes()
}
}
