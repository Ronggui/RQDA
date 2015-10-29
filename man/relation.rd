\name{relation}
\alias{relation}
\title{ Relation between two codings}
\description{
  To calculate the relation between two codings, given the coding indexes.
}
\usage{
relation(index1, index2)
}
\arguments{
  \item{index1}{ The first coding index, it is length-2 integer vector
    with the first element (index1[1]) less than the second element (index1[2]).}
  \item{index2}{ The second coding index, it is length-2 integer vector
    with the first element (index2[1]) less than the second element (index2[2]).}
}
\details{
The relation between two codings can be any of inclusion, overlap, exact
(special case of inclusion and overlap) and proximity (Neither overlap
nor inclusion). It should be noted that two adjacent codings are
regarded as proximity with distance of 0.
}
\value{
  A 6-element list:
  \item{Relation}{Length-1 character, standing for the type of
    relation. It may be one of inclusion, overlap, exact or proximity.}
  \item{OverlapIndex}{Length-2 vector, the index of overlapping between
    two coding indexes. It is c(NA,NA) when relation is proximity.}
  \item{UnionIndex}{Length-2 vector, the index of union of the two
    coding indexes. It is c(NA,NA) when relation is proximity.}
  \item{Distance}{Distance of two coding indexes. It is NA when relation is not proximity.}
  \item{WhichMin}{Which argument (index1 or index2) has the minimum
    value. If both have the same minmum value, return NA.}
  \item{WhichMax}{Which argument (index1 or index2) has the maximum
    value. If both have the same maxmum value, return NA.}
}
\author{ HUANG Ronggui}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
\dontrun{
relation(c(20,30),c(22,28)) # inclusion
relation(c(10,40),c(20,80)) # overlap
relation(c(10,20),c(30,50)) # proximity with distance of 10
relation(c(10,20),c(20,50)) # proximity with distance of 0
relation(c(10,20),c(10,20)) # exact
relation(c(10,20),c(10,30)) # WhichMin is c(1,2)
}
}
