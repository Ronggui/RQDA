\name{getCodingTable}
\alias{getCodingTable}
\title{ Get the information of codings }
\description{
Get the information of codings.
}
\usage{
getCodingTable()
}
\details{
Codings are stored in the coding table of *.rqda file. The coding table contains necessary information, but not informative to end-users. For example, it has id of code list and file list, but not the name of them, which are stored in freecode table and source table respectively. \code{GetCodingTable} joins information from the three tables, and returns more informative data. See value section on the the returned components.
}
\value{
A data frame:
  \item{cid }{Code id}
  \item{fid }{File id}
  \item{codename }{Code name, in accordance with cid}
  \item{filename }{File name, in accordance with fid}
  \item{CodingLength }{The number of characters in the coding}
  \item{index1}{beginning index of a coding}
  \item{index2}{end index of a coding}
}
\author{ HUANG Ronggui}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%\examples{}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
