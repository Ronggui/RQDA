\name{filesByCodes}
\alias{filesByCodes}
\title{Relation between files and codes.}
\description{
Return a data frame which indicates what codes are associted with each file.
}
\usage{
filesByCodes(codingTable = c("coding", "coding2"))
}
\arguments{
  \item{codingTable}{ name of the coding table in rqda database.}
}
\details{
The result is a data frame. Each row represents one file, and each
variable represents one code. If a file is coded by a code, the value of
that variable is 1, otherwise is 0.
}
\value{
A data frame.
}
\author{Ronggui HUANG}
%\seealso{}
%\examples{}