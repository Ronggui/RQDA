\name{queryFiles}
\Rdversion{1.1}
\alias{queryFiles}
\title{
Retrieval of file names according to their codings.
}
\description{
To retrieve file names according to their codings.
}
\usage{
queryFiles(or=NULL, and = NULL, not = NULL, names = TRUE)
}
\arguments{
  \item{or}{ integer vector of code id.}
  \item{and}{ integer vector of code id.}
  \item{not}{integer vector of code id.}
  \item{names}{logical, returns file names when TRUE.}
}
\details{
Let fid.or are files coded by any code from or, fid.and are files coded by all codes of and, and fid.not are files not coded by any code of not. Then the result is \code{ setdiff(intersect(fid.or, fid.and), fid.not)}.

This function is succeeded by \code{\link{filesCodedByAnd}}, \code{\link{filesCodedByOr}}, \code{\link{filesCodedByNot}} and their operators.
}
\value{
A vector of file id when names is FALSE. A vector of file names, with the side effect of updating files widget
with these file names when names is TRUE.
}
\author{
HUANG Ronggui
}

\examples{
\dontrun{
QueryFile(or=1:2) ## files coded to code 1 or 2.
QueryFile(and=1:2) ## files coded to code 1 and 2.
QueryFile(or=1:2, not=3:4) ## files coded to code 1 or 2 but neither 3 nor 4.
}
}
