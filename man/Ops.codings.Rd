\name{Ops.codings}
\alias{and}
\alias{or}
\alias{not}
\title{
  Boolean operation on codings.
}
\description{
  Return the result codings of the Boolean operation.
}
\usage{
and(CT1, CT2)

or(CT1, CT2)

not(CT1, CT2)
}
\arguments{
  \item{CT1}{
    Coding of code one.
  }
  \item{CT2}{
    Coding of code two.
  }
}
\details{
  CT1 and CT2 are subset of \code{getCodingTable} of a specific code or
  returned value of \code{getCodingsByOne}. In former situation, only
  columns of "index1", "index2","fid","filename" from CT1 and CT2 are 
  used by this function.

  These functions are the same as \code{\%and\%}, \code{\%or\%}, \code{\%not\%}.
}
\value{
  An object of class "codingsByOne" and "data.frame". It consists:
  \item{index1}{}
  \item{index2}{}
  \item{fid}{}
  \item{filename}{}
  \item{rowid}{}
  \item{coding}{The codings, or the text segements.}
}
\author{
  Ronggui HUANG
}
\seealso{
  \code{\link{relation}}, \code{\link{getCodingTable}}, \code{\link{\%and\%}}
}
\examples{
\dontrun{
a <- getCodingTable()
c1 <- subset(a,cid==6)
c2 <- subset(a,cid==24)
ans <- and(c1, c2)
ans ## put it into a widget for inspection

## another way to do the same
and(getCodingsByOne(6), getCodingsByOne(24))

## or operator
or(getCodingsByOne(6), getCodingsByOne(24))

## not operator
not(getCodingsByOne(6), getCodingsByOne(24))
}
}
