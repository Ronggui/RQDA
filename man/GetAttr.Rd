\name{getAttr}
\alias{getAttr}
\alias{showSubset}
\title{attributes}
\description{
Get the attributes of case or file.
}
\usage{
getAttr(type = c("case", "file"),attrs = svalue(.rqda$.AttrNamesWidget),
        subset)

showSubset(x,...)
}
\arguments{
  \item{type}{Type of attributes.}
  \item{attrs}{character vector, subset of attributes to retrieve.}
  \item{subset}{when subset is not missing, return subset only.}
  \item{x}{an object from \code{getAttr}}
  \item{\ldots}{Not used currently.}
}
\details{
You can add and modify the attributes of cases or files. \code{getAttr}
returns this attributes as a data frame.

Sometimes, you only want to show a subset of files or cases according to
their attributes. You can do the subset operation of the result from
\code{getAttr} and pass it to \code{showSubset}, or you can pass a
subset argument to \code{GetAttr}. The meaning of subset is the same as
that in \code{subset} function.
}
\value{
  For \code{getAttr}, when type is "case", it is a data frame with class
  of "CaseAttr"; when type is "file", it is a data frame with class of
  "FileAttr". For \code{showSubset}, no value is returned, the
  side-effect is to change the file list or case list in respective
  widget.
}
\author{HUANG Ronggui}
\note{
All the variables in the data frame is of class "character", you need to
convert to suitable class when conducting statistical analysis.
}
\examples{
\dontrun{
attr <- getAttr("case")
showSubset(subset(attr,attribute1==1)) ## assuming there is a variable
named atttribute1 in attr.
}
}

