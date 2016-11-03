\name{getFileIdSets}
\Rdversion{1.1}
\alias{getFileIdSets}
\title{
  Get file id from sets.
}
\description{
  Get the file id from file-sets given the type of relation between sets. File-set is defined by the case or
  filecategory.
}
\usage{
getFileIdSets(set = c("case", "filecategory"), relation = c("union", "intersect"))
}
\arguments{
  \item{set}{type of set, either "case" or "filecategory".}
  \item{relation}{relation between sets, either "union" or "interset".}
}
\details{
  File-set is defined by case or file category. Files belonging to a
  case/filecategory are in a set. This function gets file id from the
  selected sets. When multiple sets are selected, the relation between
  them can be defined. When relation is union, file ids from either
  selected set are returned. When relation is intersect, only file ids
  that appear in all the selected sets are returned.
}
\value{
A numeric vector or NULL if no file id is well-defined.
}
\author{
  HUANG Ronggui
}
\seealso{ \code{\link{retrieval}}, \code{\link{getFileIds}}}
