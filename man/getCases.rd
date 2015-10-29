\name{getCases}
\Rdversion{1.1}
\alias{getCaseIds}
\alias{getCaseNames}
\alias{getCases}
\title{
Get the Case ID and Case Name.
}
\description{
Return cases  IDs or names which a set of files belong to.
}
\usage{
getCaseIds(fid = GetFileId(), nFiles = FALSE)

getCaseNames(caseId = GetCaseId(nFiles = FALSE))

getCases(fid, names = TRUE) 
}

\arguments{
  \item{fid}{
    numeric vector, the file IDs.
  }
  \item{nFiles}{
    logical, return the number of files that belong to a case.
  }
  \item{caseId}{
    numeric vector, the case IDs.
  }
  \item{names}{
    logical.
  }
}
\details{
\code{GetCaseId} returns the case IDs which a file belongs to given the file IDs.

\code{GetCaseName} returns the case Names given the case IDs.

\code{getCases} returns the case Names or IDs depending on the argument of names. It is a wrapper of \code{GetCaseId} and \code{GetCaseName}.
}
\value{
  \code{GetCaseId} returns a data frame of two columns when nFiles is
  TRUE, and a numeric vector when FALSE.

  \code{GetCaseName} returns a character vector or NULL if no cases are
  associated with the file IDs.

  \code{getNames} return the names of cases when names is TRUE, id of files when FALSE.
}
\author{
  HUANG Ronggui
}
\seealso{
  See Also \code{\link{getFileIds}}
}
\examples{
\dontrun{
GetCaseName(GetCaseId(GetFileId("filecategory")))
}
}