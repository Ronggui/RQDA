\name{Deletion}
\alias{list.deleted}
\alias{pdelete}
\alias{undelete}
\alias{CleanProject}
\title{Functions for dealing with the temporarily deleted data}
\description{
  \code{list.deleted} shows the temporarily deleted data (deleted by
  delete button, which is only tagged with deletion mark in the *.rqda file).
  \code{pdelete} permanently deletes them.
  \code{CleanProject} cleans the *.rqda file (call pdelete with every possible
  value for the type argument).
  \code{undelete} removes the temporarily deletion mark to reuse the temporarily deleted data.
}
\usage{
list.deleted(type=c("file","code","case","codecategory","filecategory"))

pdelete(type=c("file","code","case","codecategory","filecategory","coding"),
        ask=FALSE)

CleanProject(ask=FALSE)

undelete(type=c("file","code","case","codecategory","filecategory"),ask=TRUE)
}

\arguments{
  \item{type}{Types of elements in the *.rqda file. "file"
  is the name of file (in the Files Tab). "code" is the name of codes
  (in the Codes Tab). "case" is the name of case (in the Case Tab). "codecategory" is name of code
  category (in the C-Cat Tab). "filecategory" is name of file category
  (in the F-Cat Tab). "coding" is the text segment associated
  with specific code.}
  \item{ask}{You can choose which ones to be deleted when is
  TRUE. Otherwise, it will delete all with temporarily deletion mark.}
}

\details{
  By GUI, you can delete file, code, case, code category and file
  categroy. When click the delete button, the status of related elements
  (e.g. for file, the elements includ file, related coding, related case
  category and file category) are set from 1 to 0. In this sense,
  deletion from GUI is temporary. After that, you can use
  \code{list.deleted} to show which ones are tagged as deleted.
  By \code{pdelete}, you can permenantly delete those tagged with
  temporarily deletion mark. By \code{undelete}, you can undo the
  temporary deletion, the status of related elements are set back to 1.

  When ask is FALSE, it will apply to all the propriate elements of
  specific type. When it is TRUE, you can choose the elements of the
  specific type which the action (pdelet or undelete) applies to.
}

\note{
In order to make the temporarily deletion of code and the associated
coding can be undeleted again, RQDA differentiates the temporarily
deletion of codings (which are deleted by deleting a code) from that
produced by unmark button in the Coding Tab: the former with status = 0
while the latter with status = -1.
}

\value{
For \code{list.deleted}, a data frame if there are some records with
temporarily deletion mark for the specified type.
For \code{pdelete}, \code{CleanProject} and \code{undelete}, no value is return. These
functions are used for the side-effects.
}

\author{Ronggui HUANG}
%\keyword{ utilities }
