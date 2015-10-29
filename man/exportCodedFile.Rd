\name{exportCodedFile}
\alias{exportCodedFile}
\title{
 exported a coded file to HTML fiel
}
\description{
Exported a coded file to a HTML file with codings and code labels
}
\usage{
exportCodedFile(file, fid, closeAfter = TRUE)
}
\arguments{
  \item{file}{
 character string to specify the HTML file path.}
  \item{fid}{
 the file id for export.}
  \item{closeAfter}{
When TRUE, the file widget is closed after export.}
}
%\details{}
\value{
No returned value, for its side effect only.
}
%\references{}
\author{
Ronggui HUANG
}
\examples{
\dontrun{
exportCodedFile("~/coded file with id of 1.html", fid=1)
}
}