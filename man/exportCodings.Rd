\name{exportCodings}
\Rdversion{1.1}
\alias{exportCodings}
\title{
Export codings to a HTML file.
}
\description{
To export retrieved codings to a HTML file.
}
\usage{
exportCodings(file = "Exported Codings.html", Fid = NULL,
             order = c("fname", "ftime", "ctime"), append = FALSE,
             codingTable="coding")
}
\arguments{
  \item{file}{
Length-one character vector, specify the name of exported file.
}
  \item{Fid}{
Integer vector of file id.The retrieved codings are from this subset of files. When is NULL, it means all the files.
}
  \item{order}{
Specify the order of retrieved codings. see details for the meanings.
}
  \item{append}{
Logical, when TRUE the exported codings are appended to the existing file (if exists); otherwise, it overwrites the existing file.
}
  \item{codingTable}{name of sqlite data table where codings are stored. It should be either "coding" or "coding2"}
}
\details{
"fname" means order the codings by file names, "ftime" by file imported time, and "ctime" by time of coding.
}
\value{
A html file.
}
\author{
HUANG Ronggui
}
