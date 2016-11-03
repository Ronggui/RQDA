\name{write.FileList}
\alias{write.FileList}
\alias{addFilesFromDir}
\title{Import a batch of files to the source table}
\description{
If importing a single file to the project, you can do it by clicking import button in the Files Tab. 
Sometimes, you want to import a batch of files quickly, you can do it by command. \code{write.FileList} can 
be used to import a batch of files into the source table in the *.rqda file. \code{addFilesFromDir} can add
all files from a directory into *.rqda.
}
\usage{
write.FileList(FileList, encoding = .rqda$encoding, con = .rqda$qdacon, ...)

addFilesFromDir(dir, pattern = "*.txt$")
}

\arguments{
  \item{FileList}{A list. Each element of the list is the file content, and the \code{names(FileList)} are the respective file name.}
  \item{encoding}{ Don't change this argument.}
  \item{con}{ Don't change this argument.}
  \item{dir}{ Path of a directory where plain files are located.}
  \item{pattern}{Argument passed to \code{list.files}; only files matching this pattern are imported.}
  \item{\dots}{ \code{\dots} is not used.}
}
\details{
The file content will be converted to UTF-8 character before being written to 
*.rqda. The original content can be in any suitable encoding, so you 
can inspect the content correctly; In other words,the better 
practices is to used the corresponding encoding (you can get a hint by 
\code{localeToCharset} function) to save the imported 
files.
}
\value{
 This function is used for the side-effects. No value is return.
}
\author{Huang Ronggui}
\examples{
\dontrun{
Files <- list("File name one"="content of first File.",
              "File name two"="content of the second File.")
write.FileList(Files) ## Please launch RQDA(), and open a project first.
}
}
% \keyword{ ~kwd1 }
