\name{summaryCodings}
\alias{summaryCodings}
\alias{print.summaryCodings}
\title{Summary of codings}
\description{
Give a summary of codings of current project.
}
\usage{
summaryCodings(byFile = FALSE, ...)
\method{print}{summaryCodings}(x, ...)
}
\arguments{
  \item{byFile}{When it is FALSE, return the summary of current project. 
When it is TRUE, return the summary of coding for each coded file.}
  \item{x}{An object returned by \code{summaryCoding}.}
  \item{\dots}{Other possible arguments.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list:
	\item{NumOfCoding}{Number of coding for each code.}
	\item{AvgLength}{Average number of characters in codings for 
each code.}
	\item{NumOfFile}{Number of files coded for each code.}
	\item{CodingOfFile}{Number of codings for each file. Returns NULL if 
byFile is FALSE.}
}
\author{ HUANG Ronggui}
\seealso{\code{\link{getFileIds}} and \code{\link{getCodingTable}}}
\examples{
\dontrun{
summaryCodings()
summaryCodings(FALSE)
 }
}
