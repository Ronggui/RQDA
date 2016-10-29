\name{codingBySearch}
\alias{codingBySearch}
\title{
Auto-coding by pattern matching
}
\description{
Applies the specified code to a specified file based on given text pattern.
}
\usage{
codingBySearch(pattern, fid = getFileIds(), cid, seperator="\n",
               concatenate = FALSE, ...)
}
\arguments{
  \item{pattern}{
a text string, to be matched to the text in the RQDA project file
}
  \item{fid}{
File id, in the GUI 'File' tab click on file to see its id
}
  \item{cid}{
Code id, in the GUI 'Codes' tab click on code to see its id
}
  \item{seperator}{
  single character string, specifying the separator of unit of analysis.
}
  \item{concatenate}{
  a boolean value, if TRUE then matches in adjacent units (ie only 
  separated by 'seperator') are fused into a single coding.
}
\item{\ldots}{
  arguments passed to \link{gregexpr}.
  
  A useful example is the 'ignore.case' argument.
}
}
\details{
The function can be used if one is interested in automatically applying a code to 
paragraphs in a file based on certain words specified by 'pattern'. 

It first splits the whole text into pieces which depend on the separator, then matches the pattern with each pieces. When a match is found, the piece is coded to the code specified by cid. The default separator defines paragraph as unit of analysis. The separator is passed to the pattern argument of \link{gregexpr}.

This function is also useful for keyword in context (KWIC) analysis.
}
\value{
The function is used for its side effect.
}
\author{
Ronggui Huang
}

\seealso{
\link{getFileIds}
}
\examples{
\dontrun{
codingBySearch("internet",fid=1,cid=2)

codingBySearch("internet",fid=getFileIds(),cid=2)

codingBySearch("internet",fid=getFileIds(),cid=2, seperator="[.!?]")
}
}
