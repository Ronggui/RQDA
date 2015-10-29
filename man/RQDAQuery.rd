\name{RQDAQuery}
\Rdversion{1.1}
\alias{RQDAQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Execute a SQL statement on the open *.rqda file.
}
\description{
Submits and executes an arbitrary SQL statement on the open *.rqda file.
}
\usage{
RQDAQuery(sql)
}
\arguments{
  \item{sql}{
    a character vector of length 1 with the SQL statement.
  }
}
\details{
  It is a wrapped version of \code{\link[RSQLite]{query}}, to make it more
  convenient to submit and execute a SQL statement.
}
\value{
  The same of \code{\link[RSQLite]{query}}, possible NULL (for the side effects of
  sql on the *.rqda file) or a data.frame with the output (if any) of
  the query.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
HUANG Ronggui
}
\seealso{
   See Also as \code{\link[RSQLite]{query}}
}
\examples{
\dontrun{
RQDAQuery("select name from source where status=1")
}
}