\name{gselect.list}
\alias{gselect.list}
\title{Select Items from a List}
\description{
  Select item(s) from a character vector.
}
\usage{
gselect.list(list, multiple = TRUE, title = NULL, width = 200, height = 500, ...)
}
\arguments{
  \item{list}{character vector. A list of items.}
  \item{multiple}{logical: can more than one item be selected?}
  \item{title}{optional character string for window title.}
  \item{width}{integer. width of the widget.}
  \item{height}{integer. heighth of the widget.}
  \item{\dots}{Not used currently.}
}
\details{
  GTK version of \code{\link[utils]{select.list}}.
}
\note{
  The license of this function is subject to interpretation of the first author.
}
\value{
  A character vector of selected items with encoding of UTF-8. If no
  item was selected and click 'OK', it returns length 0 character
  vector. If click 'Cancel', '""' is returned.
}
\author{John Verzani and Ronggui HUANG}
\seealso{\code{\link[utils]{select.list}}}
\examples{
\dontrun{
select.list(sort(.packages(all.available = TRUE)))
}
}

