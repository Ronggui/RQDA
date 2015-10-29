\name{viewPlainFile}
\alias{viewPlainFile}
\title{
View the file content of the selected file in File Widget without displaying the codings and annotations etc.
}
\description{
This function displays a data file in its bare form. The codings, annotations or other modifications done in RQDA won't be displayed.
}
\usage{
viewPlainFile(FileNameWidget = .rqda$.fnames_rqda)
}
\arguments{
  \item{FileNameWidget}{Users should leave it as it is.
}
}
\details{
This function is useful to view the raw version of the data files. None of the codings, annotations, memos done in RQDA will be displayed on the file when it's called by this function. To use the function open a project and select the data file you wish to view and type 'ViewPlainFile()' in the command line.
}
\value{
No value is return. It is used for the side effect: the function returns a widget window with the plain file in it.
}
\author{
HUANG Ronggui
}
