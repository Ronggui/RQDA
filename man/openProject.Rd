\name{Project}
\alias{openProject}
\alias{closeProject}
\title{
Open and close project.
}
\description{
To open or close a project (a *.rqda file) by command.
}
\usage{
openProject(path, updateGUI = FALSE)

closeProject(conName = "qdacon", assignenv = .rqda, ...) 
}
\arguments{
  \item{path}{
The path of of the *.rqda project file.
}
 \item{updateGUI}{
When TRUE, also update information on the GUI widgets.
}
 \item{conName}{
Do not change it.
}
 \item{assignenv}{
Do not change it.
}
 \item{\ldots}{
Do not change it.
}
}
\details{
These functions corresponde the internal functions of the "open project" and "close project" buttons.
}
\value{
No value is returned. For the side-effect only.
}
\author{
Ronggui HUANG
}

