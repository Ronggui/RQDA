## part of RQDA project
## by ronggui HUANG

.onAttach <- function(...) {
  ## use .onLoad/.onAttach rather than .First.lib when there is namespace
  ## Refer R news 2003-1 for details about name space
  optOld <- options()
  ## if (is.null(getOption("widgetCoordinate"))) options(widgetCoordinate=c(400,2))
  ## cordinate of ViewFunWidget
  if (is.null(getOption("widgetSize"))) options(widgetSize=pmin(
                                                c(550,700),
                                                c(gdkScreenHeight(),gdkScreenWidth()-300)
                                                ))
  options(andMethod=c("overlap","exact","inclusion"))
  assign("optOld",optOld,envir=.rqda)
  if (interactive()) {
     packageStartupMessage("\nUse 'RQDA()' to start the programme.\n")
     RQDA()
   }
}

.onUnload <- function(...){
  cat("Bye, RQDA is unloaded.\n")
  options(.rqda$optOld)
}


## use cam name conventions; duplicated the original functions for smooth transition.
## must in ultis.R
## crossCodes <- CrossCode
crossTwoCodes <- CrossTwo
exportCodings <- ExportCoding
getCaseNames <- GetCaseName
getCaseIds <- GetCaseId
getCodingTable <- GetCodingTable
queryFiles <- QueryFile
getFileNames <- GetFileName
getFileIds <- GetFileId
getFileIdSets <- GetFileIdSets
getAttr <- GetAttr
showSubset <- ShowSubset
