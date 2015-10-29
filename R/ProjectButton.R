NewProjectButton <- function(container){
  gbutton("New Project",container=container,handler=function(h,...){
    path=gfile(type="save",text = "Type a name for the new project and click OK.")
    if (Encoding(path) != "UTF-8") {
        Encoding(path) <- "UTF-8"
    }
    if (path!=""){
     ## if path="", then click "cancel".
     new_proj(path,assignenv=.rqda)
     path <- .rqda$qdacon@dbname
     Encoding(path) <- "UTF-8" ## path created by gfile is in utf8 encoding
     path <- gsub("\\\\","/",path,fixed=TRUE)
     path <- gsub("/","/ ",path,fixed=TRUE)
      svalue(.rqda$.currentProj) <- gsub("/ ","/",paste(strwrap(path,60),collapse="\n"),fixed=TRUE)
      gtkWidgetSetSensitive(button$cloprob@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$BacProjB@widget@widget,TRUE)
      enabled(button$saveAsB) <- TRUE
      gtkWidgetSetSensitive(button$proj_memo@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$CleProB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$ImpFilB@widget@widget,TRUE)
      enabled(button$NewFilB) <- TRUE
      gtkWidgetSetSensitive(.rqda$.fnames_rqda@widget@widget,TRUE)
      enabled(button$AddJouB) <- TRUE
      enabled(button$AddCodB) <- TRUE
      enabled(button$AddCodCatB) <- TRUE
      enabled(button$AddCasB) <- TRUE
      enabled(button$AddAttB) <- TRUE
      enabled(button$AddFilCatB) <- TRUE
      enabled(.rqda$.JournalNamesWidget) <- TRUE
      enabled(.rqda$.codes_rqda) <- TRUE
      enabled(.rqda$.SettingsGui) <- TRUE
      enabled(.rqda$.CodeCatWidget) <- TRUE
      enabled(.rqda$.CasesNamesWidget) <- TRUE
      enabled(.rqda$.AttrNamesWidget) <- TRUE
      enabled(.rqda$.FileCatWidget) <- TRUE
  }
}
          )
}

OpenProjectButton <- function(container){
  gbutton("Open Project",container=container,handler=function(h,...){
    path <- gfile(text = "Select a *.rqda file and click OK.",type="open",
                  filter=list("rqda"=list(patterns = c("*.rqda")),"All files" = list(patterns = c("*"))))
    if (!is.na(path)){
      Encoding(path) <- "UTF-8"
      openProject(path,updateGUI=TRUE)
  }
}
          )
}

openProject <- function(path,updateGUI=FALSE) {
    tryCatch(.rqda$.codes_rqda[]<-NULL,error=function(e){})
    tryCatch(.rqda$.fnames_rqda[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CasesNamesWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CodeCatWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CodeofCat[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileCatWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileofCat[]<-NULL,error=function(e){})
    tryCatch(.rqda$.AttrNamesWidget[] <- NULL,error=function(e){})
    tryCatch(.rqda$.JournalNamesWidget[] <- NULL,error=function(e){})
    tryCatch(closeProject(assignenv=.rqda),error=function(e){})
    ## close currect project before open a new one.
    open_proj(path,assignenv=.rqda)
    if (updateGUI) {
        svalue(.rqda$.currentProj) <- "Opening ..."
        UpgradeTables()
        tryCatch(CodeNamesUpdate(sortByTime=FALSE),error=function(e){})
        tryCatch(FileNamesUpdate(sortByTime=FALSE),error=function(e){})
        tryCatch(CaseNamesUpdate(),error=function(e){})
        tryCatch(UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat"),error=function(e){})
        tryCatch(UpdateCodeofCatWidget(),error=function(e){})
        tryCatch(UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat"),error=function(e){})
        tryCatch(UpdateFileofCatWidget(),error=function(e){})
        tryCatch(AttrNamesUpdate(),error=function(e){})
        tryCatch(JournalNamesUpdate(),error=function(e){})
        path <- .rqda$qdacon@dbname
        Encoding(path) <- "UTF-8"
        path <- gsub("\\\\","/", path)
        path <- gsub("/","/ ",path)
        svalue(.rqda$.currentProj) <- gsub("/ ","/",paste(strwrap(path,50),collapse="\n"))
        gtkWidgetSetSensitive(button$cloprob@widget@widget,TRUE)
        gtkWidgetSetSensitive(button$BacProjB@widget@widget,TRUE)
        enabled(button$saveAsB) <- TRUE
        gtkWidgetSetSensitive(button$proj_memo@widget@widget,TRUE)
        gtkWidgetSetSensitive(button$CleProB@widget@widget,TRUE)
        gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,TRUE)
        gtkWidgetSetSensitive(button$ImpFilB@widget@widget,TRUE)
        enabled(button$NewFilB) <- TRUE
        gtkWidgetSetSensitive(.rqda$.fnames_rqda@widget@widget,TRUE)
        enabled(button$AddJouB) <- TRUE
        enabled(button$AddCodB) <- TRUE
        enabled(button$AddCodCatB) <- TRUE
        enabled(button$AddCasB) <- TRUE
        enabled(button$AddAttB) <- TRUE
        enabled(button$AddFilCatB) <- TRUE
        enabled(.rqda$.JournalNamesWidget) <- TRUE
        enabled(.rqda$.codes_rqda) <- TRUE
        enabled(.rqda$.SettingsGui) <- TRUE
        enabled(.rqda$.CodeCatWidget) <- TRUE
        enabled(.rqda$.CasesNamesWidget) <- TRUE
        enabled(.rqda$.AttrNamesWidget) <- TRUE
        enabled(.rqda$.FileCatWidget) <- TRUE
    }
}

closeProjBF <- function(){
    svalue(.rqda$.currentProj) <- "Closing ..."
    tryCatch(.rqda$.codes_rqda[]<-NULL,error=function(e){})
    tryCatch(.rqda$.fnames_rqda[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CasesNamesWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileofCase[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CodeCatWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CodeofCat[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileCatWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileofCat[]<-NULL,error=function(e){})
    tryCatch(.rqda$.AttrNamesWidget[] <- NULL,error=function(e){})
    tryCatch(.rqda$.JournalNamesWidget[] <- NULL,error=function(e){})
    svalue(.rqda$.currentProj) <- "No project is open."
    names(.rqda$.fnames_rqda) <- "Files"
    names(.rqda$.codes_rqda) <- "Codes List"
    names(.rqda$.CodeCatWidget)<-"Code Category"
    names(.rqda$.CodeofCat)<-"Codes of This Category"
    names(.rqda$.CasesNamesWidget) <- "Cases"
    names(.rqda$.FileofCase)<-"Files of This Case"
    names(.rqda$.FileCatWidget)<-"File Category"
    names(.rqda$.FileofCat)<-"Files of This Category"
    gtkWidgetSetSensitive(.rqda$.fnames_rqda@widget@widget,FALSE)
    enabled(.rqda$.JournalNamesWidget) <- FALSE
    enabled(.rqda$.codes_rqda) <- FALSE
    enabled(.rqda$.SettingsGui) <- FALSE
    enabled(.rqda$.CodeCatWidget) <- FALSE
    enabled(.rqda$.CodeofCat) <- FALSE
    enabled(.rqda$.CasesNamesWidget) <- FALSE
    enabled(.rqda$.FileofCase) <- FALSE
    enabled(.rqda$.AttrNamesWidget) <- FALSE
    enabled(.rqda$.FileCatWidget) <- FALSE
    enabled(.rqda$.FileofCat) <- FALSE
    gtkWidgetSetSensitive(button$cloprob@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$BacProjB@widget@widget,FALSE)
    enabled(button$saveAsB) <- FALSE
    gtkWidgetSetSensitive(button$proj_memo@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$CleProB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$ImpFilB@widget@widget,FALSE)
    enabled(button$NewFilB) <- FALSE
    gtkWidgetSetSensitive(button$DelFilB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$VieFilB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$FilMemB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$FilRenB@widget@widget,FALSE)
    enabled(button$FileAttrB) <- FALSE
    enabled(button$AddJouB) <- FALSE
    enabled(button$DelJouB) <- FALSE
    enabled(button$RenJouB) <- FALSE
    enabled(button$OpeJouB) <- FALSE
    enabled(button$AddCodB) <- FALSE
    enabled(button$RetB) <- FALSE
    enabled(button$DelCodB) <- FALSE
    enabled(button$codememobuton) <- FALSE
    enabled(button$FreCodRenB) <- FALSE
    ## enabled(button$c2memobutton) <- FALSE
    enabled(button$AddCodCatB) <- FALSE
    enabled(button$DelCodCatB) <- FALSE
    enabled(button$CodCatMemB) <- FALSE
    enabled(button$CodCatRenB) <- FALSE
    enabled(button$CodCatAddToB) <- FALSE
    enabled(button$CodCatADroFromB) <- FALSE
    enabled(button$AddCasB) <- FALSE
    enabled(button$DelCasB) <- FALSE
    enabled(button$CasRenB) <- FALSE
    enabled(button$CasMarB) <- FALSE
    enabled(button$CasUnMarB) <- FALSE
    enabled(button$CasAttrB) <- FALSE
    enabled(button$profmatB) <- FALSE
    enabled(button$AddAttB) <- FALSE
    enabled(button$DelAttB) <- FALSE
    enabled(button$RenAttB) <- FALSE
    enabled(button$AttMemB) <- FALSE
    enabled(button$SetAttClsB) <- FALSE
    enabled(button$AddFilCatB) <- FALSE
    enabled(button$DelFilCatB) <- FALSE
    enabled(button$FilCatRenB) <- FALSE
    enabled(button$FilCatMemB) <- FALSE
    enabled(button$FilCatAddToB) <- FALSE
    enabled(button$FilCatDroFromB) <- FALSE
}

CloseProjectButton <- function(container){
  cloprob <- gbutton("Close Project",container=container,handler=function(h,...){
  closeProjBF()
  closeProject(assignenv=.rqda)
  }
                     )
  assign("cloprob",cloprob,envir=button)
  gtkWidgetSetSensitive(button$cloprob@widget@widget,FALSE)
}

BackupProjectButton <- function(container){
  BacProjB <- gbutton("Backup Project",container=container,handler=function(h,...){
    backup_proj(con=.rqda$qdacon)
  }
                      )
  assign("BacProjB",BacProjB,envir=button)
  gtkWidgetSetSensitive(button$BacProjB@widget@widget,FALSE)
}


Proj_MemoButton <- function(label="Porject Memo",container,...){
  ## Each button a separate function -> more easy to debug, and the main function root_gui is shorter.
  ## The memo in dataset is UTF-8
  ## label of button
  ## name of contaianer or TRUE
  proj_memo <- gbutton(label, container=container, handler=function(h,...) {
    ProjectMemoWidget()
  }
                       )
  assign("proj_memo",proj_memo,envir=button)
  gtkWidgetSetSensitive(button$proj_memo@widget@widget,FALSE)
}


CleanProjButton <- function(label="Clean Project",container,...){
  CleProB <- gbutton(label, container=container, handler=function(h,...) {
    CleanProject(ask=FALSE)
  }
                     )
  assign("CleProB",CleProB,envir=button)
  gtkWidgetSetSensitive(button$CleProB@widget@widget,FALSE)
}

CloseAllCodingsButton <- function(label="Close All Codings",container,...){
  CloAllCodB <- gbutton(label, container=container, handler=function(h,...) {
    close_AllCodings()
  }
                        )
  assign("CloAllCodB",CloAllCodB,envir=button)
  gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,FALSE)
}


####################
## defunct functions
####################
## ProjectInforButton <- function(container){
## gbutton("Current Project",container=container,handler=function(h,...){
##     if (is_projOpen(envir=.rqda,conName="qdacon")) {
##       con <- .rqda$qdacon
##       dbname <- dbGetInfo(.rqda$qdacon)$dbname
##       ##substr(dbname, nchar(dbname)-15,nchar(dbname))
##       gmessage(dbname,title="Info about current project.",container=TRUE)
##     }
##   },
##                              action=list(env=.rqda,conName="qdacon")
##                              )
## }
