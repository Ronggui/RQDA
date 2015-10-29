new_proj <- function(path, conName="qdacon",assignenv=.rqda,...){
  ## sucess <- file.create(tmpNamme <- tempfile(pattern = "file", tmpdir = dirname(path)))
  sucess <- (file.access(names=dirname(path),mode=2)==0)
  if (!sucess) {
    gmessage("No write permission.",icon="error",container=TRUE)
  }
  else{
    ## unlink(tmpNamme)
    path <- paste(gsub("\\.rqda$","",path),"rqda",sep=".") ## deal with the ".rqda"
    override <- FALSE
    if (fexist <- file.exists(path)) {
      ## if there exists a file, should ask; and test if have write access to overwrite it.
      override <- gconfirm("Over write existing project?",icon="warning")
      if (file.access(path, 2) != 0 && override) {
        override <- FALSE
        gmessage("You have no write permission to overwrite it.",con=TRUE,icon="error")
      }
    }
    if (!fexist | override ){
      ## close con in assignmenv first.
      tryCatch(closeProject(conName=conName,assignenv=assignenv),error=function(e){})
      if (Encoding(path)=='UTF-8') {
          Encoding(path)='unknown'
          ## otherwise, it is illegible under windows when path contains chinese because it is in utf8 encoding
          assign(conName,dbConnect(drv=dbDriver("SQLite"),dbname=path),envir=assignenv)
          Encoding(path) <- "UTF-8"
      } else {
          assign(conName,dbConnect(drv=dbDriver("SQLite"),dbname=path),envir=assignenv)
      }
      con <- get(conName,assignenv)

      if (dbExistsTable(con,"source")) dbRemoveTable(con, "source")
      ## interview record
      dbGetQuery(con,"create table source (name text, id integer,
                                           file text, memo text,
                                           owner text, date text, dateM text, status integer)")
      ## dateM means modified date
      if (dbExistsTable(con,"freecode")) dbRemoveTable(con, "freecode")
      ## list of free codes
      dbGetQuery(con,"create table freecode  (name text, memo text,
                                              owner text,date text,dateM text,
                                              id integer, status integer, color text)")
      if (dbExistsTable(con,"treecode")) dbRemoveTable(con, "treecode")
      ## tree-like strcuture of code (relationship between code and code-category[codecat])
      dbGetQuery(con,"create table treecode  (cid integer, catid integer,
                                              date text, dateM text,
                                              memo text, status integer, owner text)")
      if (dbExistsTable(con,"treefile")) dbRemoveTable(con, "treefile")
      ## tree-like structure of interview record  (relationship between file and file category [filecat])
      dbGetQuery(con,"create table treefile  (fid integer, catid integer,
                                              date text,dateM text,
                                              memo text, status integer,owner text)")
      if (dbExistsTable(con,"filecat")) dbRemoveTable(con, "filecat")
      ## file category
      dbGetQuery(con,"create table filecat  (name text,fid integer, catid integer, owner text,
                                             date text, dateM text,memo text, status integer)")
      if (dbExistsTable(con,"codecat")) dbRemoveTable(con, "codecat")
      ## code category
      dbGetQuery(con,"create table codecat  (name text, cid integer, catid integer, owner text, date text,
                                             dateM text,memo text, status integer)")
      if (dbExistsTable(con,"coding")) dbRemoveTable(con, "coding")
      ## coding: code and its coded text chunks
      dbGetQuery(con,"create table coding  (cid integer, fid integer,seltext text,
                                            selfirst real, selend real, status integer,
                                            owner text, date text, memo text)")
      if (dbExistsTable(con,"coding2")) dbRemoveTable(con, "coding2")
      ## second coding
      dbGetQuery(con,"create table coding2  (cid integer, fid integer,seltext text,
                                            selfirst real, selend real, status integer,
                                            owner text, date text, memo text)")
      if (dbExistsTable(con,"project")) dbRemoveTable(con, "project")
      ##dbGetQuery(con,"create table project  (encoding text, databaseversion text, date text,dateM text,
      ##                                       memo text,BOM integer)")
      dbGetQuery(con,"create table project  (databaseversion text, date text,dateM text,
                                             memo text,about text)")
      dbGetQuery(con,sprintf("insert into project (databaseversion,date,about,memo) values ('0.2.2','%s',
                            'Database created by RQDA (http://rqda.r-forge.r-project.org/)','')",date()))
      if (dbExistsTable(con,"cases")) dbRemoveTable(con, "cases")
      dbGetQuery(con,"create table cases  (name text, memo text,
                                           owner text,date text,dateM text,
                                           id integer, status integer)")
      if (dbExistsTable(con,"caselinkage")) dbRemoveTable(con, "caselinkage")
      dbGetQuery(con,"create table caselinkage  (caseid integer, fid integer,
                                                selfirst real, selend real, status integer,
                                            owner text, date text, memo text)")

      if (dbExistsTable(con,"attributes")) dbRemoveTable(con, "attributes")
      dbGetQuery(.rqda$qdacon,"create table attributes (name text, status integer, date text, dateM text, owner text,memo text)")
      if (dbExistsTable(con,"caseAttr")) dbRemoveTable(con, "caseAttr")
      dbGetQuery(.rqda$qdacon,"create table caseAttr (variable text, value text, caseID integer, date text, dateM text, owner text)")
      if (dbExistsTable(con,"fileAttr")) dbRemoveTable(con, "fileAttr")
      dbGetQuery(.rqda$qdacon,"create table fileAttr (variable text, value text, fileID integer, date text, dateM text, owner text)")
      if (dbExistsTable(con,"journal")) dbRemoveTable(con, "journal")
      dbGetQuery(.rqda$qdacon,"create table journal (name text, journal text, date text, dateM text, owner text,status integer)")
      RQDAQuery("alter table project add column imageDir text")
      try(RQDAQuery("alter table attributes add column class text"),TRUE)
      RQDAQuery("alter table caseAttr add column status integer")
      RQDAQuery("alter table fileAttr add column status integer")
      try(RQDAQuery("create table annotation (fid integer,position integer,annotation text, owner text, date text,dateM text, status integer)"),TRUE)
      if (dbExistsTable(con,"image")) dbRemoveTable(con, "image")
      RQDAQuery("create table image (name text, id integer, date text, dateM text, owner text,status integer)")
      if (dbExistsTable(con,"imageCoding")) dbRemoveTable(con, "imageCoding")
      RQDAQuery("create table imageCoding (cid integer,iid integer,x1 integer, y1 integer, x2 integer, y2 integer, memo text, date text, dateM text, owner text,status integer)")
    }
  }
}

UpgradeTables <- function(){
  Fields <- dbListFields(.rqda$qdacon,"project")
  if (!"databaseversion" %in% Fields) {
    dbGetQuery(.rqda$qdacon,"alter table project add column databaseversion text")
    dbGetQuery(.rqda$qdacon,"update project set databaseversion='0.1.5'")
  }
  currentVersion <- dbGetQuery(.rqda$qdacon,"select databaseversion from project")[[1]]
  if (currentVersion=="0.1.5") {
    ##from="0.1.5"
    dbGetQuery(.rqda$qdacon,"create table caseAttr (variable text, value text, caseID integer, date text, dateM text, owner text)")
    ## caseAttr table
    dbGetQuery(.rqda$qdacon,"create table fileAttr (variable text, value text, fileID integer, date text, dateM text, owner text)")
    ## fileAttr table
    dbGetQuery(.rqda$qdacon,"create table attributes (name text, status integer, date text, dateM text, owner text, memo text)")
    ## attributes table
    dbGetQuery(.rqda$qdacon,"create table journal (name text, journal text, date text, dateM text, owner text,status integer)")
    ## journal table
    RQDAQuery("alter table project add column about text")
    dbGetQuery(.rqda$qdacon,"update project set about='Database created by RQDA (http://rqda.r-forge.r-project.org/)'")
    dbGetQuery(.rqda$qdacon,"update project set databaseversion='0.1.9'")
    ## reset the version.
    ## added for version 0.1.8
    ## (no version 0.1.7 to make the version number consistent with RQDA version)
    RQDAQuery("alter table project add column imageDir text")
    try(RQDAQuery("alter table attributes add column class text"),TRUE)
    RQDAQuery("alter table caseAttr add column status integer")
    RQDAQuery("alter table fileAttr add column status integer")
    RQDAQuery("alter table freecode add column color text")
    RQDAQuery("update caseAttr set status=1")
    RQDAQuery("update fileAttr set status=1")
    try(RQDAQuery("create table annotation (fid integer,position integer,annotation text, owner text, date text,dateM text, status integer)"),TRUE)
    RQDAQuery("create table image (name text, id integer, date text, dateM text, owner text,status integer)")
    RQDAQuery("create table imageCoding (cid integer,iid integer,x1 integer, y1 integer, x2 integer, y2 integer, memo text, date text, dateM text, owner text,status integer)")
  }
  if (currentVersion=="0.1.6"){
    RQDAQuery("alter table project add column about text")
    dbGetQuery(.rqda$qdacon,"update project set about='Database created by RQDA (http://rqda.r-forge.r-project.org/)'")
    dbGetQuery(.rqda$qdacon,"update project set databaseversion='0.1.9'")
    RQDAQuery("alter table project add column imageDir text")
    try(RQDAQuery("alter table attributes add column class text"),TRUE)
    RQDAQuery("alter table caseAttr add column status integer")
    RQDAQuery("update caseAttr set status=1")
    RQDAQuery("alter table fileAttr add column status integer")
    RQDAQuery("alter table freecode add column color text")
    RQDAQuery("update fileAttr set status=1")
    try(RQDAQuery("create table annotation (fid integer,position integer,annotation text, owner text, date text,dateM text, status integer)"),TRUE)
    RQDAQuery("create table image (name text, id integer, date text, dateM text, owner text,status integer)")
    RQDAQuery("create table imageCoding (cid integer,iid integer,x1 integer, y1 integer, x2 integer, y2 integer, memo text, date text, dateM text, owner text,status integer)")
  }
  if (currentVersion=="0.1.8"){
    dbGetQuery(.rqda$qdacon,"update project set databaseversion='0.1.9'")
    RQDAQuery("alter table freecode add column color text")
  }
  if (currentVersion<"0.2.0"){
    if (dbExistsTable(.rqda$qdacon,"coding2")) dbRemoveTable(.rqda$qdacon, "coding2")
    dbGetQuery(.rqda$qdacon,"create table coding2  (cid integer, fid integer,seltext text,
                                            selfirst real, selend real, status integer,
                                            owner text, date text, memo text)")
    dbGetQuery(.rqda$qdacon,"update project set databaseversion='0.2.0'")
  }
  if (currentVersion<"0.2.2"){
      RQDAQuery("alter table treecode add column owner text")
      RQDAQuery("alter table treefile add column owner text")
      dbGetQuery(.rqda$qdacon,"update project set databaseversion='0.2.2'")
  }
}

open_proj <- function(path,conName="qdacon",assignenv=.rqda,...){
  tryCatch( { con <- get(conName,assignenv)
              pkg <- attr(attr(con,"class"),'package')
              Open <- getFunction("dbIsValid",where=sprintf("package:%s",pkg))(con)
             if (open) dbDisconnect(con)
              },
            error=function(e){})
  ## Fist close the con if it exist, then open a new con.
  if (file.access(path, 2) == 0) {
      Encoding(path) <- "unknown"
      assign(conName, dbConnect(drv=dbDriver("SQLite"),dbname=path),envir=assignenv)
  } else if (file.access(path, 4) == 0){
      Encoding(path) <- "unknown"
      assign(conName, dbConnect(drv=dbDriver("SQLite"),dbname=path),envir=assignenv)
      gmessage("You don't have write access to the *.rqda file. You can only read the project.",container=TRUE,icon="warning")
  } else {
      gmessage("You don't have read access to the *.rqda file. Fail to open.",container=TRUE,icon="error")
  }
}



closeProject <- function(conName="qdacon",assignenv=.rqda,...){
  tryCatch({
    con <- get(conName,assignenv)
    if (is_projOpen(message=FALSE)) {
        tryCatch(dispose(.rqda$.sfp),error=function(e){})
        tryCatch(dispose(.rqda$.root_edit),error=function(e){})
        WidgetList <- ls(envir=.rqda,pattern="^[.]codingsOf",all.names=TRUE)
        for (i in WidgetList) tryCatch(dispose(get(i,envir=.rqda)),error=function(e){})
        closeProjBF() ## update all widgets
        if (!dbDisconnect(con)) {
        gmessage("Closing project failed.",icon="waring",container=TRUE)
      }
    }
  } ,error=function(e){})
}



is_projOpen <- function(envir=.rqda,conName="qdacon",message=TRUE){
  ## test if any project is open.
  open <- FALSE
  tryCatch({
    con <- get(conName,envir)
    pkg <- attr(attr(con,"class"),'package')
    Open2 <- getFunction("dbIsValid",where=sprintf("package:%s",pkg))(con)
    open <- open + Open2
    } ,error=function(e){})
  if (!open & message) gmessage("No Project is Open.",icon="warning",container=TRUE)
  return(open)
}

backup_proj <- function(con){
  ## con=.rqda$qdacon
  dbname <- con@dbname
  Encoding(dbname) <- "UTF-8"
  backupname <- sprintf("%s%s.rqda",gsub("rqda$","",dbname),format(Sys.time(), "%H%M%S%d%m%Y"))
  success <- file.copy(from=dbname, to=backupname , overwrite = FALSE)
  if (success) {
    gmessage("Succeeded!",container=TRUE,icon="info")
  } else{
    gmessage("Fail to back up the project.",container=TRUE,icon="error")
  }
}

ProjectMemoWidget <- function(){
  if (is_projOpen(envir=.rqda,"qdacon")) {
    ## use enviroment, so you can refer to the same object easily, this is the beauty of environment
    ## if project is open, then continue
    tryCatch(dispose(.rqda$.projmemo),error=function(e) {})
    ## Close the open project memo first, then open a new one
    ## .projmemo is the container of .projmemocontent,widget for the content of memo
    wnh <- size(.rqda$.root_rqdagui) ## size of the main window
    gw <- gwindow(title="Project Memo", parent=c(wnh[1]+10,2),
                width = min(c(gdkScreenWidth()- wnh[1]-20,getOption("widgetSize")[1])),
                height = min(c(wnh[2],getOption("widgetSize")[2]))
                 )
    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    gw@widget@widget$SetIconFromFile(mainIcon)
    assign(".projmemo", gw, envir=.rqda)
    .projmemo <- get(".projmemo",.rqda)
    .projmemo2 <- gpanedgroup(horizontal = FALSE, container=.projmemo)
    ## use .projmemo2, so can add a save button to it.
    proj_memoB <- gbutton("Save memo",container=.projmemo2,handler=function(h,...){
      ## send the new content of memo back to database
      newcontent <- svalue(W)
      ## Encoding(newcontent) <- "UTF-8"
      newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
      dbGetQuery(.rqda$qdacon,sprintf("update project set memo='%s' where rowid=1", ## only one row is needed
                                      newcontent)
                 ## have to quote the character in the sql expression
                 )
      mbut <- get("proj_memoB",envir=button)
      enabled(mbut) <- FALSE ## grey out the  button
  }
            )## end of save memo button
    assign("proj_memoB",proj_memoB,envir=button)
    tmp <- gtext(container=.projmemo2,font.attr=c(sizes="large"))
    gSignalConnect(tmp@widget@widget$GetBuffer(), "changed",
                   function(h,...){
                       mbut <- get("proj_memoB",envir=button)
                       enabled(mbut) <- TRUE
                   })##
    font <- pangoFontDescriptionFromString(.rqda$font)
    gtkWidgetModifyFont(tmp@widget@widget,font)
    assign(".projmemocontent",tmp,envir=.rqda)
    prvcontent <- dbGetQuery(.rqda$qdacon, "select memo from project")[1,1]
    ## [1,1]turn data.frame to 1-length character. Existing content of memo
    if (length(prvcontent)==0) {
      dbGetQuery(.rqda$qdacon,"replace into project (memo) values('')")
      prvcontent <- ""
      ## if there is no record in project table, it fails to save memo, so insert sth into it
    }
    W <- .rqda$.projmemocontent
    Encoding(prvcontent) <- "UTF-8"
    ## add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
    add(W,prvcontent,do.newline=FALSE)
    ## do.newline:do not add a \n (new line) at the beginning
    ## push the previous content to the widget.
    enabled(proj_memoB) <- FALSE
    addHandlerUnrealize(get(".projmemo",envir=.rqda),handler <- function(h,...){
      withinWidget <- svalue(get(".projmemocontent",envir=.rqda))
      InRQDA <- dbGetQuery(.rqda$qdacon, "select memo from project where rowid=1")[1, 1]
      if (isTRUE(all.equal(withinWidget,InRQDA))) {
        return(FALSE) } else {
          val <- gconfirm("The memo has bee change, Close anyway?",container=TRUE)
          return(!val)
        }
    }
                        )
    }
}

close_AllCodings <- function(){
  obj <- ls(.rqda,all.names=TRUE,pattern="^.codingsOf")
  if (length(obj)!=0) {
    for (i in obj){tryCatch(dispose(get(i,envir=.rqda)),error=function(e){})
                 }
  }
}
