## newMySQLProjMySQL <- function(user='rqda',password='rqdaproj',dbname='rqda',host='localhost',...)
## {
##     require(RMySQL)
##     con = dbConnect(MySQL(),user=user,password=password,dbname=dbname,host=host)
##     assign("qdacon",con , envir=.rqda)
##     if (dbExistsTable(con,"source")) dbRemoveTable(con, "source")
##     ## interview record
##     dbGetQuery(con,"create table source (name text, id integer,
##                                            file text, memo text,
##                                            owner text, date text, dateM text, status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     ## dateM means modified date
##     if (dbExistsTable(con,"freecode")) dbRemoveTable(con, "freecode")
##     ## list of free codes
##     dbGetQuery(con,"create table freecode  (name text, memo text,
##                                               owner text,date text,dateM text,
##                                               id integer, status integer, color text, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"treecode")) dbRemoveTable(con, "treecode")
##     ## tree-like strcuture of code (relationship between code and code-category[codecat])
##     dbGetQuery(con,"create table treecode  (cid integer, catid integer,
##                                               date text, dateM text,
##                                               memo text, status integer, owner text, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"treefile")) dbRemoveTable(con, "treefile")
##     ## tree-like structure of interview record  (relationship between file and file category [filecat])
##     dbGetQuery(con,"create table treefile  (fid integer, catid integer,
##                                               date text,dateM text,
##                                               memo text, status integer,owner text, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"filecat")) dbRemoveTable(con, "filecat")
##     ## file category
##     dbGetQuery(con,"create table filecat  (name text,fid integer, catid integer, owner text,
##                                              date text, dateM text,memo text, status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"codecat")) dbRemoveTable(con, "codecat")
##     ## code category
##     dbGetQuery(con,"create table codecat  (name text, cid integer, catid integer, owner text, date text,
##                                              dateM text,memo text, status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"coding")) dbRemoveTable(con, "coding")
##     ## coding: code and its coded text chunks
##     dbGetQuery(con,"create table coding  (cid integer, fid integer,seltext text,
##                                             selfirst real, selend real, status integer,
##                                             owner text, date text, memo text, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"coding2")) dbRemoveTable(con, "coding2")
##     ## second coding
##     dbGetQuery(con,"create table coding2  (cid integer, fid integer,seltext text,
##                                             selfirst real, selend real, status integer,
##                                             owner text, date text, memo text, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"project")) dbRemoveTable(con, "project")
##     dbGetQuery(con,"create table project  (databaseversion text, date text,dateM text,
##                                              memo text,about text, imageDir text, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     dbGetQuery(con,sprintf("insert into project (databaseversion,date,about,memo) values ('0.2.2','%s',
##                             'Database created by RQDA (http://rqda.r-forge.r-project.org/)','')",date()))
##     if (dbExistsTable(con,"cases")) dbRemoveTable(con, "cases")
##     dbGetQuery(con,"create table cases  (name text, memo text,
##                                            owner text,date text,dateM text,
##                                            id integer, status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"caselinkage")) dbRemoveTable(con, "caselinkage")
##     dbGetQuery(con,"create table caselinkage  (caseid integer, fid integer,
##                                                 selfirst real, selend real, status integer,
##                                             owner text, date text, memo text, rowid integer NOT NULL primary key AUTO_INCREMENT)")

##     if (dbExistsTable(con,"attributes")) dbRemoveTable(con, "attributes")
##     dbGetQuery(con,"create table attributes (name text, status integer, date text, dateM text, owner text,memo text, class text, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"caseAttr")) dbRemoveTable(con, "caseAttr")
##     dbGetQuery(.rqda$qdacon,"create table caseAttr (variable text, value text, caseID integer, date text, dateM text, owner text, status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"fileAttr")) dbRemoveTable(con, "fileAttr")
##     dbGetQuery(.rqda$qdacon,"create table fileAttr (variable text, value text, fileID integer, date text, dateM text, owner text, status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"journal")) dbRemoveTable(con, "journal")
##     dbGetQuery(.rqda$qdacon,"create table journal (name text, journal text, date text, dateM text, owner text,status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"annotation")) dbRemoveTable(con, "annotation")
##     dbGetQuery(.rqda$qdacon,"create table annotation (fid integer,position integer,annotation text, owner text, date text,dateM text, status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"image")) dbRemoveTable(con, "image")
##     dbGetQuery(con,"create table image (name text, id integer, date text, dateM text, owner text,status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
##     if (dbExistsTable(con,"imageCoding")) dbRemoveTable(con, "imageCoding")
##     dbGetQuery(con,"create table imageCoding (cid integer,iid integer,x1 integer, y1 integer, x2 integer, y2 integer, memo text, date text, dateM text, owner text,status integer, rowid integer NOT NULL primary key AUTO_INCREMENT)")
## }

## openMySQLProject <- function(user='rqda',password='rqdaproj',dbname='rqda',host='localhost',updateGUI=TRUE,...) {
##     tryCatch(.rqda$.codes_rqda[]<-NULL,error=function(e){})
##     tryCatch(.rqda$.fnames_rqda[]<-NULL,error=function(e){})
##     tryCatch(.rqda$.CasesNamesWidget[]<-NULL,error=function(e){})
##     tryCatch(.rqda$.CodeCatWidget[]<-NULL,error=function(e){})
##     tryCatch(.rqda$.CodeofCat[]<-NULL,error=function(e){})
##     tryCatch(.rqda$.FileCatWidget[]<-NULL,error=function(e){})
##     tryCatch(.rqda$.FileofCat[]<-NULL,error=function(e){})
##     tryCatch(.rqda$.AttrNamesWidget[] <- NULL,error=function(e){})
##     tryCatch(.rqda$.JournalNamesWidget[] <- NULL,error=function(e){})
##     tryCatch(closeProject(assignenv=.rqda),error=function(e){})
##     ## close currect project before open a new one.
##     require(RMySQL)
##     con = dbConnect(MySQL(),user=user,password=password,dbname=dbname,host=host)
##     assign("qdacon",con ,envir=.rqda)
##     if (updateGUI) {
##         svalue(.rqda$.currentProj) <- "Opening ..."
##         UpgradeTables()
##         tryCatch(CodeNamesUpdate(sortByTime=FALSE),error=function(e){})
##         tryCatch(FileNamesUpdate(sortByTime=FALSE),error=function(e){})
##         tryCatch(CaseNamesUpdate(),error=function(e){})
##         tryCatch(UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat"),error=function(e){})
##         tryCatch(UpdateCodeofCatWidget(),error=function(e){})
##         tryCatch(UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat"),error=function(e){})
##         tryCatch(UpdateFileofCatWidget(),error=function(e){})
##         tryCatch(AttrNamesUpdate(),error=function(e){})
##         tryCatch(JournalNamesUpdate(),error=function(e){})
##         path <- gsub("\\\\","/",dbGetInfo(.rqda$qdacon)$dbname)
##         path <- gsub("/","/ ",path)
##         svalue(.rqda$.currentProj) <- gsub("/ ","/",paste(strwrap(path,50),collapse="\n"))
##         gtkWidgetSetSensitive(button$cloprob$widget,TRUE)
##         gtkWidgetSetSensitive(button$BacProjB$widget,TRUE)
##         gtkWidgetSetSensitive(button$proj_memo$widget,TRUE)
##         gtkWidgetSetSensitive(button$CleProB$widget,TRUE)
##         gtkWidgetSetSensitive(button$CloAllCodB$widget,TRUE)
##         gtkWidgetSetSensitive(button$ImpFilB$widget,TRUE)
##         enabled(button$NewFilB) <- TRUE
##         gtkWidgetSetSensitive(RQDA.rqda$.fnames_rqda$widget,TRUE)
##         enabled(button$AddJouB) <- TRUE
##         enabled(button$AddCodB) <- TRUE
##         enabled(button$AddCodCatB) <- TRUE
##         enabled(button$AddCasB) <- TRUE
##         enabled(button$AddAttB) <- TRUE
##         enabled(button$AddFilCatB) <- TRUE
##         enabled(.rqda$.JournalNamesWidget) <- TRUE
##         enabled(.rqda$.codes_rqda) <- TRUE
##         enabled(.rqda$.SettingsGui) <- TRUE
##         enabled(.rqda$.CodeCatWidget) <- TRUE
##         enabled(.rqda$.CasesNamesWidget) <- TRUE
##         enabled(.rqda$.AttrNamesWidget) <- TRUE
##         enabled(.rqda$.FileCatWidget) <- TRUE
##     }
## }
