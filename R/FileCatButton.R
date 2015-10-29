#################
AddFileCatButton <- function(label="ADD"){
  AddFilCatB <- gbutton(label,handler=function(h,...) {
    item <- ginput("Enter new File Category. ", icon="info")
    if (!is.na(item)){
      Encoding(item) <- "UTF-8"
      AddTodbTable(item,"filecat",Id="catid") ## FILE CATegory
      UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
    }
  }
                        )
  assign("AddFilCatB",AddFilCatB,button)
  enabled(AddFilCatB) <- FALSE
  AddFilCatB
}


DeleteFileCatButton <- function(label="Delete"){
  DelFilCatB <- gbutton(label, handler=function(h,...){
    del <- gconfirm("Really delete the File Category?",icon="question")
    if (isTRUE(del)){
      Selected <- svalue(.rqda$.FileCatWidget)
      Encoding(Selected) <- "UTF-8"
      catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",enc(Selected)))[,1]
      if (length(catid) ==1){
        dbGetQuery(.rqda$qdacon,sprintf("update filecat set status=0 where name='%s'",enc(Selected)))
        ## set status in table freecode to 0
        UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
        tryCatch(dbGetQuery(.rqda$qdacon,sprintf("update treefile set status=0 where catid='%s'",catid)),error=function(e){})
        ## should delete all the related codelists
        UpdateFileofCatWidget() ## update files of file cat widget
      } else gmessage("The Category Name is not unique.",con=TRUE)
    }
  }
                        )
  assign("DelFilCatB",DelFilCatB,button)
  enabled(DelFilCatB) <- FALSE
  DelFilCatB
}


FileCat_RenameButton <- function(label="Rename",Widget=.rqda$.FileCatWidget,...)
{
  ## rename of selected file cat.
  FilCatRenB <- gbutton(label,handler=function(h,...) {
    OldName <- svalue(Widget)
    ## get the new file names
    NewName <- ginput("Enter new Cateory name. ",text=OldName, icon="info")
    if (!is.na(NewName)) {
      Encoding(NewName) <- "UTF-8"
      rename(OldName,NewName,"filecat")
      UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
    }
  }
                      )
  assign("FilCatRenB",FilCatRenB,button)
  enabled(FilCatRenB) <- FALSE
  FilCatRenB
}

UpdateFileofCatWidget <- function(con=.rqda$qdacon,Widget=.rqda$.FileofCat,sortByTime=FALSE,...){
  SelectedFileCat <- svalue(.rqda$.FileCatWidget)
  if (length(SelectedFileCat)!=0){
    Encoding(SelectedFileCat) <- "UTF-8"
    catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",enc(SelectedFileCat)))[,1]
    Total_fid <- dbGetQuery(con,sprintf("select fid from treefile where status=1 and catid=%i",catid))
    if (nrow(Total_fid)!=0){
      items <- dbGetQuery(con,"select name,id,date from source where status=1")
      if (nrow(items)!=0) {
        items <- items[items$id %in% Total_fid$fid,c("name","date")]
        items <- items$name[OrderByTime(items$date)] ## sort by date
        Encoding(items) <- "UTF-8"
        if (!sortByTime) items <- sort(items)
      } else items <- NULL
    } else items <- NULL
  } else items <- NULL
  tryCatch(Widget[] <- items,error=function(e){})
}

UpdateFileofCatWidget2 <- function(con=.rqda$qdacon,Widget=.rqda$.FileofCat,sortByTime=FALSE,...)
{
  Total_fid <- GetFileIdSets("filecategory","intersect")
  if (length(Total_fid)!=0){
    items <- dbGetQuery(con,"select name,id,date from source where status=1")
    if (nrow(items)!=0) {
      items <- items[items$id %in% Total_fid,c("name","date")]
      items <- items$name[OrderByTime(items$date)] ## sort by date
      Encoding(items) <- "UTF-8"
      if (!sortByTime) items <- sort(items)
    } else items <- NULL
  } else items <- NULL
  tryCatch(Widget[] <- items,error=function(e){})
}



FileCatMemoButton <- function(label="Memo"){
  ans <- gbutton(label,handler=function(h,...) {
    MemoWidget("File Category",.rqda$.FileCatWidget,"filecat")
    }
                 )
  gtkWidgetSetTooltipText(getToolkitWidget(ans),"Memo of file category.")
  assign("FilCatMemB",ans,button)
  enabled(ans) <- FALSE
  ans
}


FileCatAddToButton <- function(label="AddTo",Widget=.rqda$.FileCatWidget,...)
{
  ans <- gbutton(label,handler=function(h,...) {
    SelectedFileCat <- svalue(.rqda$.FileCatWidget)
    catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",enc(SelectedFileCat)))[,1]
    freefile <-  dbGetQuery(.rqda$qdacon,"select name, id from source where status=1")
    if (nrow(freefile) == 0){gmessage("No files Yet.",cont=.rqda$.FileCatWidget)} else {
      Encoding(SelectedFileCat) <- Encoding(freefile[['name']]) <- "UTF-8"
      fileofcat <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and catid=%i",catid))
      if (nrow(fileofcat)!=0){
        fileoutofcat <- subset(freefile,!(id %in% fileofcat$fid))
      } else  fileoutofcat <- freefile
      Selected <- gselect.list(fileoutofcat[['name']],multiple=TRUE,x=getOption("widgetCoordinate")[1])
      if (Selected != ""){
        ## Selected <- iconv(Selected,to="UTF-8") ## already Encoded as UTF-8.
        fid <- fileoutofcat[fileoutofcat$name %in% Selected,"id"]
        Dat <- data.frame(fid=fid,catid=catid,date=date(),dateM=date(),memo=NA,status=1,owner=.rqda$owner)
        dbWriteTable(.rqda$qdacon,"treefile",Dat,row.names=FALSE,append=TRUE)
        UpdateFileofCatWidget()
      }
    }
  }
                 )
  gtkWidgetSetTooltipText(getToolkitWidget(ans),"Add file(s) to the selected file category.")
  assign("FilCatAddToB",ans,button)
  enabled(ans) <- FALSE
  return(ans)
}

FileCatDropFromButton <- function(label="DropFrom",Widget=.rqda$.FileofCat,...)
{
  ans <- gbutton(label,handler=function(h,...) {
    FileOfCat <- svalue(Widget)
    ## Give a confirm msg
    del <- gconfirm(sprintf("Delete %i file(s) from this category. Are you sure?",length(FileOfCat)),con=TRUE,icon="question")
    if (isTRUE(del)){
      SelectedFileCat <- svalue(.rqda$.FileCatWidget)
      Encoding(SelectedFileCat) <- Encoding(FileOfCat)<- "UTF-8"
      catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",enc(SelectedFileCat)))[,1]
      for (i in FileOfCat){
        fid <- dbGetQuery(.rqda$qdacon,sprintf("select id from source where status=1 and name='%s'",enc(i)))[,1]
        dbGetQuery(.rqda$qdacon,sprintf("update treefile set status=0 where catid=%i and fid=%i",catid,fid))
      }
      ## update .CodeofCat Widget
      ## .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[],FileOfCat)
      UpdateWidget(".FileofCat",from=FileOfCat,to=NULL)
      ## UpdateFileofCatWidget()
    }
  }
                 )
  gtkWidgetSetTooltipText(getToolkitWidget(ans),"Drop selected file(s) from file category.")
  assign("FilCatDroFromB",ans,button)
  enabled(ans) <- FALSE
  return(ans)
}



## AddToFileCategory<- function(){
## moved to FilesFun.R
##   ## filenames -> fid -> selfirst=0; selend=nchar(filesource)
##   filename <- svalue(.rqda$.fnames_rqda)
##   Encoding(filename) <- "unknown"
##   query <- dbGetQuery(.rqda$qdacon,sprintf("select id, file from source where name in(%s) and status=1",paste("'",filename,"'",sep="",collapse=","))) ## multiple fid
##   fid <- query$id
##   Encoding(query$file) <- "UTF-8"

##   ## select a F-cat name -> F-cat id
##   Fcat <- dbGetQuery(.rqda$qdacon,"select catid, name from filecat where status=1")
##   if (nrow(Fcat)==0){gmessage("Add File Categroy first.",con=TRUE)} else{
##     Encoding(Fcat$name) <- "UTF-8"
##     ##ans <- select.list(Fcat$name,multiple=FALSE)
##     CurrentFrame <- sys.frame(sys.nframe())
##     RunOnSelected(Fcat$name,multiple=TRUE,enclos=CurrentFrame,expr={
##     if (Selected!=""){ ## must use Selected to represent the value of selected items. see RunOnSelected() for info.
##       Selected <- iconv(Selected,to="UTF-8")
##       Fcatid <- Fcat$catid[Fcat$name %in% Selected]
##       exist <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and fid in (%s) and catid=%i",paste("'",fid,"'",sep="",collapse=","),Fcatid))
##     if (nrow(exist)!=length(fid)){
##     ## write only when the selected file associated with specific f-cat is not there
##       DAT <- data.frame(fid=fid[!fid %in% exist$fid], catid=Fcatid, date=date(),dateM=date(),memo='',status=1)
##       ## should pay attention to the var order of DAT, must be the same as that of treefile table
##       success <- dbWriteTable(.rqda$qdacon,"treefile",DAT,row.name=FALSE,append=TRUE)
##       ## write to caselinkage table
##       if (success) {
##       UpdateFileofCatWidget()
##       }
##       if (!success) gmessage("Fail to write to database.")
##     }}})}}



FileCatWidgetMenu <- list()
FileCatWidgetMenu$Memo$handler <- function(h,...){
 if (is_projOpen(env=.rqda,conName="qdacon")) {
   MemoWidget("File Category",.rqda$.FileCatWidget,"filecat")
   ## see CodeCatButton.R  for definition of MemoWidget
 }
}
FileCatWidgetMenu$"Delete all files of selected category"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    fid <- GetFileId("file")
    if (length(fid)>0){
      dbGetQuery(.rqda$qdacon, sprintf("update source set status=0 where id in (%s)",paste(shQuote(fid),collapse=",")))
      dbGetQuery(.rqda$qdacon, sprintf("update coding set status=0 where fid in (%s)",paste(shQuote(fid),collapse=",")))
      dbGetQuery(.rqda$qdacon, sprintf("update caselinkage set status=0 where fid in (%s)",paste(shQuote(fid),collapse=",")))
      dbGetQuery(.rqda$qdacon, sprintf("update treefile set status=0 where fid in (%s)",paste(shQuote(fid),collapse=",")))
      UpdateFileofCatWidget()
    }
  }
}
FileCatWidgetMenu$"Sort by created time"$handler <- function(h,...)
{
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
  }
}


## popup menu for files of this category
FileofCatWidgetMenu <- list()
FileofCatWidgetMenu$"Add To Case ..."$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
      AddFileToCaselinkage(Widget=.rqda$.FileofCat)
      UpdateFileofCaseWidget()
    }
}
FileofCatWidgetMenu$"Add To File Category ..."$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    AddToFileCategory(Widget=.rqda$.FileofCat,updateWidget=FALSE)
  }
}
FileofCatWidgetMenu$"Move To File Category ..."$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    fcatname <- svalue(.rqda$.FileCatWidget) ## should select one only
    fcatid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where name='%s'",
                                              enc(fcatname)))$catid
    fid <- GetFileId("file","select")
    ans <- AddToFileCategory(Widget=.rqda$.FileofCat,updateWidget=FALSE)
    if (isTRUE(ans)) {
      dbGetQuery(.rqda$qdacon,sprintf("update treefile set status=0 where fid in (%s) and catid='%s'",
                                      paste(shQuote(fid),collapse=","),
                                      fcatid))
      .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[],svalue(.rqda$.FileofCat))
    }}
  }
FileofCatWidgetMenu$"File Memo"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    MemoWidget("File",.rqda$.FileofCat,"source")
  }
}
FileofCatWidgetMenu$"Open Selected File"$handler <- function(h,...){
  ViewFileFun(FileNameWidget=.rqda$.FileofCat)
}
FileofCatWidgetMenu$"Edit Selected File"$handler <- function(h,...){
  EditFileFun(FileNameWidget=.rqda$.FileofCat)
}
FileofCatWidgetMenu$"Search Files Within Categroy"$handler <- function(h,...)
{
  if (is_projOpen(env=.rqda,conName="qdacon")) {
      fid <- GetFileId(condition="filecategory",type="all")
      pattern <- ifelse(is.null(.rqda$lastsearch),"file like '%%'",.rqda$lastsearch)
      pattern <- ginput("Please input a search pattern.",text=pattern)
      Encoding(pattern)<- "UTF-8"
      if (!is.na(pattern) && length(fid)!=0){
          tryCatch(SearchFiles(sprintf("(%s) and id in (%s)",pattern,paste(shQuote(fid),collapse=",")),
                               Widget=".FileofCat",is.UTF8=TRUE),
                   error=function(e) gmessage("Error~~~."),con=TRUE)
          assign("lastsearch",pattern,env=.rqda)
      }
  }
}
FileofCatWidgetMenu$"Delete selected File(s)"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    SelectedFile <- svalue(.rqda$.FileofCat)
    Encoding(SelectedFile) <- "UTF-8"
    for (i in SelectedFile){
      i <- enc(i)
      fid <- dbGetQuery(.rqda$qdacon, sprintf("select id from source where name='%s'",i))$id
      dbGetQuery(.rqda$qdacon, sprintf("update source set status=0 where name='%s'",i))
      dbGetQuery(.rqda$qdacon, sprintf("update caselinkage set status=0 where fid=%i",fid))
      dbGetQuery(.rqda$qdacon, sprintf("update treefile set status=0 where fid=%i",fid))
      dbGetQuery(.rqda$qdacon, sprintf("update coding set status=0 where fid=%i",fid))
    }
    ## UpdateFileofCatWidget()
    ## .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[],SelectedFile)
    UpdateWidget(".FileofCat",from=SelectedFile,to=NULL)
  }
}
FileofCatWidgetMenu$"Rename selected File"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    selectedFN <- svalue(.rqda$.FileofCat)
    if (length(selectedFN)==0){
      gmessage("Select a file first.",icon="error",con=TRUE)
    }
    else {
      NewFileName <- ginput("Enter new file name. ",text=selectedFN, icon="info")
      if (!is.na(NewFileName)) {
        Encoding(NewFileName) <- "UTF-8"
        rename(selectedFN,NewFileName,"source")
        ## UpdateFileofCatWidget()
        Fnames <- .rqda$.FileofCat[]
        Fnames[Fnames==selectedFN] <- NewFileName
        .rqda$.FileofCat[] <- Fnames
      }
    }}}
## FileofCatWidgetMenu$"Show ..."$"Show Uncoded Files Only Sorted By Imported Time"$handler <- function(h,...){
##  if (is_projOpen(env=.rqda,conName="qdacon")) {
##    fid <- GetFileId(condition="filecategory",type="uncoded")
##    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
##  }
## }
## FileofCatWidgetMenu$"Show ..."$"Show Coded Files Only Sorted By Imported Time"$handler <- function(h,...){
##   if (is_projOpen(env=.rqda,conName="qdacon")) {
##     fid <- GetFileId(condition="filecategory",type="coded")
##     FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
##   }
## }
FileofCatWidgetMenu$"Show ..."$"Show All By Imported Time"$handler <- function(h,...)
{
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    fid <- GetFileId(condition="filecategory",type="all")
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
  }
}
FileofCatWidgetMenu$"Show ..."$"Show Coded Files Sorted by Imported time"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=GetFileId(condition="filecat",type="coded"))
  }
}
FileofCatWidgetMenu$"Show ..."$"Show Uncoded Files Sorted by Imported time"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    ## UncodedFileNamesUpdate(FileNamesWidget = .rqda$.fnames_rqda)
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=GetFileId(condition="filecat",type="uncoded"))
    ## By default, the file names in the widget will be sorted.
  }
}
FileofCatWidgetMenu$"Show Selected File Property"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    ShowFileProperty(Fid=GetFileId("filecat","selected"))
}}

