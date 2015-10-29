AddCaseButton <- function(label="ADD"){
  AddCasB <- gbutton(label,handler=function(h,...) {
    CaseName <- ginput("Enter new Case Name. ", icon="info")
    if (!is.na(CaseName)) {
      Encoding(CaseName) <- "UTF-8"
      AddCase(CaseName)
      CaseNamesUpdate()
      enabled(button$profmatB) <- TRUE
      idx <- as.character(which(.rqda$.CasesNamesWidget[] %in%  CaseName) -1)
      ## note the position, before manipulation of items
      path <-gtkTreePathNewFromString(idx)
      gtkTreeViewScrollToCell(slot(slot(.rqda$.CasesNamesWidget,"widget"),"widget"),
                              path,use.align=TRUE,row.align = 0.05)
    }
  }
                     )
  assign("AddCasB",AddCasB,envir=button)
  enabled(AddCasB) <- FALSE
  AddCasB
}


DeleteCaseButton <- function(label="Delete"){
  DelCasB <- gbutton(label, handler=function(h,...) {
    del <- gconfirm("Really delete the Case?",icon="question")
    if (isTRUE(del)){
      SelectedCase <- svalue(.rqda$.CasesNamesWidget)
      Encoding(SelectedCase) <- "UTF-8"
      caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where name='%s'",
                                                enc(SelectedCase)))$id
      dbGetQuery(.rqda$qdacon,sprintf("update cases set status=0 where name='%s'",
                                      enc(SelectedCase)))
      ## set status in table freecode to 0
      dbGetQuery(.rqda$qdacon,sprintf("update caselinkage set status=0 where caseid=%i",caseid))
      ## set status in table caselinkage to 0
      CaseNamesUpdate()
      .rqda$.FileofCase[] <- NULL
    }
  }
                     )
  assign("DelCasB",DelCasB,envir=button)
  enabled(DelCasB) <- FALSE
  DelCasB
}


Case_RenameButton <- function(label="Rename",CaseNamesWidget=.rqda$.CasesNamesWidget,...)
{
  ## rename of selected case.
  CasRenB <- gbutton(label,handler=function(h,...) {
    selectedCaseName <- svalue(CaseNamesWidget)
    ## get the new file names
    NewName <- ginput("Enter new Case name. ", text=selectedCaseName, icon="info")
    if (!is.na(NewName)){
      rename(selectedCaseName,NewName,"cases")
      CaseNamesUpdate()
    }
  }
                     )
  assign("CasRenB",CasRenB,envir=button)
  enabled(CasRenB) <- FALSE
  CasRenB
}


CaseMark_Button<-function(label="Mark"){
  CasMarB <- gbutton(label,handler=function(h,...) {
    MarkCaseFun()
    UpdateFileofCaseWidget()
  }
                     )
  assign("CasMarB",CasMarB,envir=button)
  enabled(CasMarB) <- FALSE
  CasMarB
}

MarkCaseFun <- function(){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
    con <- .rqda$qdacon
    tryCatch({
      ans <- mark(get(".openfile_gui",envir=.rqda),fore.col=NULL,back.col=.rqda$back.col,addButton=FALSE)
      if (ans$start != ans$end){
        ## when selected no text, makes on sense to do anything.
        SelectedCase <- svalue(.rqda$.CasesNamesWidget)
        SelectedCase <- enc(SelectedCase,encoding="UTF-8")
        currentCid <-  dbGetQuery(con,sprintf("select id from cases where name='%s'",
                                              SelectedCase))[,1]
        SelectedFile <- svalue(.rqda$.root_edit)
        ##Encoding(SelectedFile) <- "UTF-8"
        SelectedFile <- enc(SelectedFile,encoding="UTF-8")
        currentFid <-  dbGetQuery(con,sprintf("select id from source where name='%s'",
                                              SelectedFile))[,1]
        ## Query of caselinkage
        ExistLinkage <-  dbGetQuery(con,sprintf("select rowid, selfirst, selend,status from caselinkage where caseid=%i and fid=%i and status=1",currentCid,currentFid))
        DAT <- data.frame(cid=currentCid,fid=currentFid,
                          selfirst=ans$start,selend=ans$end,status=1,
                          owner=.rqda$owner,date=date(),memo="")
        if (nrow(ExistLinkage)==0){
          ## if there are no relevant caselinkage, write the caselinkage table
          success <- dbWriteTable(.rqda$qdacon,"caselinkage",DAT,row.name=FALSE,append=TRUE)
          if (!success) gmessage("Fail to write to database.")
        } else {
          Relations <- apply(ExistLinkage,1,FUN=function(x) relation(x[c("selfirst","selend")],c(ans$start,ans$end)))
          ExistLinkage$Relation <- sapply(Relations,FUN=function(x)x$Relation)
          if (!any(ExistLinkage$Relation=="exact")){
            ## if there are exact caselinkage, skip; if no exact linkage then continue
            ExistLinkage$WhichMin <- sapply(Relations,FUN=function(x)x$WhichMin)
            ExistLinkage$Start <- sapply(Relations,FUN=function(x)x$UnionIndex[1])
            ExistLinkage$End <- sapply(Relations,FUN=function(x)x$UnionIndex[2])
            if (all(ExistLinkage$Relation=="proximity")){
              success <- dbWriteTable(.rqda$qdacon,"caselinkage",DAT,row.name=FALSE,append=TRUE)
              if (!success) gmessage("Fail to write to database.")
            } else {
              del1 <- ExistLinkage$WhichMin==2 & ExistLinkage$Relation =="inclusion"; del1[is.na(del1)] <- FALSE
              del2 <- ExistLinkage$Relation =="overlap"; del2[is.na(del2)] <- FALSE
              del <- (del1 | del2)
              if (any(del)){
              Sel <- c(min(ExistLinkage$Start[del]), max(ExistLinkage$End[del]))
              memo <- dbGetQuery(.rqda$qdacon,sprintf("select memo from caselinkage where rowid in (%s)",
                                                      paste(ExistLinkage$rowid[del],collapse=",",sep="")))$memo
              memo <- paste(memo,collapse="",sep="")
              dbGetQuery(.rqda$qdacon,sprintf("delete from caselinkage where rowid in (%s)",
                                              paste(ExistLinkage$rowid[del],collapse=",",sep="")))
              DAT <- data.frame(cid=currentCid,fid=currentFid,
                                selfirst=Sel[1],selend=Sel[2],status=1,
                                owner=.rqda$owner,date=date(),memo=memo)
              success <- dbWriteTable(.rqda$qdacon,"caselinkage",DAT,row.name=FALSE,append=TRUE)
              if (!success) gmessage("Fail to write to database.")
            }
            }
            }
          }
        }
    },error=function(e){}
             )
  }
}


CaseUnMark_Button<-function(label="Unmark"){
  CasUnMarB <- gbutton(label, handler=function(h,...) {
    con <- .rqda$qdacon
    W <- .rqda$.openfile_gui
    ## get the widget for file display. If it does not exist, then return NULL.
    sel_index <- tryCatch(sindex(W,includeAnchor=FALSE),error=function(e) {})
    ## if the not file is open, unmark doesn't work.
    if (!is.null(sel_index)) {
      SelectedCase <- svalue(.rqda$.CasesNamesWidget)
      if (length(SelectedCase)==0) {gmessage("Select a case first.",con=TRUE)} else{
        SelectedCase <- enc(SelectedCase,"UTF-8")
        caseid <-  dbGetQuery(.rqda$qdacon,sprintf("select id from cases where name='%s'",SelectedCase))[,1]
        SelectedFile <- svalue(.rqda$.root_edit)
        SelectedFile <- enc(SelectedFile,"UTF-8")
        currentFid <-  dbGetQuery(con,sprintf("select id from source where name='%s'", SelectedFile))[,1]
        codings_index <-  dbGetQuery(con,sprintf("select rowid, caseid, fid, selfirst, selend from caselinkage where caseid=%i and fid=%i", caseid, currentFid))
        ## should only work with those related to current case and current file.
        rowid <- codings_index$rowid[(codings_index$selfirst  >= sel_index$startN) &
                                     (codings_index$selend  <= sel_index$endN)]
        if (is.numeric(rowid)) for (j in rowid) {
          dbGetQuery(con,sprintf("update caselinkage set status=0 where rowid=%i", j))
        }
        coding.idx <- RQDAQuery(sprintf("select selfirst,selend from coding where fid=%i and status=1",currentFid))
        anno.idx <- RQDAQuery(sprintf("select position from annotation where fid=%i and status=1",currentFid))$position
        allidx <- unlist(coding.idx,anno.idx)
        if (!is.null(allidx)){
          startN<- sel_index$startN +  sum(allidx <= sel_index$startN)
          endN <- sel_index$endN +  sum(allidx <= sel_index$endN)
        }
        ## better to get around the loop by sqlite condition expression.
        ClearMark(W,min=startN,max=endN,clear.fore.col = FALSE, clear.back.col = TRUE)
        ## even for the non-current code. can improve.
      }
    }
    UpdateFileofCaseWidget()
  }
                       )
  assign("CasUnMarB",CasUnMarB,envir=button)
  enabled(CasUnMarB) <- FALSE
  CasUnMarB
}

CaseAttribute_Button <- function(label="Attribute"){
    CasAttrB <- gbutton(text=label, handler = function(h, ...) {
        SelectedCase <- svalue(.rqda$.CasesNamesWidget)
        if (length(SelectedCase!=0)){
            SelectedCase <- enc(SelectedCase,"UTF-8")
            caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",SelectedCase))[,1]
            CaseAttrFun(caseId=caseid,title=SelectedCase)
        }})
     assign("CasAttrB", CasAttrB, envir=button)
     enabled(button$CasAttrB) <- FALSE
     CasAttrB
}

prof_mat_Button <- function(label="prof_mat"){
  profmatB <- gbutton(text=label, handler = function(h, ...) {
    prof_mat()
    })
  assign("profmatB", profmatB, envir=button)
  profmatB
}

CaseNamesWidgetMenu <- list()
CaseNamesWidgetMenu$"Add File(s)"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    SelectedCase <- svalue(.rqda$.CasesNamesWidget)
    SelectedCase <- enc(SelectedCase,"UTF-8")
    caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",SelectedCase))[,1]
    freefile <-  dbGetQuery(.rqda$qdacon,"select name, id, file from source where status=1")
    fileofcase <- dbGetQuery(.rqda$qdacon,sprintf("select fid from caselinkage where status=1 and caseid=%i",caseid))
    Encoding(freefile[['name']]) <- Encoding(freefile[['file']]) <- "UTF-8"
    if (nrow(fileofcase)!=0){
      fileoutofcase <- subset(freefile,!(id %in% fileofcase$fid))
      } else  fileoutofcase <- freefile
    if (length(fileoutofcase[['name']])==0) gmessage("All files are linked with this case.", cont=TRUE) else {
      ##Selected <- select.list(fileoutofcase[['name']],multiple=TRUE)
    CurrentFrame <- sys.frame(sys.nframe())
    ## sys.frame(): get the frame of n
    ## nframe(): get n of current frame
    ## The value of them depends on where they evaluated, should not placed inside RunOnSelected()
    RunOnSelected(fileoutofcase[['name']],multiple=TRUE,enclos=CurrentFrame,expr={
      if (length(Selected)> 0) {
        Encoding(Selected) <- "UTF-8"
        fid <- fileoutofcase[fileoutofcase$name %in% Selected,"id"]
        selend <- nchar(fileoutofcase[fileoutofcase$name %in% Selected,"file"])
        Dat <- data.frame(caseid=caseid,fid=fid,selfirst=0,selend=selend,status=1,owner=.rqda$owner,date=date(),memo=NA)
        dbWriteTable(.rqda$qdacon,"caselinkage",Dat,row.names=FALSE,append=TRUE)
        UpdateFileofCaseWidget()
      }})
  }
  }
}

CaseNamesWidgetMenu$"Add New File to Selected Case"$handler <- function(h, ...) {
  AddNewFileFunOfCase()
}

CaseNamesWidgetMenu$"Case Memo"$handler <- function(h,...){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
    MemoWidget("Case",.rqda$.CasesNamesWidget,"cases")
    ## see CodeCatButton.R  for definition of MemoWidget
  }
}
CaseNamesWidgetMenu$"Show Cases with Memo Only"$handler <- function(h,...){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
   cnames <- RQDAQuery("select name from cases where memo is not null")$name
   if (!is.null(cnames)) cnames <- enc(cnames,"UTF-8")
   .rqda$.CasesNamesWidget[] <- cnames
  }
}
CaseNamesWidgetMenu$"Add/modify Attributes..."$handler <- function(h,...){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
    SelectedCase <- svalue(.rqda$.CasesNamesWidget)
    if (length(SelectedCase!=0)){
    SelectedCase <- enc(SelectedCase,"UTF-8")
    caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",SelectedCase))[,1]
    CaseAttrFun(caseId=caseid,title=SelectedCase)
  }
}}
CaseNamesWidgetMenu$"View Attributes"$handler <- function(h,...){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
   viewCaseAttr()
  }
}
CaseNamesWidgetMenu$"Export Case Attributes"$handler <- function(h,...){
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        fName <- gfile(type='save',filter=list("csv"=list(pattern=c("*.csv"))))
        Encoding(fName) <- "UTF-8"
        if (length(grep(".csv$",fName))==0) fName <- sprintf("%s.csv",fName)
        write.csv(GetAttr("case"), row.names=FALSE, file=fName, na="")
    }
}
CaseNamesWidgetMenu$"Sort All by Created Time"$handler <- function(h,...){
  CaseNamesUpdate(.rqda$.CasesNamesWidget,sortByTime = TRUE)
}
CaseNamesWidgetMenu$"Web Search"$Google$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8")
    browseURL(sprintf("http://www.google.com/search?q=%s",KeyWord))
  }
}
CaseNamesWidgetMenu$"Web Search"$Yahoo$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8")
    browseURL(sprintf("http://search.yahoo.com/search;_ylt=A0oGkmFV.CZJNssAOK.l87UF?p=%s&ei=UTF-8&iscqry=&fr=sfp&fr2=sfp"
                      ,KeyWord))
  }
}
CaseNamesWidgetMenu$"Web Search"$Baidu$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8",to="CP936") ## should be in CP936 to work properly.
    browseURL(sprintf("http://www.baidu.com/s?wd=%s",paste("%",paste(charToRaw(KeyWord),sep="",collapse="%"),sep="",collapse="")))
  }
}
CaseNamesWidgetMenu$"Web Search"$Sogou$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8",to="CP936")## should be in CP936 to work properly.
    browseURL(sprintf("http://www.sogou.com/sohu?query=%s",paste("%",paste(charToRaw(KeyWord),sep="",collapse="%"),sep="",collapse="")))
  }
}


## pop-up menu of .rqda$.FileofCase
FileofCaseWidgetMenu <- list() ## not used yet.
FileofCaseWidgetMenu$"Add To File Category ..."$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    AddToFileCategory(Widget=.rqda$.FileofCase,updateWidget=FALSE)
  }
}
FileofCaseWidgetMenu$"Drop Selected File(s)"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    FileOfCat <- svalue(.rqda$.FileofCase)
    if ((NumofSelected <- length(FileOfCat)) ==0) {
      gmessage("Please select the Files you want to delete.",con=TRUE)} else
    {
      ## Give a confirm msg
      del <- gconfirm(sprintf("Delete %i file(s) from this category. Are you sure?",NumofSelected),con=TRUE,icon="question")
      if (isTRUE(del)){
        SelectedCase <- svalue(.rqda$.CasesNamesWidget)
        ## Encoding(SelectedCase) <- Encoding(FileOfCat)<- "UTF-8"
        SelectedCase <- enc(SelectedCase,"UTF-8")
        FileOfCat <- enc(FileOfCat,"UTF-8")
        caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",SelectedCase))[,1]
        for (i in FileOfCat){
          fid <- dbGetQuery(.rqda$qdacon,sprintf("select id from source where status=1 and name='%s'",i))[,1]
          dbGetQuery(.rqda$qdacon,sprintf("update caselinkage set status=0 where caseid=%i and fid=%i",caseid,fid))
        }
        ## update Widget
        UpdateFileofCaseWidget()
      }
    }
  }
}
FileofCaseWidgetMenu$"Delete Selected File(s)"$handler <- function(h,...){
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        SelectedFile <- svalue(.rqda$.FileofCase)
        Encoding(SelectedFile) <- "UTF-8"
        for (i in SelectedFile){
            fid <- dbGetQuery(.rqda$qdacon, sprintf("select id from source where name='%s'",i))$id
            dbGetQuery(.rqda$qdacon, sprintf("update source set status=0 where name='%s'",i))
            dbGetQuery(.rqda$qdacon, sprintf("update caselinkage set status=0 where fid=%i",fid))
            dbGetQuery(.rqda$qdacon, sprintf("update treefile set status=0 where fid=%i",fid))
            dbGetQuery(.rqda$qdacon, sprintf("update coding set status=0 where fid=%i",fid))
        }
        .rqda$.FileofCase[] <- setdiff(.rqda$.FileofCase[],SelectedFile)
    }
}
FileofCaseWidgetMenu$"Edit Selected File"$handler <- function(h,...){
  EditFileFun(FileNameWidget=.rqda$.FileofCase)
}
FileofCaseWidgetMenu$"File Memo"$handler <- function(h,...){
  MemoWidget("File",.rqda$.FileofCase,"source")
}
FileofCaseWidgetMenu$"Rename selected File"$handler <- function(h,...){
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        selectedFN <- svalue(.rqda$.FileofCase)
        if (length(selectedFN)==0){
            gmessage("Select a file first.",icon="error",con=TRUE)
        }
        else {
            NewFileName <- ginput("Enter new file name. ",text=selectedFN, icon="info")
            if (!is.na(NewFileName)) {
                Encoding(NewFileName) <- "UTF-8"
                rename(selectedFN,NewFileName,"source")
                Fnames <- .rqda$.FileofCase[]
                Fnames[Fnames==selectedFN] <- NewFileName
                .rqda$.FileofCase[] <- Fnames
            }
        }}}
FileofCaseWidgetMenu$"Search Files within Seleted Case"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
        pattern <- ifelse(is.null(.rqda$lastsearch),"file like '%%'",.rqda$lastsearch)
        pattern <- ginput("Please input a search pattern.",text=pattern)
        if (!is.na(pattern)){
            Fid <- GetFileId("case")
            tryCatch(SearchFiles(pattern,Fid=Fid,Widget=".FileofCase",is.UTF8=TRUE),error=function(e) gmessage("Error~~~."),con=TRUE)
            assign("lastsearch",pattern,envir=.rqda)
        }
    }
}
FileofCaseWidgetMenu$"Show ..."$"Show All by Sorted by Imported Time"$handler <- function(h,...){
  ## UpdateFileofCaseWidget()
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
    fid <- GetFileId(condition="case",type="all")
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCase,FileId=fid)
  }
}
FileofCaseWidgetMenu$"Show ..."$"Show Coded Files Only (sorted)"$handler <- function(h,...){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
    fid <- GetFileId(condition="case",type="coded")
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCase,FileId=fid)
  }
}
FileofCaseWidgetMenu$"Show ..."$"Show Uncoded Files Only (sorted)"$handler <- function(h,...){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
    fid <- GetFileId(condition="case",type="uncoded")
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCase,FileId=fid)
  }
}
FileofCaseWidgetMenu$"Show Selected File Property"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    ShowFileProperty(Fid=GetFileId("case","selected"))
    }
}

####################
## Defunct functions
####################

##   AddWebSearchButton <- function(label="WebSearch",CaseNamesWidget=.rqda$.CasesNamesWidget){
##     gbutton(label,handler=function(h,...) {
##       if (is_projOpen(envir=.rqda,conName="qdacon")) {
##         KeyWord <- svalue(CaseNamesWidget)
##         engine <- select.list(c("Baidu","Google","Yahoo"))
##         if (engine=="Baidu") {
##           KeyWord <- iconv(KeyWord, from="UTF-8")
##           browseURL(sprintf("http://www.baidu.com/s?wd=%s",paste("%",paste(charToRaw(KeyWord),sep="",collapse="%"),sep="",collapse="")))
##         }
##         if (engine=="Yahoo") {
##           KeyWord <- iconv(KeyWord, from="UTF-8")
##           browseURL(sprintf("http://search.yahoo.com/search;_ylt=A0oGkmFV.CZJNssAOK.l87UF?p=%s&ei=UTF-8&iscqry=&fr=sfp&fr2=sfp"
##                             ,KeyWord))
##         }
##     if (engine=="Google")browseURL(sprintf("http://www.google.com/search?q=%s",KeyWord))
##       }
##     }
##             )
## }


## CaseMemoButton <- function(label="Memo",...){
## ## no longer used
##   gbutton(label, handler=function(h,...) {
##     ## code memo: such as meaning of code etc.
##     if (is_projOpen(envir=.rqda,"qdacon")) {
##       currentCase <- svalue(.rqda$.CasesNamesWidget)
##       if (length(currentCase)==0){
##         gmessage("Select a Case first.",icon="error",con=TRUE)
##       }
##       else {
##         tryCatch(dispose(.rqda$.casememo),error=function(e) {})
##         assign(".casememo",gwindow(title=paste("Case Memo",.rqda$currentCase,sep=":"),
##                                    parent=c(370,10),width=600,height=400),envir=.rqda)
##         .casememo <- .rqda$.casememo
##         .casememo2 <- gpanedgroup(horizontal = FALSE, con=.casememo)
##         currentCase <- enc(currentCase, encoding="UTF-8")
##         gbutton("Save Case Memo",con=.casememo2,handler=function(h,...){
##           newcontent <- svalue(W)
##           ## Encoding(newcontent) <- "UTF-8"
##           newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
##           ## Encoding(currentCase) <- "UTF-8"
##           dbGetQuery(.rqda$qdacon,sprintf("update cases set memo='%s' where name='%s'",newcontent,currentCase))
##         }
##                 )## end of save memo button
##         assign(".casememoW",gtext(container=.casememo2,font.attr=c(sizes="large")),envir=.rqda)
##         prvcontent <- dbGetQuery(.rqda$qdacon, sprintf("select memo from cases where name='%s'",currentCase))[1,1]
##         if (is.na(prvcontent)) prvcontent <- ""
##         Encoding(prvcontent) <- "UTF-8"
##         W <- .rqda$.casememoW
##         add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
##       }
##     }
##   }
##           )
## }
