DefaultCodeColor <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99")

rename <- function(from,to,table=c("source","freecode","cases","codecat","filecat","journal")){
  ## rename name field in table source and freecode (other tables can be added futher)
  ## source is the file name, freecode is the free code name
  table <- match.arg(table)
  if (to!=""){ ## if to is "", makes no sense to rename
      exists <- dbGetQuery(.rqda$qdacon, sprintf("select * from %s where name = '%s' ",table, enc(to)))
      ## should check it there is any dupliation in the table
      if (nrow(exists) > 0) {
          gmessage("The new name is duplicated. Please use another new name.",container=TRUE)
      } else {
          dbGetQuery(.rqda$qdacon, sprintf("update '%s' set name = '%s' where name = '%s' ",table, enc(to), enc(from)))
      }
  }
}

UpdateWidget <- function(widget,from,to=NULL){
  ## widget is character of length 1.
  items <- eval(parse(text=sprintf(".rqda$%s[]",widget)))
  if (length(items)!= 0){
    Encoding(items) <- "UTF-8"
    idx <- as.character(which(items %in%  from[1])) ## note the position, before manipulation of items
    if (is.null(to)) {
      items <- items[! items %in% from]
    } else {
      if (length(from) == length(to))
        items[items %in% from] <- to
    }
    ## eval(parse(text=sprintf(".rqda$%s[] <- items",widget)))
    tryCatch(eval(parse(text = sprintf(".rqda$%s[] <- items", widget))),
             error = function(e) cat("warning msg from the replacement.\n"))
    if (length(idx)>0) {
    path <-gtkTreePathNewFromString(idx)
    gtkTreeViewScrollToCell(slot(slot(get(widget,envir=.rqda),"widget"),"widget"),
                            path,use.align=TRUE,row.align = 0.07)
  }}
}

ScrollToItem <- function(widget,item=svalue(widget)){
  items <- widget[]
  if (length(items)!= 0){
    Encoding(items) <- "UTF-8"
    idx <- as.character(which(items %in% item) - 1)
    if (length(idx)!=0){
      path <-gtkTreePathNewFromString(idx)
      gtkTreeViewScrollToCell(slot(slot(widget,"widget"),"widget"), path,use.align=TRUE,row.align = 0.07)
    }}}

enc <- function(x,encoding="UTF-8") {
  ## replace " with two '. to make insert smoothly.
  ## encoding is the encoding of x (character vector).
  Encoding(x) <- encoding
  x <- gsub("'", "''", x)
  if (all(Encoding(x)!="UTF-8")) {
    x <- iconv(x,to="UTF-8")
  }
  x
}

OrderByTime <- function(date,decreasing = FALSE)
{
  ## return tbe permutation of the date which is get by sql "select date from ..."
  ## see order for the meaning of permutation. It can be used as index to sort vector or date frame
  ##   if (getRversion()<"2.8.0"){
  ##     permutation <- ifelse(decreasing,1:length(date),length(date):1)
  ##     ## should rewrite it when project merge is provided.
  ##   } else{
  ## Work for R.2.8.0 or above for Dateclass,so convert to character
  oldLCTIME<- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","C")
  on.exit(Sys.setlocale("LC_TIME",oldLCTIME))
  Newdate <- as.character(strptime(date, "%a %b %d %H:%M:%S %Y"))
  permutation <- order(Newdate,decreasing = decreasing)
  ##  }
}
## dd<- dbGetQuery(.rqda$qdacon,"select date from source")$date
## sort(dd) == dd[order(dd)] ## but the order is not correct.
## dd[OrderByTime(dd)]


MemoWidget <- function(prefix,widget,dbTable){
  ## prefix of window tile. E.g. "Code" ->  tile of gwindow becomes "Code Memo:"
  ## widget of the F-cat/C-cat list, such as widget=.rqda$.fnames_rqda
  if (is_projOpen(envir=.rqda,"qdacon")) {
      Selected <- svalue(widget)
      if (length(Selected)==0){
        gmessage("Select first.",icon="error",container=TRUE)
      }
      else {
          CloseYes <- function(currentCode){
              withinWidget <- svalue(get(sprintf(".%smemoW",prefix),envir=.rqda))
              InRQDA <- dbGetQuery(.rqda$qdacon, sprintf("select memo from %s where name='%s'",dbTable, enc(currentCode,"UTF-8")))[1, 1]
              if (isTRUE(all.equal(withinWidget,InRQDA))) {
                  return(TRUE) } else {
                      if (is.na(InRQDA) && withinWidget=="")  {
                          return(TRUE) } else {
                      val <- gconfirm("The memo has been change, Close anyway?",container=TRUE)
                  }
                      return(val)
                  }
          } ## helper function
          IsOpen <- tryCatch(eval(parse(text=sprintf("svalue(.rqda$.%smemoW)",prefix))),error=function(e) simpleError("No opened memo widget."))
          if (!inherits(IsOpen,"simpleError")){ ## if a widget is open
              prvSelected <- svalue(get(sprintf(".%smemo",prefix),envir=.rqda)) ## title of the memo widget
              Encoding(prvSelected) <- "UTF-8"
              prvSelected <- sub(sprintf("^%s Memo: ",prefix),"",prvSelected)
              prvSelected <- iconv(prvSelected,to="UTF-8") ## previously selected codename
              IfCont <- CloseYes(currentCode=prvSelected)}
          if ( inherits(IsOpen,"simpleError") || IfCont){ ## if not open or the same.
              tryCatch(eval(parse(text=sprintf("dispose(.rqda$.%smemo)",prefix))),error=function(e) {})
              gw <- gwindow(title=sprintf("%s Memo:%s",prefix,Selected),
                            parent=getOption("widgetCoordinate"),
                            width = getOption("widgetSize")[1],
                            height = getOption("widgetSize")[2]
                            )
              mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
              gw@widget@widget$SetIconFromFile(mainIcon)
              assign(sprintf(".%smemo",prefix),gw,envir=.rqda)
              assign(sprintf(".%smemo2",prefix),
                     gpanedgroup(horizontal = FALSE, container=get(sprintf(".%smemo",prefix),envir=.rqda)),
                     envir=.rqda)
              mbut <- gbutton("Save Memo",container=get(sprintf(".%smemo2",prefix),envir=.rqda),handler=function(h,...){
                  newcontent <- svalue(W)
                  newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
                  dbGetQuery(.rqda$qdacon,sprintf("update %s set memo='%s' where name='%s'",dbTable,newcontent,enc(Selected)))
                  mbut <- get(sprintf("buttonOf.%smemo",prefix),envir=button)
                  enabled(mbut) <- FALSE
              }
                              )## end of save memo button
              enabled(mbut) <- FALSE
              assign(sprintf("buttonOf.%smemo",prefix),mbut,envir=button) ## assign the button object
              tmp <- gtext(container=get(sprintf(".%smemo2",prefix),envir=.rqda))
              font <- pangoFontDescriptionFromString(.rqda$font)
              gtkWidgetModifyFont(tmp@widget@widget,font)## set the default fontsize
              assign(sprintf(".%smemoW",prefix),tmp,envir=.rqda)
              prvcontent <- dbGetQuery(.rqda$qdacon, sprintf("select memo from %s where name='%s'",dbTable,enc(Selected)))[1,1]
              if (is.na(prvcontent)) prvcontent <- ""
              Encoding(prvcontent) <- "UTF-8"
              W <- get(sprintf(".%smemoW",prefix),envir=.rqda)
              add(W,prvcontent,do.newline=FALSE)
              addHandlerUnrealize(get(sprintf(".%smemo",prefix),envir=.rqda),handler <- function(h,...)  {!CloseYes(Selected)})
              gSignalConnect(tmp@widget@widget$GetBuffer(), "changed", function(h,...) {
                  mbut <- get(sprintf("buttonOf.%smemo",prefix),envir=button)
                  enabled(mbut) <- TRUE
              }
                             )##
          }
      }
  }
}

getAnnos <- function(type="file"){
    annos <- RQDAQuery("select annotation.rowid, source.id, source.name, annotation.annotation,annotation.date from annotation join source on annotation.fid=source.id where  annotation.status==1 and annotation not in ('NA','')")
    if (nrow(annos)>0){
        Encoding(annos$annotation) <- "UTF-8"
        Encoding(annos$name) <- "UTF-8"
    }
    attr(annos,"field.name") <- "annotation"
    attr(annos,"descr") <- sprintf("%i %s", nrow(annos), ngettext(nrow(annos),"annotation","annotations"))
    class(annos) <- c("annotations","Info4Widget", "data.frame")
    annos
}

getMemos <- function(type="codes"){
    memos <- RQDAQuery("select memo, name, id, date, dateM from freecode where status==1 and memo not in ('NA','')")
    if (nrow(memos)>0){
        Encoding(memos$memo) <- "UTF-8"
        Encoding(memos$name) <- "UTF-8"
    }
    class(memos) <- c("memos","Info4Widget","data.frame")
    attr(memos,"field.name") <- "memo"
    attr(memos,"descr") <- sprintf("%i code %s", nrow(memos), ngettext(nrow(memos),"memo","memos"))
    memos
}

print.Info4Widget <- function(x, ...){
    ComputeCallbackFun <- function(FileName, rowid) {
        CallBackFUN <- function(widget, event, ...) {
            ViewFileFunHelper(FileName, hightlight = FALSE)
            textView <- .rqda$.openfile_gui@widget@widget
            buffer <- textView$GetBuffer()
            mark1 <- gtkTextBufferGetMark(buffer, sprintf("%s.1",
                rowid))
            gtkTextViewScrollToMark(textView, mark1, 0)
            iter1 <- buffer$GetIterAtMark(mark1)$iter
            idx1 <- gtkTextIterGetOffset(iter1)
            mark2 <- buffer$GetMark(sprintf("%s.2", rowid))
            gtkTextMarkSetVisible(mark2, TRUE)
            iter2 <- buffer$GetIterAtMark(mark2)$iter
            idx2 <- gtkTextIterGetOffset(iter2)
            HL(.rqda$.openfile_gui, data.frame(idx1, idx2), fore.col = .rqda$fore.col,
               back.col = NULL)
        }
        CallBackFUN
    }
    if (nrow(x) == 0)
        gmessage("No Information is collected.", container = TRUE)
    else {
        field.name <-attr(x,"field.name")
        .gw <- gwindow(title = attr(x,"descr"), parent = getOption("widgetCoordinate"),
                       width = getOption("widgetSize")[1], height = getOption("widgetSize")[2])
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        .gw@widget@widget$SetIconFromFile(mainIcon)
        ## assign(sprintf(".codingsOf%s", "codingsByone"), .gw, env = .rqda)
        .retreivalgui <- gtext(container = .gw)
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(.retreivalgui@widget@widget, font)
        .retreivalgui@widget@widget$SetPixelsBelowLines(5)
        .retreivalgui@widget@widget$SetPixelsInsideWrap(5)
        buffer <- .retreivalgui@widget@widget$GetBuffer()
        buffer$createTag("red", foreground = "red")
        iter <- buffer$getIterAtOffset(0)$iter
        apply(x, 1, function(x) {
            metaData <- sprintf("%s created on %s", x[["name"]], x[["date"]])
            buffer$InsertWithTagsByName(iter, metaData, "red")
            ## anchorcreated <- buffer$createChildAnchor(iter)
            ## iter$BackwardChar()
            ## anchor <- iter$getChildAnchor()
            ## lab <- gtkLabelNew("Back")
            ## widget <- gtkEventBoxNew()
            ## widget$Add(lab)
            ## gSignalConnect(widget, "button-press-event", ComputeCallbackFun(x[["filename"]],
            ##    as.numeric(x[["rowid"]])))
            ## .retreivalgui@widget@widget$addChildAtAnchor(widget, anchor)
            ## widget$showAll()
            iter$ForwardChar()
            buffer$insert(iter, "\n")
            buffer$InsertWithTagsByName(iter, x[[field.name]])
            buffer$insert(iter, "\n\n")
        })
        buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter)
    }
}


## summary coding information
GetCodingTable <- function(){
  ## test when any table is empty
  ## http://archives.postgresql.org/pgsql-sql/2004-01/msg00160.php
  if ( is_projOpen()) {
   ## Codings <- dbGetQuery(.rqda$qdacon,"select freecode.name as codename, freecode.id as cid,
   ##         coding.cid as cid2,coding.fid as fid,source.id as fid2, source.name as filename,
   ##         coding.selend - coding.selfirst as CodingLength,coding.selend, coding.selfirst
   ##         from coding, freecode, source
   ##         where coding.status==1 and freecode.id=coding.cid and coding.fid=source.id")
   Codings <- dbGetQuery(.rqda$qdacon,"select coding.rowid as rowid, coding.cid, coding.fid, freecode.name as codename, source.name as filename,
                                       coding.selfirst as index1, coding.selend as index2,
                                       coding.selend - coding.selfirst as CodingLength
                                      from coding left join freecode on (coding.cid=freecode.id)
                                                  left join source on (coding.fid=source.id)
                                      where coding.status==1 and source.status=1 and freecode.status=1")

    if (nrow(Codings)!=0){
      Encoding(Codings$codename) <- Encoding(Codings$filename) <- "UTF-8"
    }
   ## if (!all (all.equal(Codings$cid,Codings$cid2),all.equal(Codings$fid,Codings$fid2))){
   ##   stop("Errors!") ## check to make sure the sql is correct
   ## }
    class(Codings) <- c("codingTable","data.frame")
    Codings
  } else cat("Open a project first.\n")
}

summaryCodings <-
SummaryCoding <- function(byFile=FALSE,...){
  if ( is_projOpen() ) {
    Codings <- GetCodingTable()
    if (nrow(Codings)>0){
      NumOfCoding <- table(Codings$codename,...) ## how many coding for each code
      AvgLength <- tapply(Codings$CodingLength,Codings$codename,FUN=mean,...) # Average of words for each code
      NumOfFile <- tapply(Codings$fid,Codings$codename,FUN=function(ii)length(unique(ii))) # Number of files for each code
      if (byFile){
        CodingOfFile <- tapply(Codings$codename,Codings$filename,FUN=table,...) # summary of codings for each file
      } else CodingOfFile <- NULL
      ans <- list(NumOfCoding=NumOfCoding,AvgLength=AvgLength,NumOfFile=NumOfFile,CodingOfFile=CodingOfFile)
      class(ans) <- "summaryCodings"
      ans
    } else {
      cat("No coding.\n")
    }
  } else {
    cat("Open a project first.\n")
  }
}

print.summaryCodings <- function(x,...){
  class(x)
  if (!is.null(x$CodingOfFile)){
    cat("----------------\n")
    cat("Number of codings for each file.\n")
    print(x$CodingOfFile)
  }
  cat("----------------\n")
  cat("Number of codings for each code.\n")
  print(x$NumOfCoding)
  cat("----------------\n")
  cat("Average number of characters associated with each code.\n\n")
  print(x$AvgLength)
  cat("----------------\n")
  cat("Number of files associated with each code.\n\n")
  print(x$NumOfFile)
}


searchFiles <- SearchFiles <- function(pattern,content=FALSE,Fid=NULL,Widget=NULL,is.UTF8=FALSE){
##SearchFiles("file like '%新民晚报%'")
##SearchFiles("name like '%物权法%'")
##SearchFiles("file like '%新民晚报%'",Widget=.rqda$.fnames_rqda)
    if ( is_projOpen() ) {
        if(!is.UTF8){ pattern <- iconv(pattern,to="UTF-8")}
        Encoding(pattern) <- "unknown"
        if (!is.null(Fid)) pattern <- sprintf("(%s) and id in (%s)",pattern,paste(shQuote(Fid),collapse=","))
        if (content){
            ans <- dbGetQuery(.rqda$qdacon,sprintf("select id, name,file from source where status=1 and %s",pattern))
        } else {
            ans <- dbGetQuery(.rqda$qdacon,sprintf("select id, name from source where status=1 and %s",pattern))
        }
        if (nrow(ans)>0) Encoding(ans$name) <- "UTF-8"
        if (!is.null(ans$file)) Encoding(ans$file) <- "UTF-8"
        if (!is.null(Widget))  {
            eval(parse(text=sprintf(".rqda$%s[] <- ans$name",Widget)))
            ## eval(substitute(widget[] <- ans$name,list(widget=quote(Widget))))
        }
        cat(sprintf("%s retrieved file(s).", nrow(ans)))
        invisible(ans)
    } else cat("Open a project first.\n")
}

RunOnSelected <- function(x,multiple=TRUE,expr,enclos=parent.frame(),title=NULL,
                          hpos = ifelse(is.null(getOption("widgetCoordinate")[1]),
                            420,getOption("widgetCoordinate")[1]),
                          vpos = ifelse(is.null(getOption("widgetCoordinate")[2]),
                            2,getOption("widgetCoordinate")[2]),
                          ...){
  ## expr used the return of Selected as an argument
  if (is.null(title)) title <- ifelse(multiple,"Select one or more","Select one")
  g <- gwindow(title=title,width=250,height=600,parent=c(hpos, vpos))
  x1<-ggroup(FALSE,container=g)
  ##x1@widget@widget$parent$parent$parent$SetTitle(title)
  ##x1@widget@widget$parent$parent$parent$SetDefaultSize(200, 500)
  x2<-gtable(x,multiple=multiple,container=x1,expand=TRUE)
  gbutton("Cancel",container=x1,handler=function(h,...){
    dispose(x1)
  })
  gbutton("OK",container=x1,handler=function(h,...){
    Selected <- svalue(x2)
    if (Selected!=""){
      eval(h$action$expr,envir=pairlist(Selected=Selected),enclos=h$action$enclos)
      ## evaluate expr in env
      ## Variable Selected will be found in env
      ## because env is parilist and there are variables not there, which will be found in enclos.
      dispose(g)
    } else gmessage("Select before Click OK.\n",container=TRUE,icon="error")
  },
          action=list(expr=substitute(expr),enclos=enclos)
          )
  invisible()
}


gselect.list <- function(list,multiple=TRUE,title=NULL,width=200, height=500,...){
  ## gtk version of select.list(), revised on 21 Apr. 2010 to fix a bug (crash R with 2.18 or newer libgtk2).
  ## Thanks go to John Verzani for his help.
  if (is.null(title)) title <- ifelse(multiple,"Select one or more","Select one")
  helper <- function(){
      ans<-new.env()
       dlg <- gbasicdialog(title=title,handler=function(h,...){
          value <- svalue(x2)
          assign("selected",value,envir=h$action$env)
          },action=list(envir=ans))
      x2<-gtable(list,multiple=multiple,container=dlg,expand=TRUE)
      dlg@widget@widget$Move(size(.rqda$.root_rqdagui)[1],2)
      size(dlg) <- c(width,height)
      visible(dlg, set=TRUE)
      ans
  }## end helper function
  items <- helper()$selected
  if (is.null(items)) items <- ""
  items
}


GetFileName <- function(fid=GetFileId()){
  ans <-  dbGetQuery(.rqda$qdacon,sprintf("select name from source where status=1 and id in (%s)",paste(shQuote(fid),collapse=",")))$name
  if (length(ans)>0) Encoding(ans) <- "UTF-8"
  class(ans) <- c("RQDA.vector","fileName")
  ans
}

getFiles <- function(condition = c("unconditional", "case", "filecategory", "both"),
                     type = c("all", "coded", "uncoded", "selected"),names=TRUE) {
  ans <- GetFileId(condition,type)
  if (names){
    ans <- GetFileName(ans)
  }
  ans
}

GetCaseId <- function(fid=GetFileId(),nFiles=FALSE){
  ## if (caseName){
  if (nFiles) {
    ## ans <-  dbGetQuery(.rqda$qdacon,sprintf(" select name,id from cases where status=1 and id in (select caseid from caselinkage where status=1 and fid in (%s) group by caseid )",paste(shQuote(fid),collapse=",")))
    ## if (nrow(ans)>0) Encoding(ans$name) <- "UTF-8"
    ans <- dbGetQuery(.rqda$qdacon,sprintf("select caseid, count(caseid) as nFiles from caselinkage where status=1 and fid in (%s) group by caseid",paste(shQuote(fid),collapse=",")))
  } else {
    ans <- dbGetQuery(.rqda$qdacon,sprintf("select caseid from caselinkage where status=1 and fid in (%s) group by caseid",paste(shQuote(fid),collapse=",")))$caseid
  }
  ## attr(ans,"caseName") <- caseName
  class(ans) <- c("RQDA.vector","caseId")
  ans
}

getCases <- function(fid, names=TRUE) {
  ans <- GetCaseId(fid,nFiles=FALSE)
  if (names){
    ans <- GetCaseName(ans)
  }
  ans
}

GetCaseName <- function(caseId=GetCaseId(nFiles=FALSE)){
  ans <-  dbGetQuery(.rqda$qdacon,sprintf("select name from cases where status=1 and id in (%s)",paste(shQuote(caseId),collapse=",")))$name
  if (length(ans)>0) Encoding(ans) <- "UTF-8"
  class(ans) <- c("RQDA.vector","caseName")
  ans
}

casesCodedByAnd <- function(cid){
  ## cid can be splitted across files, but still on the same case
  Ncid <- length(cid)
  cid <- paste(cid,collapse=',')
  fid <- RQDAQuery(sprintf("select fid,cid from coding where status=1 and cid in (%s)",cid))
  if (nrow(fid)>0) {
    fidUnique <- unique(fid$fid)
    fidUnique <- paste(fidUnique,collapse=',')
    case <- RQDAQuery(sprintf("select fid, caseid from caselinkage where status=1 and fid in (%s)",fidUnique))
    codes <- tapply(case$fid, case$caseid,FUN=function(x) unique(fid[fid$fid %in% unique(x),]$cid))
    ans <- sapply(codes,length)
    ans <- as.numeric(names(ans)[ans==Ncid])
  }
  class(ans) <- c("RQDA.vector","caseId")
  ans
}

casesCodedByNot <- function(cid){
  fid <- filesCodedByOr(cid)
  codedcaseId <- GetCaseId(fid)
  allcaseid <- GetCaseId(GetFileId("unconditional","coded"))
  ans <- setdiff(allcaseid,codedcaseId)
  class(ans) <- c("RQDA.vector","caseId")
  ans
}

casesCodedByOr <- function(cid){
  fid <- filesCodedByOr(cid)
  if (length(fid)!=0) {
      ans <- GetCaseId(fid)
  } else ans <- integer(0)
  class(ans) <- c("RQDA.vector","caseId")
  ans
}

RQDAQuery <- function(sql){
if (is_projOpen()) {
dbGetQuery(.rqda$qdacon,sql)
} else (cat("open a project first\n."))
}

ShowSubset <- function(x,...){
  UseMethod("ShowSubset")
}
ShowSubset.fileName <- function(x,widget=".fnames_rqda",envir=.rqda,...){
  widget <- get(widget,envir=envir)
  class(x) <- NULL
  widget[] <- x
}
ShowSubset.CaseAttr <- function(x,...){
  tryCatch(.rqda$.CasesNamesWidget[] <- x$case, error = function(e) {})
}
ShowSubset.FileAttr <- function(x,...){
  tryCatch(.rqda$.fnames_rqda[] <- x$file, error = function(e) {})
}
ShowSubset.caseName <- function(x,...){
   class(x) <- NULL
   .rqda$.CasesNamesWidget[] <- x
}

ShowFileProperty <- function(Fid = GetFileId(,"selected"),focus=TRUE) {
  if (is_projOpen(envir = .rqda, conName = "qdacon", message = FALSE)) {
    if (is.null(Fid)) val <- "No files are selected."
    if (length(Fid)==1) {
      Fcat <- RQDAQuery(sprintf("select name from filecat where catid in (select catid from treefile where fid=%i and status=1) and status=1",Fid))$name
      Case <- RQDAQuery(sprintf("select name from cases where id in (select caseid from caselinkage where fid=%i and status=1) and status=1",Fid))$name
      if (!is.null(Fcat)) Encoding(Fcat) <- "UTF-8"
      if (!is.null(Case)) Encoding(Case) <- "UTF-8"
      fcat <- paste(strwrap(sprintf("File Category is %s",paste(shQuote(Fcat),collapse=", ")),105,exdent=4),collapse="\n")
      Encoding(fcat) <-  "UTF-8"
      val <- sprintf(" File ID is %i \n %s \nCase is %s",Fid,fcat,paste(shQuote(Case),collapse=", "))
    }
    if (length(Fid)>1) val <- "Please select one file only."
    tryCatch(svalue(.rqda$.sfp) <- val,error=function(e){
      gw <- gwindow("File Property",parent=size(.rqda$.root_rqdagui)+c(19,-50),
            width = min(c(gdkScreenWidth() - size(.rqda$.root_rqdagui)[1] -20,getOption("widgetSize")[1])),
            height = 50)
      mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
      gw@widget@widget$SetIconFromFile(mainIcon)
      sfp <- glabel(val,container=gw)
      assign(".sfp",sfp,envir=.rqda)
      "focus<-"(gw,value=focus)
    })
  }}


filesCodedByAnd <- function(cid, codingTable=c("coding","coding2")){
    cid <- paste(cid,collapse=',')
    fid <- RQDAQuery(sprintf("select fid,cid from %s where status=1 and cid in (%s)",codingTable, cid))
    if (nrow(fid)>0) {
        fidList <- by(fid,factor(fid$cid),FUN=function(x) unique(x$fid))
        fid <- Reduce(intersect,fidList)
    } else {fid <- integer(0)}
    class(fid) <- c("RQDA.vector","fileId")
    fid
}

filesCodedByOr <- function(cid, codingTable=c("coding","coding2")){
    cid <- paste(cid,collapse=',')
    fid <- RQDAQuery(sprintf("select fid from %s where status=1 and cid in (%s)",codingTable, cid))$fid
    if (length(fid)==0) {fid <- integer(0)}
    class(fid) <- c("RQDA.vector","fileId")
    fid
}

filesCodedByNot <- function(cid, codingTable=c("coding","coding2")){
    codedfid <- filesCodedByOr(cid)
    allfid <- RQDAQuery(sprintf("select fid from %s where status=1 group by fid",codingTable))$fid
    fid <- setdiff(allfid,codedfid)
    if (length(fid)==0) {fid <- integer(0)}
    class(fid) <- c("RQDA.vector","fileId")
    fid
}

nCodedByTwo <- function(FUN, codeList=NULL, print=TRUE,...){
    ## codeList is character vector of codes.
    if (!is_projOpen(message=FALSE)) stop("No project is opened.")
    FUN <- match.fun(FUN)
    Cid_Name <- RQDAQuery("select id, name from freecode where status=1")
    if (is.null(codeList)) {
        codeList <- gselect.list(Cid_Name$name,multiple=TRUE)
    }
    if (length(codeList)<2) {
        stop("The codeList should be a vector of length 2 or abvoe.")
    } else {
        cidList <- Cid_Name$id[match(codeList, Cid_Name$name)]
        ans <- matrix(nrow=length(codeList), ncol=length(codeList),dimnames=list(
                                                                   sprintf("%s(%s)", codeList,cidList),
                                                                   cidList))
        for (i in 1:length(cidList)){
            for (j in i:length(cidList)){
                ans[i,j] <- length(do.call(FUN, list(cidList[c(i,j)])))
            }
        }
        if (print) {print(ans,na.print="")}
        invisible(ans)
    }
}


"%and%" <- function(e1,e2){
    UseMethod("%and%")
}

"%or%" <- function(e1,e2){
    UseMethod("%or%")
}

"%not%" <- function(e1,e2){
    UseMethod("%not%")
}

"%and%.RQDA.vector" <- function(e1,e2)
{
    cls <- class(e1)
    ans <- intersect(e1,e2)
    class(ans) <- cls
    ans
}

"%not%.RQDA.vector" <- function(e1,e2)
{
    cls <- class(e1)
    ans <- setdiff(e1, e2)
    class(ans) <- cls
    ans
}

"%or%.RQDA.vector" <- function(e1,e2)
{
    cls <- class(e1)
    ans <- union(e1, e2)
    class(ans) <- cls
    ans
}

QueryFile <- function(or=NULL,and=NULL,not=NULL,names=TRUE){
  fid.or <- fid.and <- fid.not <- integer(0)
  if (!is.null(or)) fid.or <- filesCodedByOr(or)
  if (!is.null(and)) fid.and <- filesCodedByAnd(and)
  if (!is.null(not)) fid.or <- filesCodedByOr(not)
  ans <- setdiff(intersect(fid.or,fid.and),fid.not)
  class(ans) <- c("RQDA.vector","fileId")
  if (names) {
    if (length(ans)>0){
      ans <- RQDAQuery(sprintf("select name from source where status=1 and id in (%s)", paste(ans,collapse=',')))$name
      Encoding(ans) <- "UTF-8"
    } else {
      ans <- character(0)
    }
    class(ans) <- c("RQDA.vector","fileName")
    .rqda$.fnames_rqda[] <- ans
  }
  ans
}

UpdateCoding <- function(){
    rowid <- RQDAQuery("select rowid from coding")$rowid
    for (i in rowid) {
    RQDAQuery(sprintf("update coding set seltext=(select substr(source.file,coding.selfirst+1,coding.selend-coding.selfirst)
        from coding inner join source on coding.fid=source.id where coding.ROWID=%i) where coding.ROWID=%i",i,i))
}}
#UpdateCoding()


filesByCodes <- function(codingTable=c("coding","coding2")){
  codingTable <- match.arg(codingTable)
  if (codingTable=="coding"){
    ans <- RQDAQuery("select coding.fid as fid, freecode.name as codename, source.name as filename from coding left join freecode on (coding.cid=freecode.id)left join source on (coding.fid=source.id) where coding.status=1 and source.status=1 and freecode.status=1")
  }
  if (codingTable=="coding2"){
    ans <- RQDAQuery("select coding2.fid as fid, freecode.name as codename, source.name as filename from coding2 left join freecode on (coding2.cid=freecode.id)left join source on (coding2.fid=source.id) where coding2.status=1 and source.status=1 and freecode.status=1")
  }
  if (nrow(ans)!=0){
    Encoding(ans$codename) <- Encoding(ans$filename) <- "UTF-8"
    ans$codedBy <- 1
    ansWide <- reshape(ans,idvar="fid",timevar="codename",v.names="codedBy",direction="wide")
    ansWide[is.na(ansWide)] <- 0
  }
  ansWide
}


