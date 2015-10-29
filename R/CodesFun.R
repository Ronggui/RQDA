addcode <- function(name,conName="qdacon",assignenv=.rqda,...) {
  if (name != ""){
    con <- get(conName,assignenv)
    maxid <- dbGetQuery(con,"select max(id) from freecode")[[1]]
    nextid <- ifelse(is.na(maxid),0+1, maxid+1)
    write <- FALSE
    if (nextid==1){
      write <- TRUE
    } else {
      dup <- dbGetQuery(con,sprintf("select name from freecode where name='%s'",name))
      if (nrow(dup)==0) write <- TRUE
    }
    if (write ) {
      dbGetQuery(con,sprintf("insert into freecode (name, id, status,date,owner)
                                            values ('%s', %i, %i,%s, %s)",
                             name,nextid, 1, shQuote(date()),shQuote(.rqda$owner)))
    }
  }
}


CodeNamesUpdate <- function(CodeNamesWidget=.rqda$.codes_rqda,sortByTime=TRUE,decreasing = FALSE,...)
{
  if (is_projOpen()){
  freecode <- RQDAQuery("select name, id,date from freecode where status=1 order by lower(name)")
  codeName <- freecode$name
  if (nrow(freecode)!=0) {
    Encoding(codeName) <- "UTF-8"
    if (sortByTime){
      codeName <- codeName[OrderByTime(freecode$date,decreasing=decreasing)]
    }
  }
  tryCatch(CodeNamesWidget[] <- codeName, error=function(e){})
  } else gmessage("Cannot update Code List in the Widget. Project is closed already.\n",con=TRUE)
}


CodeNamesWidgetUpdate <- function(CodeNamesWidget=.rqda$.codes_rqda,sortByTime=TRUE,decreasing = FALSE,CodeId=NULL,...)
  ## CodeNamesWidgetUpdate is the alternative function of CodeNamesUpdate, should be used afterwards
{
  if (is_projOpen()){
    freecode <- dbGetQuery(.rqda$qdacon, "select name, id,date from freecode where status=1 order by lower(name)")
    if (nrow(freecode)!=0) {
      if (!is.null(CodeId)) {freecode <- freecode[freecode$id %in% CodeId,]}
      codeName <- freecode$name
      Encoding(codeName) <- "UTF-8"
      if (sortByTime){
        codeName <- codeName[OrderByTime(freecode$date,decreasing=decreasing)]
      }
    }
    tryCatch(CodeNamesWidget[] <- codeName, error=function(e){})
  } else gmessage("Cannot update Code List in the Widget. Project is closed already.\n",con=TRUE)
}

mark <- function(widget,fore.col=.rqda$fore.col,back.col=NULL,addButton=FALSE,buttonLabel="",codingTable="coding"){
  ## modified so can change fore.col and back.col easily
  index <- sindex(widget,includeAnchor=TRUE,codingTable=codingTable)
  startI <- index$startI ## start and end iter
  endI <- index$endI
  selected <- index$seltext
  Encoding(selected) <- "UTF-8"
  startN <- index$startN # translate iter pointer to number
  endN <- index$endN
  if (selected != ""){## only when selected text chunk is not "", apply the color scheme.
    buffer <- slot(widget,"widget")@widget$GetBuffer()
    if(addButton) {
      InsertAnchor(widget,sprintf("%s<",buttonLabel),index=startN,handler=TRUE)
      InsertAnchor(widget,sprintf(">%s",buttonLabel),index=endN + 1)
    }
    startIter <- buffer$GetIterAtMark(index$startMark)$iter
    endIter <- buffer$GetIterAtMark(index$endMark)$iter
    if (!is.null(fore.col)){  ## when col is NULL, it is skipped
      buffer$ApplyTagByName(fore.col,startIter,endIter)## make use of property of gtext().
    }
    if (!is.null(back.col)){
      buffer$ApplyTagByName(sprintf("%s.background",back.col),startIter,endIter)
    }
    startN <- index$startN
    endN <- index$endN
    startN <- startN - countAnchorsWithFileName(to=startN,codingTable=codingTable)
    endN <- endN - countAnchorsWithFileName(to=endN,codingTable=codingTable)
    ##startN <- startN - countAnchors(.rqda$.openfile_gui,from=0,to=startN)
    ##endN <- endN - countAnchors(.rqda$.openfile_gui,from=0,to=endN)
    return(list(start=startN,end=endN,text=selected))
  }
}


markRange <- function(widget,from,to,rowid,fore.col=.rqda$fore.col,back.col=NULL,addButton=FALSE,buttonLabel="",buttonCol=.rqda$codeMark.col,codingTable="coding"){
  if (from != to){
    FileName <- tryCatch(svalue(.rqda$.root_edit),error=function(e){})
    if (!is.null(FileName)){
      Fid <- RQDAQuery(sprintf("select id from source where status =1 and name='%s'",enc(FileName)))$id
      idx <- RQDAQuery(sprintf("select selfirst,selend,rowid from %s where fid=%i and status=1", codingTable, Fid))
      if (nrow(idx)!=0) idx <- idx[idx$rowid!=rowid,c("selfirst","selend")] ## exclude itself
      anno <- RQDAQuery(sprintf("select position,rowid from annotation where status=1 and fid=%s",Fid))
      allidx <- c(idx$selfirst,anno$position)
      if (!is.null(allidx)){
        from <- from + sum(allidx <= from)
        to <- to + sum(allidx <= to)
      }
      buffer <- slot(widget,"widget")@widget$GetBuffer()
      startIter <- buffer$GetIterAtOffset(from)$iter
      endIter <- buffer$GetIterAtOffset(to)$iter
      buffer$CreateMark(sprintf("%s.1",rowid),where=startIter)
      buffer$CreateMark(sprintf("%s.2",rowid),where=endIter)
      buffer <- slot(widget,"widget")@widget$GetBuffer()
      if(addButton) {
        InsertAnchor(widget,sprintf("<%s>",buttonLabel),index=from,label.col=buttonCol,
                     handler=TRUE, EndMarkName=sprintf("%s.2", rowid))
      }
      m1 <- buffer$GetMark(sprintf("%s.1", rowid))
      startIter <- buffer$GetIterAtMark(m1)$iter
      m2 <- buffer$GetMark(sprintf("%s.2", rowid))
      endIter <- buffer$GetIterAtMark(m2)$iter
      if (!is.null(fore.col)) buffer$ApplyTagByName(fore.col,startIter,endIter)
      if (!is.null(back.col)) buffer$ApplyTagByName(sprintf("%s.background",back.col),startIter,endIter)
    }}}

ClearMark <- function(widget,min=0, max, clear.fore.col=TRUE,clear.back.col=FALSE, clear.underline=TRUE){
  ## max position of marked text.
  buffer <- slot(widget,"widget")@widget$GetBuffer()
  startI <- gtkTextBufferGetIterAtOffset(buffer,min)$iter # translate number back to iter
  endI <-gtkTextBufferGetIterAtOffset(buffer,max)$iter
  if (clear.fore.col) gtkTextBufferRemoveTagByName(buffer,.rqda$fore.col,startI,endI)
  if (clear.back.col) gtkTextBufferRemoveTagByName(buffer,sprintf("%s.background",.rqda$back.col),startI,endI)
  if (clear.underline) gtkTextBufferRemoveTagByName(buffer,"underline",startI,endI)
}

HL <- function(W,index,fore.col=.rqda$fore.col,back.col=NULL){
  ## highlight text chuck according to index
  ## W is the gtext widget of the text.
  ## index is a data frame, each row == one text chuck.
  buffer <- slot(W,"widget")@widget$GetBuffer()
  apply(index,1, function(x){
    start <-gtkTextBufferGetIterAtOffset(buffer,x[1])$iter # translate number back to iter
    end <-gtkTextBufferGetIterAtOffset(buffer,x[2])$iter
    if (!is.null(fore.col)){
      buffer$ApplyTagByName(fore.col,start,end)
    }
    if (!is.null(back.col)){
      buffer$ApplyTagByName(sprintf("%s.background",back.col),start,end)
    }
  }
        )
}

sindex <- function(widget=.rqda$.openfile_gui,includeAnchor=TRUE,codingTable="coding"){
  buffer <- slot(widget,"widget")@widget$GetBuffer()
  bounds = buffer$GetSelectionBounds()
  startI = bounds$start ## start and end iter
  endI = bounds$end
  selected <- buffer$GetText(startI,endI)
  startMark <- buffer$CreateMark(mark.name=NULL,where=startI)
  endMark <- buffer$CreateMark(mark.name=NULL,where=endI)
  startN <- gtkTextIterGetOffset(startI) # translate iter pointer to number
  endN <- gtkTextIterGetOffset(endI)
  if (!includeAnchor) {
    startN <- startN - countAnchorsWithFileName(to=startN,codingTable=codingTable)
    endN <- endN - countAnchorsWithFileName(to=endN,codingTable=codingTable)
    ##startN <- startN - countAnchors(widget,from=0,to=startN)
    ##endN <- endN - countAnchors(widget,from=0,to=endN)
  }
  return(list(startI=startI,endI=endI,startN=startN,endN=endN,
              startMark=startMark,endMark=endMark,seltext=selected))
}

InsertAnchor <- function(widget,label,index,label.col="gray90",
                         handler=FALSE, EndMarkName=NULL) {
    ## EndMarkName is a gtk mark for end position of highlight
    lab <- gtkLabelNew(label)
    labelEvBox <- gtkEventBoxNew()
    if (isTRUE(handler)) labelEvBox$ModifyBg("normal", gdkColorParse(label.col)$color)
    labelEvBox$Add(lab)
    buffer <- slot(widget,"widget")@widget$GetBuffer()
    if (isTRUE(handler)){
      button_press <-function(widget,event,W, codeName = label){
          if (attr(event$type,"name")== "GDK_BUTTON_PRESS" && event$button==1) {
              ## action for left click
              if (!is.null(EndMarkName)){
                  Iter <- gtkTextBufferGetIterAtChildAnchor(buffer,anchor)$iter
                  Offset <- Iter$GetOffset()
                  maxidx <- buffer$GetBounds()$end$GetOffset()
                  ClearMark(W,min=0,max=maxidx)
                  m <- buffer$GetMark(EndMarkName)
                  gtkTextMarkSetVisible(m,TRUE) ## useful when a coding end with space
                  Offset2 <- buffer$GetIterAtMark(m)$iter$GetOffset()
                  HL(W=W, index=data.frame(Offset,Offset2))
                  ## buffer$createTag("underline", underline = "single")
                  ## should be created when a file is opened
                  rowid <- gsub(".2$","",EndMarkName)
                  assign("selectedRowid", rowid, envir=.codingEnv)
                  enabled(button$UnMarB1) <- TRUE
                  memo <- RQDAQuery(sprintf("select memo from coding where rowid=%s",rowid))$memo
                  if (!is.na(memo) && memo!="") {
                      buffer$ApplyTagByName("underline",Iter,buffer$GetIterAtMark(m)$iter)
                  }
              }
          }
          if (attr(event$type,"name")== "GDK_BUTTON_PRESS" && event$button==3) {
              ## action for right click
              if (!is.null(EndMarkName)) {
                  rowid <- gsub(".2$","",EndMarkName)
                  prvcontent <- RQDAQuery(sprintf("select memo from coding where rowid=%s",rowid))[1,1]
                  tryCatch(dispose(.rqda$.codingmemo),error=function(e) {})
                  ## Close the coding memo first, then open a new one
                  title <- sprintf("Coding Memo:%s",codeName)
                  .codingmemo <- gwindow(title=title,getOption("widgetCoordinate"),width=600,height=400)
                  assign(".codingmemo",.codingmemo, envir=.rqda)
                  .codingmemo <- get(".codingmemo",envir=.rqda)
                  .codingmemo2 <- gpanedgroup(horizontal = FALSE, container=.codingmemo)
                  .codingMemoSaveButton <- gbutton("Save Coding Memo",container=.codingmemo2,action=list(rowid=rowid),handler=function(h,...){
                      newcontent <- svalue(.rqda$.cdmemocontent)
                      newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
                      RQDAQuery(sprintf("update coding set memo='%s' where rowid=%s",newcontent,rowid=h$action$rowid))
                      enabled(.rqda$".codingMemoSaveButton") <- FALSE
                  })## end of save memo button
                  enabled(.codingMemoSaveButton) <- FALSE
                  assign(".codingMemoSaveButton",.codingMemoSaveButton,envir=.rqda)
                  assign(".cdmemocontent",gtext(container=.codingmemo2,font.attr=c(sizes="large")),envir=.rqda)
                  if (is.na(prvcontent)) prvcontent <- ""
                  Encoding(prvcontent) <- "UTF-8"
                  if (prvcontent=="") assign("NewCodingMemo",TRUE,envir=.rqda)
                  W <- get(".cdmemocontent",envir=.rqda)
                  add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
                  gSignalConnect(W@widget@widget$GetBuffer(), "changed",
                                 function(h,...){
                                 enabled(.rqda$".codingMemoSaveButton") <- TRUE
                                 })

              }
          }
      }
      gSignalConnect(labelEvBox, "button-press-event",button_press,data=widget)
  }
    iter <- gtkTextBufferGetIterAtOffset(buffer,index)$iter
    anchorcreated <- buffer$createChildAnchor(iter)
    iter$BackwardChar()
    anchor <- iter$getChildAnchor()
    anchor <- gtkTextIterGetChildAnchor(iter)
    widget@widget@widget$addChildAtAnchor(labelEvBox, anchor)
}


DeleteButton <- function(widget,label,index,direction=c("backward","forward")){
  buffer <- slot(widget,"widget")@widget$GetBuffer()
  direction <- match.arg(direction)
  if (direction=="backward") index <- index - 1
  iter <- gtkTextBufferGetIterAtOffset(buffer,index)$iter
  stop <- FALSE
  isRemove <- FALSE
  while (!stop) {
    Anchor <- iter$getChildAnchor()
    if (!is.null(Anchor)){
      lab <- Anchor$GetWidgets()[[1]][["child"]]$GetLabel()
      Encoding(lab) <- "UTF-8"
      if (lab==label){
        iterEnd <- gtkTextIterGetOffset(iter)
        iterEnd <- gtkTextBufferGetIterAtOffset(buffer,iterEnd+1)$iter
        gtkTextBufferDelete(buffer,iter,iterEnd)
        stop <- TRUE
        isRemove <- TRUE
      }
      if (direction=="backward") if (! iter$BackwardChar()) stop <- TRUE
      if (direction=="forward") if (! iter$ForwardChar()) stop <- TRUE
    } else {stop <- TRUE}
  }
  invisible(isRemove)
}

countAnchors <- function(widget=.rqda$.openfile_gui,to,from=0){
  buffer <- slot(widget,"widget")@widget$GetBuffer()
  iter <- gtkTextBufferGetIterAtOffset(buffer,from)$iter
  ans <- 0
  while(from<to){
    hasAnchor <- iter$getChildAnchor()
    ans <- ans + ifelse(is.null(hasAnchor),0,1)
    gtkTextIterForwardChar(iter)
    from <- gtkTextIterGetOffset(iter)
  }
  ans
}
## testing
## g<-gtext("testing widget of text.",container=T)
## InsertAnchor(g,"button",8)

countAnchorsWithFileName <- function(to,fileName=enc(svalue(.rqda$.root_edit),encoding="UTF-8"),codingTable="coding")
{
  ## the same purpose as countAnchors, but faster.
  fid <- RQDAQuery(sprintf("select id from source where status=1 and name='%s'",fileName))$id
  ## idx <- RQDAQuery(sprintf("select selfirst,selend from coding where status==1 and fid==%s",fid))
  idx <- RQDAQuery(sprintf("select selfirst from %s where status=1 and fid=%s", codingTable, fid)) ## insert one code lable only for 0.2-0
  anno <- RQDAQuery(sprintf("select position from annotation where status=1 and fid=%s",fid))$position
  allidx <- c(unlist(idx),anno)
  if (!is.null(allidx)){
    allidx <- allidx + rank(allidx,ties.method="first")
    ans <- sum(allidx <= to) ## note the equal sign
   } else ans <- 0
  ans
}

## testIt <- function(){ ## test the reliability of countAnchorsWithFileName().
## a <- sindex(incl=T)
## ans <- data.frame(correct=c(countAnchors(to=a$startN),countAnchors(to=a$endN)),wrong=c(countAnchorsWithFileName(to=a$startN),countAnchorsWithFileName(to=a$endN)))
## ans
## }

retrieval <- function(Fid=NULL,order=c("fname","ftime","ctime"),CodeNameWidget=.rqda$.codes_rqda, codingTable="coding")
## retrieval is rewritten in rev 134
{
  currentCode2 <- svalue(CodeNameWidget)
  if (length(currentCode2)!=0){
    currentCode <- enc(currentCode2,"UTF-8")
    Encoding(currentCode2) <- "UTF-8"
    currentCid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where name= '%s' ",currentCode))[1,1]
    order <- match.arg(order)
    order <- switch(order,
                    fname="order by source.name",
                    ftime="order by source.id",
                    ctime="")
    if (is.null(Fid)){
      retrieval <- RQDAQuery(sprintf("select cid,fid, selfirst, selend, seltext,%s.rowid, source.name,source.id from %s,source where %s.status=1 and cid=%i and source.id=fid %s",codingTable,codingTable,codingTable,currentCid,order))
    } else {
      retrieval <- RQDAQuery(sprintf("select cid,fid, selfirst, selend, seltext, %s.rowid,source.name,source.id from %s,source where %s.status=1 and cid=%i and source.id=fid and fid in (%s) %s",codingTable, codingTable, codingTable, currentCid, paste(Fid,collapse=","), order))
    }
    if (nrow(retrieval)==0) gmessage("No Coding associated with the selected code.",container=TRUE) else {
      fid <- unique(retrieval$fid)
      retrieval$fname <-""
      Nfiles <- length(fid)
      Ncodings <- nrow(retrieval)
      title <- sprintf(ngettext(Ncodings,"%i Retrieved coding: \"%s\" from %s %s","%i Retrieved codings: \"%s\" from %s %s"),Ncodings,currentCode2,Nfiles,ngettext(Nfiles,"file","files"))
      tryCatch(eval(parse(text=sprintf("dispose(.rqda$.codingsOf%s)",currentCid))),error=function(e){})
      wnh <- size(.rqda$.root_rqdagui) ## size of the main window
      .gw <- gwindow(title=title, parent=c(wnh[1]+10,2),
                     width = min(c(gdkScreenWidth()- wnh[1]-20,getOption("widgetSize")[1])),
                     height = min(c(wnh[2],getOption("widgetSize")[2]))
                     )
      mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
      .gw@widget@widget$SetIconFromFile(mainIcon)
      assign(sprintf(".codingsOf%s",currentCid),.gw,envir=.rqda)
      .retreivalgui <- gtext(container=.gw)
      font <- pangoFontDescriptionFromString(.rqda$font)
      gtkWidgetModifyFont(.retreivalgui@widget@widget,font)
      .retreivalgui@widget@widget$SetPixelsBelowLines(5) ## set the spacing
      .retreivalgui@widget@widget$SetPixelsInsideWrap(5) ## so the text looks more confortable.
    ## .retreivalgui <- gtext(container=.gw)
      for (i in fid){
        FileName <- dbGetQuery(.rqda$qdacon,sprintf("select name from source where status=1 and id=%i",i))[['name']]
        if (!is.null(FileName)){
          Encoding(FileName) <- "UTF-8"
          retrieval$fname[retrieval$fid==i] <- FileName
        } else {
          retrieval <- retrieval[retrieval$fid!=i,]
          RQDAQuery(sprintf("update %s set status=0 where fid=%i",codingTable, i))
        }
      }
      Encoding(retrieval$seltext) <-  Encoding(retrieval$fname) <- "UTF-8"
      ## helper function
      ComputeCallbackFun <- function(FileName,rowid){
        CallBackFUN <- function(widget,event,...){
          ViewFileFunHelper(FileName,hightlight=FALSE)
          textView <- .rqda$.openfile_gui@widget@widget
          buffer <- textView$GetBuffer()
          mark1 <- gtkTextBufferGetMark(buffer,sprintf("%s.1",rowid))
          gtkTextViewScrollToMark(textView,mark1,0)
          iter1 <- buffer$GetIterAtMark(mark1)$iter
          idx1 <- gtkTextIterGetOffset(iter1)
          mark2 <- buffer$GetMark(sprintf("%s.2", rowid))
          gtkTextMarkSetVisible(mark2,TRUE)
          iter2 <- buffer$GetIterAtMark(mark2)$iter
          idx2 <- gtkTextIterGetOffset(iter2)
          HL(.rqda$.openfile_gui, data.frame(idx1,idx2), fore.col = .rqda$fore.col, back.col = NULL)
        }
        CallBackFUN
      } ## end of ComputeCallbackFun

    ComputeRecodeFun <- function(rowid){
      RecodeFun <- function(widget, event, ...){
        SelectedCode <- svalue(.rqda$.codes_rqda)
        if (length(SelectedCode)!=0){
          Encoding(SelectedCode) <- "UTF-8"
          SelectedCode2 <- enc(SelectedCode, encoding="UTF-8")
          currentCid <-  dbGetQuery(.rqda$qdacon, sprintf("select id from freecode where name='%s'",SelectedCode2))$id
          DAT <- RQDAQuery(sprintf("select * from coding where rowid=%s", rowid))
          DAT$seltext <- enc(DAT$seltext)
          Exists <- RQDAQuery(sprintf("select * from coding where cid=%s and selfirst=%s and selend=%s and status=1", currentCid, DAT$selfirst, DAT$selend))
          if (nrow(Exists)==0) {
          success <- is.null(try(RQDAQuery(sprintf("insert into %s (cid,fid, seltext, selfirst, selend, status, owner, date) values (%s, %s, '%s', %s, %s, %s, '%s', '%s') ", codingTable, currentCid, DAT$fid, DAT$seltext, DAT$selfirst, DAT$selend, 1, .rqda$owner, as.character(date()))),silent=TRUE))
          if (!success) gmessage("cannot recode this text segment.", type="warning") else{
            freq <- RQDAQuery(sprintf("select count(cid) as freq from coding where status=1 and cid=%s", currentCid))$freq
            names(CodeNameWidget) <- sprintf("Selected code id is %s__%s codings",currentCid, freq)
          }
          }
        }
      }
      RecodeFun
    } ## end of ComputeRecodeFun

    ComputeUnMarkFun <- function(rowid){
      RecodeFun <- function(widget, event, ...){
        RQDAQuery(sprintf("update %s set status=-1 where rowid=%s", .rqda$codingTable, rowid))
        freq <- RQDAQuery(sprintf("select count(cid) as freq from coding where status=1 and cid=%s", currentCid))$freq
        names(CodeNameWidget) <- sprintf("Selected code id is %s__%s codings",currentCid, freq)
      }
      RecodeFun
      } ## end of ComputeUnMarkFun

      buffer <- .retreivalgui@widget@widget$GetBuffer()
      buffer$createTag("red", foreground = "red")
      iter <- buffer$getIterAtOffset(0)$iter

      apply(retrieval,1, function(x){
        metaData <- sprintf("%s [%i:%i]",x[['fname']],as.numeric(x[['selfirst']]),as.numeric(x[['selend']]))
        ## buffer$InsertWithTagsByName(iter, metaData,"x-large","red")
        buffer$InsertWithTagsByName(iter, metaData,"red")
        iter$ForwardChar()
        buffer$Insert(iter, "\n")
        anchorcreated <- buffer$createChildAnchor(iter)
        iter$BackwardChar()
        anchor <- iter$getChildAnchor()
        lab <- gtkLabelNew("Back")
        widget <- gtkEventBoxNew()
        widget$Add(lab)
        gSignalConnect(widget, "button-press-event",
                       ComputeCallbackFun(x[["fname"]],as.numeric(x[["rowid"]])))
        .retreivalgui@widget@widget$addChildAtAnchor(widget, anchor)
        iter$ForwardChar()
        buffer$Insert(iter, " ")
        buffer$createChildAnchor(iter)
        iter$BackwardChar()
        anchor_recode <- iter$getChildAnchor()
        lab_recode <- gtkLabelNew("Recode")
        widget_recode <- gtkEventBoxNew()
        widget_recode$Add(lab_recode)
        gSignalConnect(widget_recode, "button-press-event",
                       ComputeRecodeFun(as.numeric(x[["rowid"]])))
        .retreivalgui@widget@widget$addChildAtAnchor(widget_recode, anchor_recode)
        iter$ForwardChar()
        buffer$Insert(iter, " ")
        buffer$createChildAnchor(iter)
        iter$BackwardChar()
        anchor_unmark <- iter$getChildAnchor()
        lab_unmark<- gtkLabelNew("Unmark")
        widget_unmark <- gtkEventBoxNew()
        widget_unmark$Add(lab_unmark)
        gSignalConnect(widget_unmark, "button-press-event",
                       ComputeUnMarkFun(as.numeric(x[["rowid"]])))
        .retreivalgui@widget@widget$addChildAtAnchor(widget_unmark, anchor_unmark)
        widget$showAll()
        iter$ForwardChar()
        buffer$insert(iter, "\n")
        buffer$InsertWithTagsByName(iter, x[['seltext']])
        buffer$insert(iter, "\n\n")
      }
            )## end of apply
      buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter)
    }
  }
}

ExportCoding <- function(file="Exported Codings.html",Fid=NULL,order=c("fname","ftime","ctime"),append=FALSE,codingTable="coding")
{
ExportCodingOfOneCode <- function(file,currentCode,Fid,order=c("fname","ftime","ctime"),append=TRUE){
  if (length(currentCode)!=0){
    currentCid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where name= '%s' ",enc(currentCode)))[1,1]
    order <- match.arg(order)
    order <- switch(order,
                    fname="order by source.name",
                    ftime="order by source.id",
                    ctime="")
    ##if (is.null(Fid)){
    ##  retrieval <- RQDAQuery(sprintf("select coding.cid,coding.fid, coding.selfirst, ##coding.selend,coding.seltext,coding.rowid, source.name,source.id from coding,source where coding.status=1 and coding.cid=%i and source.id=coding.fid %s",currentCid,order))
   ## } else {
    retrieval <- RQDAQuery(sprintf("select cid,fid, selfirst, selend, seltext, %s.rowid,source.name,source.id from %s,source where %s.status=1 and cid=%i and source.id=coding.fid and fid in (%s) %s",codingTable,codingTable,codingTable,currentCid, paste(Fid,collapse=","), order))
##    }
    if (nrow(retrieval)==0) gmessage(sprintf("No Coding associated with the '%s'.",currentCode),container=TRUE) else {
      fid <- unique(retrieval$fid)
      retrieval$fname <-""
      for (i in fid){
        FileName <- dbGetQuery(.rqda$qdacon,sprintf("select name from source where status=1 and id=%i",i))[['name']]
        Encoding(FileName) <- "UTF-8"
        if (!is.null(FileName)){
          retrieval$fname[retrieval$fid==i] <- FileName
        } else {
          retrieval <- retrieval[retrieval$fid!=i,]
          RQDAQuery(sprintf("update %s set status=0 where fid=%i",codingTable,i))
        }
      }
      Nfiles <- length(unique(retrieval$fname))
      Ncodings <- nrow(retrieval)
      Encoding(retrieval$seltext) <- "UTF-8"
      if (nrow(retrieval)==1) {
        cat(sprintf("<hr><p align='center'><b><font color='blue' size='+2'>%i Coding of <a id='%s'>\"%s\" from %s %s </a></b></font><hr><p align='left'>",Ncodings,currentCode,currentCode,Nfiles,ngettext(Nfiles,"file","files")),file=file,append=append)
     } else {
       cat(sprintf("<hr><p align='center'><b><font color='blue' size='+2'>%i Codings of <a id='%s'>\"%s\"</a> from %s %s.</b></font><hr><p align='left'>",Ncodings,currentCode,currentCode,Nfiles,ngettext(Nfiles,"file","files")),file=file,append=append)
      }
      retrieval$seltext <- gsub("\\n", "<p>", retrieval$seltext)
      apply(retrieval,1, function(x){
        metaData <- sprintf("<b><font color='red'> %s [%s:%s] </font></b><br><br>",x[['fname']],x[['selfirst']],x[['selend']])
        cat(metaData,file=file,append=TRUE)
        cat(x[['seltext']],file=file,append=TRUE)
        cat(sprintf("<br><a href='#%s+b'>Back<a><br><br>",currentCode),file=file,append=TRUE)
      }
            )## end of apply
    }}}## end of export helper function

if (is.null(Fid)) Fid <- GetFileId(type="coded")
allcodes <- RQDAQuery(sprintf("select freecode.name from freecode, %s where freecode.status=1 and freecode.id=%s.cid and %s.status=1 and %s.fid in (%s) group by freecode.name",
codingTable,codingTable,codingTable,codingTable,
paste(shQuote(Fid),collapse=",")))$name
if (!is.null(allcodes)){
    Encoding(allcodes) <- "UTF-8"
    CodeList <- gselect.list(allcodes, multiple = TRUE, title = "Select one or more codes.")
    if (length(CodeList)>1 || CodeList!="") {
        file=file(file,open="w",encoding="UTF-8")
        if (!append){
            cat("<HEAD><META HTTP-EQUIV='CONTENT-TYPE' CONTENT='text/html; charset=UTF-8'><TITLE>Codings created by RQDA.</TITLE><META NAME='AUTHOR' CONTENT='RQDA'>",file=file,append=append)
        }
        cat(sprintf("Created by <a href='http://rqda.r-forge.r-project.org/'>RQDA</a> at %s<br><br>\n",Sys.time()),file=file,append=TRUE)
        for (i in CodeList){
        cat(sprintf("<a id='%s+b' href='#%s'>%s<a><br>",i,i,i),file=file,append=TRUE)
          }
        for (i in seq_along(CodeList)){
            ExportCodingOfOneCode(file=file,currentCode=CodeList[i],Fid=Fid,order=order,append=TRUE)
        }
        close(file)
    }
}}


ClickHandlerFun <- function(CodeNameWidget,buttons=c("MarCodB1","UnMarB1"),codingTable="coding"){
    ## CodeNameWidget=.rqda$.codes_rqda
    con <- .rqda$qdacon
    SelectedCode <- currentCode <- svalue(CodeNameWidget)
    if (length(SelectedCode)!=0) {
        SelectedCode <- currentCode <- enc(currentCode,encoding="UTF-8")
        currentCid <- dbGetQuery(con,sprintf("select id from freecode where name='%s'",SelectedCode))[,1]
       freq <- RQDAQuery(sprintf("select count(cid) as freq from coding where status=1 and cid=%s", currentCid))$freq
        names(CodeNameWidget) <- sprintf("Selected code id is %s__%s codings",currentCid, freq)
        if (exists(".root_edit",envir=.rqda) && isExtant(.rqda$.root_edit)) { ## a file is open
            for (i in buttons) {
                b <- get(i,envir=button)
                enabled(b) <- TRUE
            }
            SelectedFile <- svalue(.rqda$.root_edit)
            SelectedFile <- enc(SelectedFile,encoding="UTF-8")
            currentFid <-  RQDAQuery(sprintf("select id from source where name='%s'",SelectedFile))[,1]
            ## following code: Only mark the text chuck according to the current code.
            idx1 <-  dbGetQuery(con,sprintf("select selfirst, selend from %s where
                                       cid=%i and fid=%i and status=1",codingTable, currentCid, currentFid))
            idx2 <- dbGetQuery(con, sprintf("select selfirst, selend from %s where fid=%i and status=1",codingTable, currentFid))
            if (nrow(idx2)>0) {
                ClearMark(.rqda$.openfile_gui,min=0,max=max(as.numeric(idx2$selend))+2*nrow(idx2),clear.fore.col = TRUE, clear.back.col =FALSE)
            }
            if (nrow(idx1)>0) {
                ##allidx <- unlist(idx2)
                anno <- RQDAQuery(sprintf("select position from annotation where status=1 and fid=%s",currentFid))$position
                allidx <- c(idx2[,1],anno) ## since 0.2-0, only one code label is added to file widget.
                addidx <-  data.frame(selfirst=apply(outer(allidx,idx1$selfirst,"<="),2,sum),
                                      selend=apply(outer(allidx,idx1$selend,"<="),2,sum))
                idx1 <- idx1+addidx
                HL(.rqda$.openfile_gui,index=idx1,fore.col=.rqda$fore.col,back.col=NULL)
            }
        }# end of mark text chuck
    }
}


HL_CodingWithMemo <- function(codingTable="coding"){
  if (is_projOpen(envir=.rqda,conName="qdacon")){
    SelectedFile <- tryCatch(svalue(.rqda$.root_edit),error=function(e){})
    if (!is.null(SelectedFile)) {
      SelectedFile <- enc(SelectedFile,encoding="UTF-8")
      currentFid <-  RQDAQuery(sprintf("select id from source where name='%s'",SelectedFile))[,1]
      tryCatch({
        widget <- .rqda$.openfile_gui
        idx <-  RQDAQuery(sprintf("select selfirst, selend,memo from %s where fid=%i and status=1",codingTable, currentFid))
        if (nrow(idx)!=0){
          ClearMark(widget,min=0,max=max(as.numeric(idx$selend))+2*nrow(idx),clear.fore.col = TRUE, clear.back.col =FALSE)
          anno <- RQDAQuery(sprintf("select position from annotation where status=1 and fid=%s",currentFid))$position
          ## allidx <- unlist(idx[,c("selfirst","selend")])
          allidx <- c(idx[,c("selfirst")],anno)
          addidx <-  data.frame(selfirst=apply(outer(allidx,idx$selfirst,"<="),2,sum),
                                selend=apply(outer(allidx,idx$selend,"<="),2,sum))
          idx[,c("selfirst","selend")] <- idx[,c("selfirst","selend")] + addidx
          idx1 <- idx[(idx$memo!="") & (!is.na(idx$memo)),c("selfirst","selend")]
          HL(widget,index=idx1,fore.col=.rqda$fore.col,back.col=NULL)
        }
      },error=function(e){}) # end of mark text chuck
    }}}

HL_AllCodings <- function(codingTable="coding") {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        SelectedFile <- tryCatch(svalue(.rqda$.root_edit),error=function(e){NULL})
        if (!is.null(SelectedFile)) {
            currentFid <-  RQDAQuery(sprintf("select id from source where name='%s'",enc(SelectedFile,"UTF-8")))[,1]
            idx <- RQDAQuery(sprintf("select selfirst,selend from %s where fid=%i and status=1",codingTable,currentFid))
            if ((N <- nrow(idx)) != 0){
                anno <- RQDAQuery(sprintf("select position from annotation where status=1 and fid=%s",currentFid))$position
                idx1 <- c(idx$selfirst,anno)
                idx1 <- idx1 + rank(idx1)
                idx2 <- c(idx$selend,anno)
                idx2 <- idx2 + rank(idx2)
                idx <-data.frame(idx1,idx2)
                ClearMark(.rqda$.openfile_gui ,0 , max(idx2))
                HL(.rqda$.openfile_gui,index=idx)
            }
        }
    }
}


##addAnnoTable <- function(){
##  tryCatch(
##  RQDAQuery("create table annotation (fid integer,position integer,annotation text, owner text, date text,dateM text, ##status integer)"),error=function(e){})
##} ##RQDAQuery("drop table annotation")

NextRowId <- function(table){
  ans <- RQDAQuery(sprintf("select max(rowid)+1 as nextid from %s",table))$nextid
  if (is.na(ans)) ans <- 1
  ans
}

InsertAnnotation <- function (index,fid,rowid,label="[Annotation]",AnchorPos=NULL)
  {
    widget=.rqda$.openfile_gui
    lab <- gtkLabelNew(label)
    label <- gtkEventBoxNew()
    label$ModifyBg("normal", gdkColorParse("yellow")$color)
    label$Add(lab)
    buffer <- slot(widget, "widget")@widget$GetBuffer()
    button_press <- function(widget, event,moreArgs) {
      openAnnotation(New=FALSE,pos=moreArgs$pos,fid=moreArgs$fid,rowid=moreArgs$rowid)
      enabled(button$savAnnB) <- FALSE
    }
    gSignalConnect(label, "button-press-event", button_press,data = list(pos=index,fid=fid,rowid=rowid))
    if (is.null(AnchorPos)) AnchorPos <- index
    iter <- gtkTextBufferGetIterAtOffset(buffer, AnchorPos)$iter
    buffer$CreateMark(mark.name=sprintf("%s.3",rowid),where=iter)
    anchorcreated <- buffer$createChildAnchor(iter)
    iter$BackwardChar()
    anchor <- iter$getChildAnchor()
    anchor <- gtkTextIterGetChildAnchor(iter)
    widget@widget@widget$addChildAtAnchor(label, anchor)
  } ## end of helper widget

DeleteAnnotationAnchorByMark <- function(markname){
  buffer <- .rqda$.openfile_gui@widget@widget$GetBuffer()
  mark <- buffer$GetMark(markname)
  buffer$GetIterAtMark(mark)
  offset2 <- buffer$GetIterAtMark(mark)$iter$GetOffset()
  offset1 <- offset2 - 1
  iter2 <- buffer$GetIterAtOffset(offset2)$iter
  iter1 <- buffer$GetIterAtOffset(offset1)$iter
  buffer$Delete(iter1,iter2)
}


openAnnotation <- function(New=TRUE,pos,fid,rowid,AnchorPos=NULL){
    tryCatch(dispose(.rqda$.annotation),error=function(e) {})
    wnh <- size(.rqda$.root_rqdagui)
    .annotation <- gwindow(title="Annotation",parent=c(wnh[1]+10,2),
                           width = min(c(gdkScreenWidth()- wnh[1]-20,getOption("widgetSize")[1])),
                           height = min(c(wnh[2],getOption("widgetSize")[2]))
                           )
    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    .annotation@widget@widget$SetIconFromFile(mainIcon)
    assign(".annotation",.annotation, envir=.rqda)
    .annotation2 <- gpanedgroup(horizontal = FALSE, container=.annotation)
    savAnnB <-
    gbutton("Save Annotation",container=.annotation2,handler=function(h,...){
        newcontent <- svalue(W)
        newcontent <- enc(newcontent,encoding="UTF-8")
        if (newcontent != ""){
            if (New) {
                if (is.null(AnchorPos)) AnchorPos <- pos
                InsertAnnotation(index=pos,fid=fid,rowid=rowid,AnchorPos=AnchorPos)
                RQDAQuery(sprintf("insert into annotation (fid,position,annotation,owner,date,status) values (%i,%i,'%s','%s','%s',1)", fid,pos,newcontent,.rqda$owner,date()))
                New <<- FALSE ## note the replacement <<-
            } else {
                ## RQDAQuery(sprintf("update annotation set annotation='%s' where fid=%i and position=%s and status=1", newcontent,fid,pos))
                RQDAQuery(sprintf("update annotation set annotation='%s' where rowid=%s and status=1", newcontent,rowid))
            }
        } else {## action for empty new content.
            tryCatch(DeleteAnnotationAnchorByMark(sprintf("%s.3",rowid)),error=function(e){})
            ## RQDAQuery(sprintf("update annotation set annotation='%s' where fid=%i and position=%s and status=1", newcontent,fid,pos))
            RQDAQuery(sprintf("update annotation set annotation='%s' where rowid=%s and status=1", newcontent,rowid))
            ## RQDAQuery(sprintf("update annotation set status=0 where fid=%i and position=%s and status=1",fid,pos))
            RQDAQuery(sprintf("update annotation set status=0 where rowid=%s and status=1",rowid))
        }
        enabled(savAnnB) <- FALSE
    }
            )## end of save button
    enabled(savAnnB) <- FALSE
    assign("savAnnB", savAnnB, envir=button)
    assign(".annotationContent",gtext(container=.annotation2,font.attr=c(sizes="large")),envir=.rqda)
    ## prvcontent <- RQDAQuery(sprintf("select annotation from annotation where fid=%i and position=%s and status=1",fid,pos))[1,1]
    prvcontent <- RQDAQuery(sprintf("select annotation from annotation where rowid=%s and status=1",rowid))[1,1]
    if (is.null(prvcontent) || is.na(prvcontent)) prvcontent <- ""
    Encoding(prvcontent) <- "UTF-8"
    W <- get(".annotationContent",envir=.rqda)
    gSignalConnect(W@widget@widget$GetBuffer(), "changed",
                   function(h,...){
                       mbut <- get("savAnnB",envir=button)
                       enabled(mbut) <- TRUE
                   }
                   )##
    add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
}

Annotation <- function(...){
  if (is_projOpen(envir=.rqda,conName="qdacon")) {
    W <- tryCatch( get(".openfile_gui",envir=.rqda), error=function(e){})
    ## get the widget for file display. If it does not exist, then return NULL.
    pos <- tryCatch(sindex(W,includeAnchor=FALSE),error=function(e) {}) ## if the not file is open, it doesn't work.
    if (is.null(pos)) {gmessage("Open a file first!",container=TRUE)}
    else {
      AnchorPos <- sindex(W,includeAnchor=TRUE)$startN
      SelectedFile <- svalue(.rqda$.root_edit)
      SelectedFile <- enc(SelectedFile,encoding="UTF-8")
      currentFid <-  RQDAQuery(sprintf("select id from source where name='%s'",SelectedFile))[,1]
      idx <- RQDAQuery(sprintf("select fid, annotation,rowid from annotation where fid=%i and position=%s and status=1",currentFid,pos$startN))
      New <- ifelse(nrow(idx)==0,TRUE,FALSE)
      if (nrow(idx)==0) rowid <- NextRowId("annotation") else rowid <- idx$rowid
      openAnnotation(New=New,pos=pos$startN,fid=currentFid,rowid=rowid,AnchorPos=AnchorPos)
    }
  }
}

CodeWithCoding <- function(condition = c("unconditional", "case", "filecategory","both"),
                           codingTable="coding"){
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        condition <- match.arg(condition)
        fid <- GetFileId(condition,"coded")
        if (length(fid)!=0){
            ans <- unlist(RQDAQuery(sprintf("select name from freecode where status=1 and id in (select cid from %s where status=1 and fid in (%s) group by cid)",codingTable, paste(shQuote(fid),collapse=","))))
            Encoding(ans) <- "UTF-8"
            .rqda$.codes_rqda[] <- ans
            invisible(ans)
        }}}

CodeWithoutCoding <- function(condition = c("unconditional", "case", "filecategory","both"),
                              codingTable="coding"){
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        condition <- match.arg(condition)
        fid <- GetFileId(condition,"coded")
        if (length(fid)!=0){
            ans <- unlist(RQDAQuery(sprintf("select name from freecode where status=1 and id not in
(select cid from %s where status=1 and fid in (%s) group by cid)",
codingTable, paste(shQuote(fid),collapse=","))))
            Encoding(ans) <- "UTF-8"
            .rqda$.codes_rqda[] <- ans
            invisible(ans)
        }
}
}

AddToCodeCategory <- function (Widget = .rqda$.codes_rqda, updateWidget = TRUE)
{
  codename2 <- svalue(Widget)
  codename <- enc(codename2)
  query <- dbGetQuery(.rqda$qdacon, sprintf("select id, name from freecode where name in(%s) and status=1",
                                            paste("'", codename, "'", sep = "", collapse = ",")))
  cid <- query$id
  Encoding(query$name) <- "UTF-8"
  CodeCat <- RQDAQuery(sprintf("select name, catid from codecat where status=1 and catid not in (select catid from treecode where status=1 and cid in (%s) group by catid)", paste("'", cid, "'", sep = "", collapse = ",")))
  if (nrow(CodeCat) == 0) {
    gmessage("Add Code Categroy First.", container=TRUE)
  }
  else {
    Encoding(CodeCat$name) <- "UTF-8"
    Selecteds <- gselect.list(CodeCat$name, multiple = TRUE,x=getOption("widgetCoordinate")[1])
    if (length(Selecteds) > 0 && Selecteds != "") {
      Encoding(Selecteds) <- "UTF-8"
      for (Selected in Selecteds) {
        CodeCatid <- CodeCat$catid[CodeCat$name %in% Selected]
        exist <- dbGetQuery(.rqda$qdacon, sprintf("select cid from treecode where status=1 and cid in (%s) and catid=%i", paste("'", cid, "'", sep = "", collapse = ","), CodeCatid)) ## this check is unnecessary
        if (nrow(exist) != length(cid)) {
          DAT <- data.frame(cid = cid[!cid %in% exist$cid],
                            catid = CodeCatid, date = date(), dateM = date(),
                            memo = "", status = 1, owner=.rqda$owner)
          success <- dbWriteTable(.rqda$qdacon, "treecode",
                                  DAT, row.name = FALSE, append = TRUE)
          if (success && updateWidget) {
            UpdateCodeofCatWidget()
          }
          if (!success)
            gmessage(sprintf("Fail to write to code category of %s",
                             Selected))
        }
      }
    }
  }
}

## c2InfoFun <- function(){
##   con <- .rqda$qdacon
##   if (is_projOpen(envir=.rqda,conName="qdacon")) {
##     W <- tryCatch(get(".openfile_gui",envir=.rqda), error=function(e){})
## ## get the widget for file display. If it does not exist, then return NULL.
## sel_index <- tryCatch(sindex(W,includeAnchor=FALSE),error=function(e) {})
## ## if the not file is open, it doesn't work.
## if (is.null(sel_index)) {gmessage("Open a file first!",container=TRUE)}
## else {
## CodeTable <-  dbGetQuery(con,"select id,name from freecode where status==1")
## SelectedFile <- svalue(.rqda$.root_edit); Encoding(SelectedFile) <- "UTF-8" ##file title
## currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",SelectedFile))[,1]
## codings_index <-  RQDAQuery(sprintf("select rowid, cid, fid, selfirst, selend from coding where fid==%i ", currentFid))
## ## should only work with those related to current code and current file.
## rowid <- codings_index$rowid[(codings_index$selfirst >= sel_index$startN) &
##                              (codings_index$selend  <= sel_index$endN)
##                              ] ## determine which codes correspond to the selection
## cid <- codings_index$cid[codings_index$rowid %in% rowid]
## Codes <- CodeTable$name[CodeTable$id %in% cid]
## ## should not use data frame as x, otherwise, svalue(c2infoWidget) is a factor rather than a character
## if (length(Codes)!=0){
##   Encoding(Codes) <- "UTF-8"
##   tryCatch(dispose(.rqda$.c2info),error=function(e){})
##   gw <- gwindow(title="Associted code-list.",heigh=min(33*length(Codes),600),parent=.rqda$.openfile_gui)
##   c2infoWidget <- gtable(Codes,container=gw)
##   assign(".c2info",gw,envir=.rqda)
##   addhandlerdoubleclick(c2infoWidget,handler=function(h,...) retrieval2(CodeNameWidget=c2infoWidget))
##   addHandlerClicked(c2infoWidget,handler <- function(h,...){ClickHandlerFun(CodeNameWidget=c2infoWidget)})
## }
## }}}


## InsertAnchor <- function(widget,label,index,handler=FALSE,label.col="gray90",
##                          forward=TRUE){ ## forward is used only when handler is TRUE
##   ## rev 233
##     lab <- gtkLabelNew(label)
##     label <- gtkEventBoxNew()
##     if (isTRUE(handler)) label$ModifyBg("normal", gdkColorParse(label.col)$color)
##     label$Add(lab)
##     buffer <- slot(widget,"widget")@widget$GetBuffer()
##     if (isTRUE(handler)){
##       button_press <-function(widget,event,W){
##         Iter <- gtkTextBufferGetIterAtChildAnchor(buffer,anchor)$iter
##         Offset <- Iter$GetOffset()
##         label <- lab$GetLabel()
##         if (forward) {
##           label <- gsub("<$","",label)
##           Succeed <- FALSE
##           while (!Succeed){
##             if (! Iter$ForwardChar()) Succeed <- TRUE
##             Anchor <- Iter$getChildAnchor()
##             if (!is.null(Anchor)){
##               lab <- Anchor$GetWidgets()[[1]][["child"]]$GetLabel()##Anchor is event box.
##               lab <- gsub("^>","",lab)
##               if (lab==label){
##                 Succeed <- TRUE
##                 maxidx <- buffer$GetBounds()$end$GetOffset()
##                 ClearMark(W,min=0,max=maxidx)
##                 Offset2 <- Iter$GetOffset()
##                 HL(W=W, index=data.frame(Offset,Offset2))
##               }}}} else {
##                 label <- gsub("^>","",label)
##                 Succeed <- FALSE
##                 while (!Succeed){
##                   if (! Iter$BackwardChar()) Succeed <- TRUE
##                   Anchor <- Iter$getChildAnchor()
##                   if (!is.null(Anchor)){
##                     lab <- Anchor$GetWidgets()[[1]][["child"]]$GetLabel()
##                     lab <- gsub("<$","",lab)
##                     if (lab==label){
##                       Succeed <- TRUE
##                       maxidx <- buffer$GetBounds()$end$GetOffset()
##                       ClearMark(W,min=0,max=maxidx)
##                       Offset2 <- Iter$GetOffset()
##                       HL(W=W, index=data.frame(Offset2,Offset)) ## note the offset2 comes first
##                     }}}}
##       }
##     gSignalConnect(label, "button-press-event",button_press,data=widget)}
##     iter <- gtkTextBufferGetIterAtOffset(buffer,index)$iter
##     anchorcreated <- buffer$createChildAnchor(iter)
##     iter$BackwardChar()
##     anchor <- iter$getChildAnchor()
##     anchor <- gtkTextIterGetChildAnchor(iter)
##     widget@widget@widget$addChildAtAnchor(label, anchor)
## }


