ImportFile <- function(path,encoding=.rqda$encoding,con=.rqda$qdacon,...){
  ## import a file into a DBI connection _con_.
  Fname <- gsub("\\.[[:alpha:]]*$","",basename(path))## Fname is in locale Encoding Now.
  FnameUTF8 <- iconv(Fname,to="UTF-8")
  ## remove the suffix such as .txt
  if ( Fname!="" ) {
    file_con <- file(path,open="r")
    if (isTRUE(.rqda$BOM)) seek(file_con,3)
    content <- readLines(file_con,warn=FALSE,encoding=encoding)
    close(file_con)
    content <- paste(content,collapse="\n")
    content <- enc(content,encoding=Encoding(content))
    if (Encoding(content)!="UTF-8"){
      content <- iconv(content,to="UTF-8") ## UTF-8 file content
    }
    maxid <- dbGetQuery(con,"select max(id) from source")[[1]]
    nextid <- ifelse(is.na(maxid),0+1, maxid+1)
    write <- FALSE
    ## check if the content should be written into con.
    if (nextid==1) {
      write <- TRUE
      ## if this is the first file, no need to worry about the duplication issue.
    } else {
      if (nrow(dbGetQuery(con,sprintf("select name from source where name='%s'",FnameUTF8)))==0) {
        ## no duplication file exists, then write.
        write <- TRUE
      } else {
        gmessage(gettext("A file with the same name exists in the database!", domain = "R-RQDA"))
      }
    }
    if (write ) {
      dbGetQuery(con,sprintf("insert into source (name, file, id, status,date,owner )
                             values ('%s', '%s',%i, %i, '%s', '%s')",
                             Fname,content, nextid, 1,date(),.rqda$owner))
    }
  }
}


FileNamesUpdate <- function(FileNamesWidget=.rqda$.fnames_rqda,sortByTime=TRUE,decreasing = FALSE,...){
  ##update file names list in the FileNamesWidget
  wopt <- options(warn=-2)
  on.exit(options(wopt))
  source <- dbGetQuery(.rqda$qdacon, "select name, date, id from source where status=1 order by lower(name)")
  if (nrow(source)!=0) {
    fnames <- source$name
    Encoding(fnames) <- "UTF-8"
    if (sortByTime){
      fnames <- fnames[OrderByTime(source$date,decreasing=decreasing)]
    }
    tryCatch(FileNamesWidget[] <- fnames,error=function(e){})
  }
}

LineNumber.expose <- function(da,event,data){
    ## translated from http://www.pygtk.org/pygtk2tutorial/sec-TextViewExample.html
    textView <- da
    textView$SetBorderWindowSize('GTK_TEXT_WINDOW_LEFT',30)
    vis <- textView$GetVisibleRect()
    heightVis <- vis$visible.rect$height
    firstY <- vis$visible.rect$y
    lastY <- firstY + heightVis
    posFirst <- gtkTextViewWindowToBufferCoords(textView, 'GTK_TEXT_WINDOW_LEFT', 0, firstY )
    posLast <- gtkTextViewWindowToBufferCoords(textView, 'GTK_TEXT_WINDOW_LEFT', 0, lastY)
    windowL <- textView$GetWindow('GTK_TEXT_WINDOW_LEFT')
    atTop <- textView$GetLineAtY(firstY)
    iter  <- atTop$target.iter
    top <- atTop$line.top
    count <- 0
    pixels <- numbers <- c()
    while (!iter$IsEnd()) {
        tmp <- textView$GetLineYrange(iter)
        y <- tmp$y
        line_num <- gtkTextViewGetLineAtY(textView,y)$target.iter$GetLine()+1
        numbers <- c(numbers, line_num)
        height <- tmp$height
        count <- count + 1
        pixels <- c(pixels, y)
        if ((y + height) >= lastY) {break}
        iter$ForwardLine()
    }
    pixels <- pixels - min(pixels)

    pango <- gtkWidgetCreatePangoLayout(textView,NULL)
    for (i in 1:count){
        pango$SetText(as.character(numbers[i]))
        gtkPaintLayout(textView$Style,windowL,textView$State(),FALSE,NULL,widget=textView,x=2,y=pixels[i],layout=pango)
    }
}


ViewFileFun <- function(FileNameWidget,hightlight=TRUE){
    ## FileNameWidget=.rqda$.fnames_rqda in Files Tab
    ## FileNameWidget=.rqda$.FileofCat in F-CAT Tab
    if (is_projOpen(envir = .rqda, conName = "qdacon")) {
        if (length(svalue(FileNameWidget)) == 0) {
            gmessage(gettext("Select a file first.", domain = "R-RQDA"), icon = "error",con = TRUE)
        } else {
            SelectedFileName <- svalue(FileNameWidget)
            ViewFileFunHelper(SelectedFileName,hightlight=TRUE)
        }}}


ViewFileFunHelper <- function(FileName,hightlight=TRUE,codingTable=.rqda$codingTable, annotation=TRUE){
  if (exists(".root_edit",envir=.rqda) && isExtant(.rqda$.root_edit)) {
    dispose(.rqda$.root_edit)
  }
  SelectedFileName <- FileName
  wnh <- size(.rqda$.root_rqdagui)
  if (grepl("apple", R.version$platform)) {
    gw <- gwindow(title = SelectedFileName,parent = c(0, 0),
                  width = min(c(gdkScreenWidth()- wnh[1]-20,getOption("widgetSize")[1])),
                  height = min(c(wnh[2],getOption("widgetSize")[2]))
    )
  } else {
    gw <- gwindow(title = SelectedFileName,parent = wnh, ## .rqda$.root_rqdagui,
                width = min(c(gdkScreenWidth()- wnh[1]-20,getOption("widgetSize")[1])),
                height = min(c(wnh[2],getOption("widgetSize")[2]))
    )
    }
  mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
  gw@widget@widget$SetIconFromFile(mainIcon)
  getToolkitWidget(gw)$Move(getOption("widgetCoordinate")[1],getOption("widgetCoordinate")[2])
  assign(".root_edit", gw, envir = .rqda)
  .root_edit <- get(".root_edit", .rqda)
  tmp <- gtext(container=.root_edit)
  font <- pangoFontDescriptionFromString(.rqda$font)
  gtkWidgetModifyFont(tmp@widget@widget,font)
  tmp@widget@widget$SetPixelsBelowLines(5) ## set the spacing
  tmp@widget@widget$SetPixelsInsideWrap(5) ## so the text looks more confortable.
  assign(".openfile_gui", tmp, envir= .rqda)
  Encoding(SelectedFileName) <- "unknown"
  IDandContent <- RQDAQuery(sprintf("select id, file from source where name='%s'",
                                    enc(SelectedFileName))
                            )
  content <- IDandContent$file
  Encoding(content) <- "UTF-8"
  W <- get(".openfile_gui", .rqda)
  add(W, content)
  slot(W, "widget")@widget$SetEditable(FALSE)
  markidx <- RQDAQuery(sprintf("select %s.rowid,selfirst,selend,freecode.name,freecode.color, freecode.id from %s,freecode where fid=%i and %s.status=1 and freecode.id=cid and freecode.status=1",codingTable,codingTable, IDandContent$id,codingTable))
  if (annotation) {
      anno <- RQDAQuery(sprintf("select position,rowid from annotation where status=1 and fid=%s",IDandContent$id))
  }
  buffer <- W@widget@widget$GetBuffer()
  fore.col <- .rqda$fore.col
  back.col <- .rqda$back.col
  buffer$createTag("underline", underline = "single")
  buffer$createTag(fore.col,foreground = fore.col)
  buffer$createTag(sprintf("%s.background",back.col),background = back.col)
  ## create buffer tag, which is created by defualt since gwidgetRGtk2 changes its API
  N <- nrow(markidx)
  if (nrow(markidx)!=0){ ## make sense only when there is coding there
      for (i in 1:N){
          iter <- gtkTextBufferGetIterAtOffset(buffer, markidx[i,"selfirst"]) ## index to iter
          buffer$CreateMark(sprintf("%s.1",markidx[i,"rowid"]),where=iter$iter) ## insert marks
          iter <- gtkTextBufferGetIterAtOffset(buffer, markidx[i,"selend"])
          buffer$CreateMark(sprintf("%s.2",markidx[i, "rowid"]),where=iter$iter)
          ## the second iter is used to HL coding
      }
  } ## create marks
  if (annotation){
      if (nrow(anno)!=0){
          for (i in 1:nrow(anno)) {
              iter <- gtkTextBufferGetIterAtOffset(buffer, anno[i,"position"]) ## index to iter
              buffer$CreateMark(sprintf("%s.3",anno[i,"rowid"]),where=iter$iter) ## insert marks
          }} ## creat marks for annotation
  }
  if (nrow(markidx)!=0){
    sapply(markidx[, "rowid"], FUN = function(x) {
      code <- markidx[markidx$rowid == x, "name"]
      Encoding(code) <- "UTF-8"
      codeColor <- markidx[markidx$rowid == x, "color"]
      if (is.na(codeColor)) {
        codeColor <-  DefaultCodeColor[as.numeric(markidx[markidx$rowid == x, "id"]) %% length(DefaultCodeColor)+1]
      }
      m1 <- buffer$GetMark(sprintf("%s.1", x))
      iter1 <- buffer$GetIterAtMark(m1)
      idx1 <- gtkTextIterGetOffset(iter1$iter)
      m2 <- buffer$GetMark(sprintf("%s.2", x))
      iter2 <- buffer$GetIterAtMark(m2)
      idx2 <- gtkTextIterGetOffset(iter2$iter)
      InsertAnchor(.rqda$.openfile_gui, label = sprintf("<%s>",code), index = idx1, label.col=codeColor,
                  handler=TRUE, EndMarkName=sprintf("%s.2", x))
    }) ## end of sapply -> insert code label
    if (hightlight){
      idx <- sapply(markidx[, "rowid"], FUN = function(x) {
        m1 <- buffer$GetMark(sprintf("%s.1", x))
        iter1 <- buffer$GetIterAtMark(m1)
        idx1 <- gtkTextIterGetOffset(iter1$iter)
        m2 <- buffer$GetMark(sprintf("%s.2", x))
        iter2 <- buffer$GetIterAtMark(m2)
        idx2 <- gtkTextIterGetOffset(iter2$iter)
        return(c(idx1,idx2))
      })## get offset for HL.
      idx <- t(idx)
      HL(W, idx, fore.col = .rqda$fore.col, back.col = NULL)
    }}
  if (annotation) {
      if (nrow(anno)!=0){
          apply(anno,1,function(x){
              m <- buffer$GetMark(sprintf("%s.3", x["rowid"]))
              iter <- buffer$GetIterAtMark(m)
              idx <- gtkTextIterGetOffset(iter$iter)
              InsertAnnotation(index=idx,fid=IDandContent$id, rowid=x["rowid"])
          })}}
  buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter) ## place cursor at the beginning
  ## gSignalConnect(tmp@widget@widget,"expose_event",LineNumber.expose) ## add line number to the widget
  ## does not work well yet
  enabled(button$AnnB) <- TRUE
  enabled(button$MarCodB1) <- (length(svalue(.rqda$.codes_rqda))==1)
  ## enabled(button$UnMarB1) <- (length(svalue(.rqda$.codes_rqda))==1)
  enabled(button$MarCodB2) <- (length(svalue(.rqda$.CodeofCat))==1)
  enabled(button$UnMarB2) <- (length(svalue(.rqda$.CodeofCat))==1)
  ## enabled(button$c2memobutton) <- TRUE
  addHandlerUnrealize(gw, handler = function(h,...) {
    enabled(button$AnnB) <- FALSE
    enabled(button$MarCodB1) <- FALSE
    enabled(button$UnMarB1) <- FALSE
    enabled(button$MarCodB2) <- FALSE
    enabled(button$UnMarB2) <- FALSE
    ## enabled(button$c2memobutton) <- FALSE
    enabled(button$CasMarB) <- FALSE
    enabled(button$CasUnMarB) <- FALSE
    return(FALSE)
  })
}


EditFileFun <- function(FileNameWidget=.rqda$.fnames_rqda){
  ## FileNameWidget=.rqda$.fnames_rqda in Files Tab
  ## FileNameWidget=.rqda$.FileofCat in F-CAT Tab
  if (is_projOpen(envir=.rqda, conName = "qdacon")) {
    SelectedFileName <- svalue(FileNameWidget)
    if (length(svalue(FileNameWidget)) == 0) {
      gmessage(gettext("Select a file first.", domain = "R-RQDA"), icon = "error", con = TRUE)
    }
    else {
      tryCatch(dispose(.rqda$.root_edit),error=function(e) {})
      gw <- gwindow(title=SelectedFileName,parent=getOption("widgetCoordinate"),
                    width = getOption("widgetSize")[1], height = getOption("widgetSize")[2]
                    )
      mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
      gw@widget@widget$SetIconFromFile(mainIcon)
      assign(".root_edit",gw,envir=.rqda)
      assign(".root_edit2",gpanedgroup(horizontal = FALSE, container=.rqda$.root_edit),envir=.rqda)
      EdiFilB <- gbutton(gettext("Save File", domain = "R-RQDA"),container=.rqda$.root_edit2,handler=function(h,...){
        content <-  svalue(.rqda$.openfile_gui)
        RQDAQuery(sprintf("update source set file='%s', dateM='%s' where name='%s'",
                          enc(content,"UTF-8"),date(),enc(svalue(.rqda$.root_edit),"UTF-8"))) ## update source table
        if (nrow(mark_index)!=0){ ## only manipulate the coding when there is one.
          idx <- apply(mark_index, 1, FUN = function(x) {
            m1 <- buffer$GetMark(sprintf("%s.1", x[3]))
            iter1 <- buffer$GetIterAtMark(m1)
            idx1 <- gtkTextIterGetOffset(iter1$iter)
            m2 <- buffer$GetMark(sprintf("%s.2", x[3]))
            iter2 <- buffer$GetIterAtMark(m2)
            idx2 <- gtkTextIterGetOffset(iter2$iter)
            ans <- c(selfirst = idx1, selend = idx2,x[3])## matrix of 3x N (N=nrow(mark_index))
          }) ## end of apply
          apply(idx,2,FUN=function(x){
            if (x[1]==x[2])  RQDAQuery(sprintf("update coding set status=0 where rowid=%i",x[3])) else {
              Encoding(content) <- "UTF-8"
              RQDAQuery(sprintf("update coding set seltext='%s',selfirst=%i, selend=%i where rowid=%i",
                                enc(substr(content,x[1],x[2]),"UTF-8"),x[1],x[2],x[3]))
            }
          })## update the coding table (seltext,selfirst, selend), on the rowid (use rowid to name the marks)
        }

        if (nrow(mark_indexS)!=0){ ## only manipulate coding2
          idxS <- apply(mark_indexS, 1, FUN = function(x) {
            m1 <- buffer$GetMark(sprintf("%s.1S", x[3]))
            iter1 <- buffer$GetIterAtMark(m1)
            idx1 <- gtkTextIterGetOffset(iter1$iter)
            m2 <- buffer$GetMark(sprintf("%s.2S", x[3]))
            iter2 <- buffer$GetIterAtMark(m2)
            idx2 <- gtkTextIterGetOffset(iter2$iter)
            ans <- c(selfirst = idx1, selend = idx2,x[3])## matrix of 3x N (N=nrow(mark_index))
          }) ## end of apply
          apply(idxS,2,FUN=function(x){
            if (x[1]==x[2])  RQDAQuery(sprintf("update coding2 set status=0 where rowid=%i",x[3])) else {
              Encoding(content) <- "UTF-8"
              RQDAQuery(sprintf("update coding2 set seltext='%s',selfirst=%i, selend=%i where rowid=%i",
                                enc(substr(content,x[1],x[2]),"UTF-8"),x[1],x[2],x[3]))
            }
          })
        } ## end of updating coding2

        if (nrow(mark_idx_case)!=0){
          idx_case <- apply(mark_idx_case, 1, FUN = function(x) {
            m1 <- buffer$GetMark(sprintf("c%s.1", x["rowid"]))
            iter1 <- buffer$GetIterAtMark(m1)
            idx1 <- gtkTextIterGetOffset(iter1$iter)
            m2 <- buffer$GetMark(sprintf("c%s.2", x["rowid"]))
            iter2 <- buffer$GetIterAtMark(m2)
            idx2 <- gtkTextIterGetOffset(iter2$iter)
            ans <- c(selfirst = idx1, selend = idx2,x["rowid"])
          }) ## end of apply
          apply(idx_case,2,FUN=function(x){
            if (x[1]==x[2])  RQDAQuery(sprintf("update caselinkage set status=0 where rowid=%i",x["rowid"])) else {
              RQDAQuery(sprintf("update caselinkage set selfirst=%i, selend=%i where rowid=%i",x[1],x[2],x[3]))
            }
          })## end of apply
        }
        enabled(button$EdiFilB) <- FALSE
      })## end of save button

      assign("EdiFilB",EdiFilB,envir=button)
      enabled(EdiFilB) <- FALSE
      tmp <- gtext(container=.rqda$.root_edit2)
      font <- pangoFontDescriptionFromString(.rqda$font)
      gtkWidgetModifyFont(tmp@widget@widget,font)
      assign(".openfile_gui", tmp, envir= .rqda)
      Encoding(SelectedFileName) <- "unknown"
      IDandContent <- dbGetQuery(.rqda$qdacon, sprintf("select id, file from source where name='%s'",enc(SelectedFileName)))
      content <- IDandContent$file
      Encoding(content) <- "UTF-8"
      W <- get(".openfile_gui", .rqda)
      ## add(W, content, font.attr = c(sizes = "large"))
      add(W, content)
      buffer <- slot(W, "widget")@widget$GetBuffer() ## get text buffer.
      mark_index <- dbGetQuery(.rqda$qdacon,sprintf("select selfirst,selend,rowid from coding where fid=%i and status=1",
                                                    IDandContent$id))
      if (nrow(mark_index)!=0){## make sense only when there is coding there
        ClearMark(W ,0 , max(mark_index$selend),TRUE,FALSE)
        HL(W,index=mark_index[,c("selfirst","selend")],.rqda$fore.col,NULL)
        ## insert marks according to mark_index (use rowid to name the marks)
        apply(mark_index,1,function(x){
          iter <- gtkTextBufferGetIterAtOffset(buffer, x[1]) ## index to iter
          mark <- buffer$CreateMark(sprintf("%s.1",x[3]),where=iter$iter)         ## insert marks
          ## gtkTextMarkSetVisible(mark,TRUE)                   ## set itvisible
          iter <- gtkTextBufferGetIterAtOffset(buffer, x[2]) ## index to iter
          mark <- buffer$CreateMark(sprintf("%s.2",x[3]),where=iter$iter)         ## insert marks
          ## gtkTextMarkSetVisible(mark,TRUE)                   ## set itvisible
        }) ## end of apply
      }

      mark_indexS <- dbGetQuery(.rqda$qdacon,sprintf("select selfirst,selend,rowid from coding2 where fid=%i and status=1",IDandContent$id))
      if (nrow(mark_indexS)!=0){
        apply(mark_indexS,1,function(x){
          iter <- gtkTextBufferGetIterAtOffset(buffer, x[1]) ## index to iter
          mark <- buffer$CreateMark(sprintf("%s.1S",x[3]),where=iter$iter)
          iter <- gtkTextBufferGetIterAtOffset(buffer, x[2]) ## index to iter
          mark <- buffer$CreateMark(sprintf("%s.2S",x[3]),where=iter$iter)
        }) ## end of apply
      }

      mark_idx_case<- dbGetQuery(.rqda$qdacon,sprintf("select selfirst,selend,rowid from caselinkage where fid=%i and status=1",
                                                      IDandContent$id))
      if (nrow(mark_idx_case)!=0){
        ClearMark(W ,0 , max(mark_idx_case$selend),FALSE,TRUE)
        HL(W,index=mark_idx_case[,c("selfirst","selend")],NULL,.rqda$back.col)
        apply(mark_idx_case,1,function(x){
          iter <- gtkTextBufferGetIterAtOffset(buffer, x["selfirst"])
          mark <- buffer$CreateMark(sprintf("c%s.1",x["rowid"]),where=iter$iter)
          gtkTextMarkSetVisible(mark,TRUE)
          iter <- gtkTextBufferGetIterAtOffset(buffer, x["selend"])
          mark <- buffer$CreateMark(sprintf("c%s.2",x["rowid"]),where=iter$iter)
          gtkTextMarkSetVisible(mark,TRUE)
        }) ## end of apply
      }
      gSignalConnect(.rqda$.openfile_gui@widget@widget$GetBuffer(), "changed",
                     function(h,...){
                         enabled(button$EdiFilB) <- TRUE
                     })
      addhandlerunrealize(.rqda$.openfile_gui,handler=function(h,...){
          rm("EdiFilB",envir=button)
          rm(".root_edit",".root_edit2",".openfile_gui",envir=.rqda)
          FALSE
      })
    } ## end of else
  }
}


write.FileList <- function(FileList,encoding=.rqda$encoding,con=.rqda$qdacon,...){
  ## import a list of files into the source table
  ## FileList is a list of file content, with names(FileList) the name of the files.
  WriteToTable <- function(Fname,content){
      ## helper function
      ## FnameUTF8 <- iconv(Fname,to="UTF-8")
      FnameUTF8 <- enc(Fname, encoding=encoding)
    content <- enc(content,encoding=encoding) ## adjust encoding argument.
    if (Encoding(content)!="UTF-8"){
      content <- iconv(content,to="UTF-8") ## UTF-8 file content
    }
    maxid <- dbGetQuery(con,"select max(id) from source")[[1]]
    nextid <- ifelse(is.na(maxid),0+1, maxid+1)
    write <- FALSE
  ## check if the content should be written into con.
    if (nextid==1) {
      write <- TRUE
      ## if this is the first file, no need to worry about the duplication issue.
    } else {
      if (nrow(dbGetQuery(con,sprintf("select name from source where name='%s'",FnameUTF8)))==0) {
      ## no duplication file exists, then write.
        write <- TRUE
      } else {
        cat(sprintf("%s exists in the database!\n",Fname))
      }
    }
  if (write ) {
    dbGetQuery(con,sprintf("insert into source (name, file, id, status,date,owner )
                             values ('%s', '%s',%i, %i, '%s', '%s')",
                           FnameUTF8,content, nextid, 1,date(),.rqda$owner))
  }
  }
  FileNames <- names(FileList)
  FileNames[FileNames==""] <- as.character(1:sum(FileNames==""))

  if (is_projOpen()) {
    for (i in 1:length(FileList)) {
      WriteToTable(FileNames[i],FileList[[i]])
    }
    FileNamesUpdate(FileNamesWidget=.rqda$.fnames_rqda)
    } else gmessage(gettext("Open a project first.", domain = "R-RQDA"), container=TRUE)
}

addFilesFromDir <- function(dir, pattern = "*.txt$"){
  oldDir <- getwd()
  setwd(dir)
  Files <- list.files(pattern = pattern)
  Contents <- lapply(Files, function(x) paste(readLines(x, warn = FALSE), collasep = "\n"))
  names(Contents) <- Files
  write.FileList(Contents)
  on.exit(setwd(oldDir))
}

ProjectMemoWidget <- function(){
  if (is_projOpen(envir=.rqda,"qdacon")) {
    ## use enviroment, so you can refer to the same object easily, this is the beauty of environment
    ## if project is open, then continue
    tryCatch(dispose(.rqda$.projmemo),error=function(e) {})
    ## Close the open project memo first, then open a new one
    ## .projmemo is the container of .projmemocontent,widget for the content of memo
    assign(".projmemo",
           gwindow(title="Project Memo", parent=c(395,10),
                   width = getOption("widgetSize")[1],
                   height = getOption("widgetSize")[2]
                   ),
           envir=.rqda)
    .projmemo <- get(".projmemo",.rqda)
    .projmemo2 <- gpanedgroup(horizontal = FALSE, container=.projmemo)
    ## use .projmemo2, so can add a save button to it.
    gbutton(gettext("Save memo", domain = "R-RQDA"),container=.projmemo2,handler=function(h,...){
      ## send the new content of memo back to database
      newcontent <- svalue(W)
      ## Encoding(newcontent) <- "UTF-8"
      newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
      dbGetQuery(.rqda$qdacon,sprintf("update project set memo='%s' where rowid=1", ## only one row is needed
                                      newcontent)
                 ## have to quote the character in the sql expression
                 )
    }
            )## end of save memo button
    tmp <- gtext(container=.projmemo2)
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
    }
}



FileNameWidgetUpdate <- function(FileNamesWidget=.rqda$.fnames_rqda,sort=TRUE,decreasing = FALSE,FileId=NULL,...){
  ##update file names list in the FileNamesWidget
  wopt <- options(warn=-2)
  on.exit(options(wopt))
  source <- dbGetQuery(.rqda$qdacon, "select name, date, id from source where status=1")
  if (nrow(source)==0){
    fnames <- NULL
  } else {
    Encoding(source$name) <- "UTF-8"
    if (!is.null(FileId)){
      source <- source[source$id %in% FileId,]
      fnames <- source$name##when FileId is not in source$id, fnames is character(0), still works.
      date <- source$date
    } else{
      fnames <- source$name
      date <- source$date
    }
    if (sort){
      fnames <- fnames[OrderByTime(date,decreasing=decreasing)]
    }
  }
  tryCatch(FileNamesWidget[] <- fnames,error=function(e){})
}


GetFileId <- function(condition=c("unconditional","case","filecategory","both"),type=c("all","coded","uncoded","selected"))
{
  ## helper function
  unconditionalFun <- function(type)
    {
      if (type=="selected"){
        selected <- svalue(.rqda$.fnames_rqda)
        ans <- dbGetQuery(.rqda$qdacon,
                          sprintf("select id from source where status=1 and name in (%s)",
                                  paste(paste("'",enc(selected),"'",sep=""),collapse=",")
                                  ))$id
      } else {
        allfid <- dbGetQuery(.rqda$qdacon,"select id from source where status=1 group by id")$id
        if (type!="all"){
          fid_coded <- dbGetQuery(.rqda$qdacon,"select fid from coding where status=1 group by fid")$fid
        }
        if (type=="all") {
          ans <- allfid
        } else if (type=="coded"){
          ans <- fid_coded
        } else if (type=="uncoded"){
          ans <- allfid[! (allfid %in% fid_coded)]
        }
      }
      ans
    }

  FidOfCaseFun <- function(type){
    if (type=="selected"){
      selected <- svalue(.rqda$.FileofCase)
      ans <- dbGetQuery(.rqda$qdacon,
                        sprintf("select id from source where status=1 and name in (%s)",
                                paste(paste("'",enc(selected),"'",sep=""),collapse=",")
                                ))$id
    } else {
      Selected <- svalue(.rqda$.CasesNamesWidget)
      if (length(Selected)==0){
        ans <- NULL
      } else {
        if (length(Selected)>1) {gmessage(gettext("select one file category only.", domain = "R-RQDA"),container=TRUE)
                                 stop("more than one file categories are selected", domain = "R-RQDA")
                               }
        caseid <- RQDAQuery(sprintf("select id from cases where status=1 and name='%s'",
                                    enc(Selected)))$id
        fidofcase <- RQDAQuery(sprintf("select fid from caselinkage where status=1 and caseid=%i",caseid))$fid
        ##         caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name in (%s)",
        ##                                                  paste(paste("'",Selected,"'",sep=""),collapse=",")))$id
        ##         fidofcase <- dbGetQuery(.rqda$qdacon,sprintf("select fid from caselinkage where status==1 and caseid in (%s)",
        ##                                                     paste(paste("'",caseid,"'",sep=""),collapse=",")))$fid
        ## roll back to rev 90
        allfid <-  unconditionalFun(type=type)
        ans <- intersect(fidofcase,allfid)
      }
    }
    ans
  }

  FidOfCatFun <- function(type){
    if (type=="selected"){
      selected <- svalue(.rqda$.FileofCat)
      ans <- dbGetQuery(.rqda$qdacon,
                        sprintf("select id from source where status=1 and name in (%s)",
                                paste(paste("'",enc(selected),"'",sep=""),collapse=",")
                                ))$id
    }
    allfid <- GetFileIdSets("filecategory","intersect")
    if (type=="all") {ans <- allfid} else {
      codedfid <- RQDAQuery(sprintf("select fid from coding where status=1 and fid in (%s) group by fid",paste(shQuote(allfid),collapse=",")))$fid
      if (type=="coded") {ans <- codedfid}
      if (type=="uncoded") { ans <-  setdiff(allfid,codedfid)}
    }
    ans
  }

  bothFun <- function(type){
    ans <- intersect(GetFileId("case",type),GetFileId("file",type))
    ans
  }

  condition <- match.arg(condition)
  type <- match.arg(type)
  fid <- switch(condition,
                unconditional=unconditionalFun(type=type),
                case=FidOfCaseFun(type=type),
                filecategory=FidOfCatFun(type=type),
                both=bothFun(type=type)
                )
  if (is.null(fid)) fid <- integer(0)
  class(fid) <- c("RQDA.vector","fileId")
  fid
}



GetFileIdSets <- function(set=c("case","filecategory"),relation=c("union","intersect")){
  set <- match.arg(set)
  relation <- match.arg(relation)
  if (set=="case") {
    Selected <- svalue(.rqda$.CasesNamesWidget)
    if (length(Selected)==0){
      ans <- NULL
    } else {
      Selected <- gsub("'", "''", Selected)
      if (relation=="union"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid from caselinkage where status=1 and caseid in (select id from cases where status=1 and name in (%s)) group by fid", paste(paste("'",Selected,"'",sep=""),collapse=",")))$fid
      } else if (relation=="intersect"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid, count(fid) as n from caselinkage where status=1 and caseid in (select id from cases where status=1 and name in (%s)) group by fid having n= %i", paste(paste("'",Selected,"'",sep=""),collapse=","),length(Selected)))$fid
      }
    }
  }## end of set=="case"
  if (set=="filecategory"){
    Selected <- svalue(.rqda$.FileCatWidget)
    if (length(Selected)==0){
      ans <- NULL
    } else {
      Selected <- gsub("'", "''", Selected)
      if (relation=="union"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and catid in (select catid from filecat where status=1 and name in (%s)) group by fid", paste(paste("'",Selected,"'",sep=""),collapse=",")))$fid
      } else if (relation=="intersect"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid, count(fid) as n from treefile where status=1 and catid in (select catid from filecat where status=1 and name in (%s)) group by fid having n= %i", paste(paste("'",Selected,"'",sep=""),collapse=","),length(Selected)))$fid
      }
    }
  } ## end of set=="filecategory"
  if (is.null(ans)) ans <- integer(0)
  class(ans) <- c("RQDA.vector","fileId")
  ans
}

AddToFileCategory <- function(Widget=.rqda$.fnames_rqda,updateWidget=TRUE){
  ## filenames -> fid -> selfirst=0; selend=nchar(filesource)
  filename <- svalue(Widget)
  Encoding(filename) <- "unknown"
  query <- dbGetQuery(.rqda$qdacon,sprintf("select id, file from source where name in(%s) and status=1",paste("'",enc(filename),"'",sep="",collapse=","))) ## multiple fid
  fid <- query$id
  Encoding(query$file) <- "UTF-8"
  ## select a F-cat name -> F-cat id
  Fcat <- dbGetQuery(.rqda$qdacon,"select catid, name from filecat where status=1")
  if (nrow(Fcat)==0){gmessage(gettext("Add File Category first.", domain = "R-RQDA"),container=TRUE)} else{
    Encoding(Fcat$name) <- "UTF-8"
    Selecteds <- gselect.list(Fcat$name,multiple=TRUE)
    if (length(Selecteds)>0 && Selecteds!=""){
      Encoding(Selecteds) <- "UTF-8"
      for (Selected in Selecteds) {
        Fcatid <- Fcat$catid[Fcat$name %in% Selected]
        exist <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and fid in (%s) and catid=%i",paste("'",fid,"'",sep="",collapse=","),Fcatid))
        if (nrow(exist)!=length(fid)){
          ## write only when the selected file associated with specific f-cat is not there
          DAT <- data.frame(fid=fid[!fid %in% exist$fid], catid=Fcatid, date=date(),dateM=date(),memo='',status=1,owner=.rqda$owner)
          ## should pay attention to the var order of DAT, must be the same as that of treefile table
          success <- dbWriteTable(.rqda$qdacon,"treefile",DAT,row.name=FALSE,append=TRUE)
          ## write to caselinkage table
          if (success && updateWidget) {
            UpdateFileofCatWidget()
          }
          if (!success) gmessage(sprintf(gettext("Fail to write to file category of %s", domain = "R-RQDA"),Selected))
        }
      }
    } else {
      invisible(FALSE)
    }
  }
}


## library(RGtk2)
searchWord <- function(str,widget,from=0,col="green", verbose=FALSE){
    tview <- slot(widget,"widget")@widget
    buffer <- tview$GetBuffer()
    Iter0 <- buffer$GetIterAtOffset(from)$iter
    ans <- gtkTextIterForwardSearch(Iter0,str,'GTK_TEXT_SEARCH_VISIBLE_ONLY')
    if (ans$retval) {
        gtkTextViewScrollToIter(tview,ans$match.start,0.47)
        buffer$createTag(sprintf("%s.background",col),background = col)
        buffer$ApplyTagByName(sprintf("%s.background", col),ans$match.start, ans$match.end)
        ans$match.end$GetOffset()
    } else {
        if (verbose) gmessage(gettext("Reach the end.", domain = "R-RQDA"))
        invisible(NULL)
    }
}

SearchButton <- function(widget){
## widget=.rqda$.openfile_gui)
    assign("searchFrom",0,envir=.rqda)
    group <- ggroup(horizontal=FALSE, container=gwindow(width=50,height=20,title="Search a word"))
    kwdW <- gedit("", container=group)
    gbutton(gettext("Search next", domain = "R-RQDA"), container = group,handler=function(h,...){
        if (!is.null(.rqda$searchFrom)){
            str <- svalue(h$action)
            Encoding(str) <- "UTF-8"
            res <- searchWord(str,widget=widget,from=.rqda$searchFrom, verbose=TRUE)
            assign("searchFrom",res,envir=.rqda)
        }},action=kwdW)
     gbutton(gettext("Restart", domain = "R-RQDA"), container = group,handler=function(h,...){
         assign("searchFrom",0,envir=.rqda)
     })
}



viewPlainFile <- function(FileNameWidget=.rqda$.fnames_rqda){
    if (is_projOpen(envir= .rqda, conName = "qdacon")) {
        if (length(svalue(FileNameWidget)) == 0) {
            gmessage(gettext("Select a file first.", domain = "R-RQDA"), icon = "error",con = TRUE)
        } else {
            SelectedFileName <- svalue(FileNameWidget)

  wnh <- size(.rqda$.root_rqdagui) ## size of the main window
  gw <- gwindow(title = SelectedFileName,parent = wnh, ## .rqda$.root_rqdagui,
                width = min(c(gdkScreenWidth()- wnh[1]-20,getOption("widgetSize")[1])),
                height = min(c(wnh[2],getOption("widgetSize")[2]))
                )
  mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
  gw@widget@widget$SetIconFromFile(mainIcon)
  getToolkitWidget(gw)$Move(getOption("widgetCoordinate")[1],getOption("widgetCoordinate")[2])
  tmp <- gtext(container=gw)
  font <- pangoFontDescriptionFromString(.rqda$font)
  gtkWidgetModifyFont(tmp@widget@widget,font)
  tmp@widget@widget$SetPixelsBelowLines(5) ## set the spacing
  tmp@widget@widget$SetPixelsInsideWrap(5) ## so the text looks more confortable.
  Encoding(SelectedFileName) <- "unknown"
  IDandContent <- RQDAQuery(sprintf("select id, file from source where name='%s'",
                                    enc(SelectedFileName))
                            )
  content <- IDandContent$file
  Encoding(content) <- "UTF-8"
  add(tmp, content)
  slot(tmp, "widget")@widget$SetEditable(FALSE)
}}}

## UncodedFileNamesUpdate <- function(FileNamesWidget = .rqda$.fnames_rqda, sort=TRUE, decreasing = FALSE){
## replaced by the general function of FileNameWigetUpdate() and GetFileId()
## ## only show the uncoded file names in the .rqda$.fnames_rqda
## ## The fnames will be sort if sort=TRUE
##   fid <- dbGetQuery(.rqda$qdacon,"select id from source where status==1 group by id")$id
##   if (!is.null(fid)){
##     fid_coded <- dbGetQuery(.rqda$qdacon,"select fid from coding where status==1 group by fid")$fid
##     fid_uncoded <- fid[! (fid %in% fid_coded)]
##     source <- dbGetQuery(.rqda$qdacon,
##                          sprintf("select name,date, id from source where status=1 and id in (%s)",
##                                  paste(fid_uncoded,sep="",collapse=",")))
##     if (nrow(source) != 0){
##       fnames <- source$name
##       Encoding(fnames) <- "UTF-8"
##       if (sort){
##       fnames <- fnames[OrderByTime(source$date,decreasing=decreasing)]
##       }
##     }
##     tryCatch(FileNamesWidget[] <- fnames, error = function(e) {})
##   }
## }


## setEncoding <- function(encoding="unknown"){
  ## moved to utils.R
##   ## specify what encoding is used in the imported files.
##   .rqda$encoding <- encoding
## }

## enc <- function(x,encoding="UTF-8") {
##   ## replace " with two '. to make insert smoothly.
##   ## encoding is the encoding of x (character vector).
##   ## moved to utils.R
##   Encoding(x) <- encoding
##   x <- gsub("'", "''", x)
##   if (Encoding(x)!="UTF-8") {
##     x <- iconv(x,to="UTF-8")
##   }
##   x
## }
