getCodingsOfCodes <- function(fid = NULL, codingTable = c("coding", "coding2")){
    codes <- .rqda$.codes_rqda[]
    Encoding(codes) <- "UTF-8"
    selected <- gselect.list(codes)
    selected <- enc(selected)
    cid <- RQDAQuery(sprintf("select id from freecode where status=1 and name in (%s)",paste( paste("'", selected, "'", sep=""), collapse = ",")))$id
    codingTable <- match.arg(codingTable)
    if (codingTable == "coding") {
        ct <- RQDAQuery(sprintf("select coding.rowid as rowid, coding.cid, coding.fid,
freecode.name as codename, source.name as filename, coding.selfirst as index1,
coding.selend as index2, coding.seltext as coding, coding.selend - coding.selfirst as CodingLength from coding
 left join freecode on (coding.cid=freecode.id) left join source on (coding.fid=source.id)
where coding.status=1 and source.status=1 and freecode.status=1 and coding.cid in (%s)", paste(cid,collapse=",")))
    }
    if (codingTable == "coding2") {
        ct <- RQDAQuery(sprintf("select coding.rowid as rowid, coding.cid, coding.fid,
freecode.name as codename, source.name as filename, coding2.selfirst as index1,
coding2.selend as index2, coding2.seltext as coding2, coding2.selend - coding2.selfirst as CodingLength from coding2
 left join freecode on (coding2.cid=freecode.id) left join source on (coding2.fid=source.id)
where coding2.status=1 and source.status=1 and freecode.status=1 and coding2.cid in (%s)", paste(cid,collapse=",")))
    }
    if (nrow(ct) != 0) {
        Encoding(ct$codename) <- Encoding(ct$filename) <- Encoding(ct$coding) <- "UTF-8"
        if (!is.null(fid))
            ct <- ct[ct$fid %in% fid, ]
    }
    ct ## can be printed via print.CodingsByOne
}

getCodingsFromFiles <- function(Fid, order=c("fname","ftime","ctime"), codingTable="coding")
{
    order <- match.arg(order)
    order <- switch(order,
                    fname="order by freecode.name, source.name, selfirst, selend ASC",
                    ftime="order by freecode.name, source.id, selfirst, selend ASC",
                    ctime="")
    retrieval <- RQDAQuery(sprintf("select cid, freecode.name as code, fid, selfirst, selend, seltext, %s.rowid, source.name,source.id from %s,source, freecode where %s.status=1 and source.id=fid and freecode.id=%s.cid and fid in (%s) %s", codingTable, codingTable, codingTable, codingTable, paste(Fid,collapse=","), order))
    if (nrow(retrieval)==0) gmessage("No Coding associated with the selected code.",container=TRUE) else {
        fid <- unique(retrieval$fid)
        Nfiles <- length(fid)
        retrieval$fname <-""
        Ncodings <- nrow(retrieval)
        title <- sprintf(ngettext(Ncodings,"%i Retrieved coding from %i %s", "%i Retrieved codings from %i %s"),Ncodings, Nfiles, ngettext(Nfiles,"file","files"))
        wnh <- size(.rqda$.root_rqdagui) ## size of the main window
        .gw <- gwindow(title=title, parent=c(wnh[1]+10,2),
                       width = min(c(gdkScreenWidth()- wnh[1]-20,getOption("widgetSize")[1])),
                       height = min(c(wnh[2],getOption("widgetSize")[2]))
                       )
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        .gw@widget@widget$SetIconFromFile(mainIcon)
        .retreivalgui <- gtext(container=.gw)
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(.retreivalgui@widget@widget,font)
        .retreivalgui@widget@widget$SetPixelsBelowLines(5) ## set the spacing
        .retreivalgui@widget@widget$SetPixelsInsideWrap(5) ## so the text looks more confortable.
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

        buffer <- .retreivalgui@widget@widget$GetBuffer()
        buffer$createTag("red", foreground = "red")
        iter <- buffer$getIterAtOffset(0)$iter

        apply(retrieval,1, function(x){
            metaData <- sprintf("[%s] - %s [%i:%i]",x[["code"]], x[['fname']],as.numeric(x[['selfirst']]),as.numeric(x[['selend']]))
            ## buffer$InsertWithTagsByName(iter, metaData,"x-large","red")
            buffer$InsertWithTagsByName(iter, metaData,"red")
            anchorcreated <- buffer$createChildAnchor(iter)
            iter$BackwardChar()
            anchor <- iter$getChildAnchor()
            lab <- gtkLabelNew("Back")
            widget <- gtkEventBoxNew()
            widget$Add(lab)
            gSignalConnect(widget, "button-press-event",
                           ComputeCallbackFun(x[["fname"]],as.numeric(x[["rowid"]])))
            .retreivalgui@widget@widget$addChildAtAnchor(widget, anchor)
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
