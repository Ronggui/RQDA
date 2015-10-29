exportCodedFile <- function(file, fid, closeAfter=TRUE){
    ## possible bug when there is annotations
    ## open a file of fid first
    fname <- RQDAQuery(sprintf("select name from source where id=%s and status=1",fid))$name[1]
    Encoding(fname) <- "UTF-8"
    ViewFileFunHelper(fname, annotation=FALSE)
    ans <- c()
    shift <- 0
    cidx <- RQDAQuery(sprintf("select coding.rowid as rowid,selfirst,selend, freecode.name as code from coding, freecode where fid=%s and coding.status=1 and coding.cid=freecode.id",fid))
    idx <- cidx[order(cidx$selfirst),]
    wf <- .rqda$.openfile_gui
    buffer <- wf@widget@widget$GetBuffer()
    endidx <- buffer$GetEndIter()$iter$GetOffset()
    idx <- sort(unique(c(0, endidx, cidx$selfirst,cidx$selend)))
    nidx <- length(idx) - 1

    for (i in 1:nidx){
        idx1 <- idx[i] + shift
        idx2 <- idx[i+1] + shift
        iter1 <- buffer$GetIterAtOffset(idx1)$iter
        iter2 <- buffer$GetIterAtOffset(idx2)$iter
        text <- buffer$GetText(iter1, iter2)
        Encoding(text) <- "UTF-8"
        ans <- c(ans, text)
        marks <- gtkTextIterGetMarks(iter2)
        nincrease <- 0
        while(identical(marks, list())) {
            nincrease <- iter2$ForwardChar() + nincrease
            marks <- gtkTextIterGetMarks(iter2)
        }
        rowids <- sapply(marks,gtkTextMarkGetName)
        pos <- match(gsub(".[1,2]$","",rowids),cidx$rowid)
        code <- cidx$code[pos]
        Encoding(code) <- "UTF-8"
        b <- grep(".1$",rowids)
        code[b] <- paste("<b><font color='#FF0000'>&lt&lt ",code[b],"</font></b>", sep="")
        e <- grep(".2$",rowids)
        code[e] <- paste("<u><font color='#FF0000'><i>",code[e]," &gt&gt</font></u></i>", sep="")
        ans <- c(ans, code)
        shift <- shift + nincrease
    } ## end of loop over i
    ans <- paste(ans, collapse="|", sep="")
    ans <-gsub("\n","<br>",ans)
    file <- file(file, open = "w", encoding = "UTF-8")
    cat("<HEAD><META HTTP-EQUIV='CONTENT-TYPE' CONTENT='text/html; charset=UTF-8'><TITLE>Coded file exported by RQDA.</TITLE><META NAME='AUTHOR' CONTENT='RQDA'>",
        file = file, append = FALSE)
     cat(sprintf("Created by <a href='http://rqda.r-forge.r-project.org/'>RQDA</a> at %s<br><br>\n",Sys.time()),file=file,append=TRUE)
    cat(ans,file=file,append=TRUE)
    close(file)
    if (closeAfter) dispose(.rqda$.root_edit)
}
