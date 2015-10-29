getCodingsByOne <- function(cid, fid=NULL,codingTable=c("coding","coding2")){
    if (length(cid)!=1) stop("cid should be length-1 integer vector.")
    codingTable <- match.arg(codingTable)
     if (codingTable=="coding"){
    ct <- RQDAQuery(sprintf("select coding.rowid as rowid, coding.cid, coding.fid, freecode.name as codename, source.name as filename, coding.selfirst as index1, coding.selend as index2, coding.seltext as coding, coding.selend - coding.selfirst as CodingLength from coding left join freecode on (coding.cid=freecode.id) left join source on (coding.fid=source.id) where coding.status=1 and source.status=1 and freecode.status=1 and coding.cid=%s",cid))
     }
     if (codingTable=="coding2"){
    ct <- RQDAQuery(sprintf("select coding2.rowid as rowid, coding2.cid, coding2.fid, freecode.name as codename, source.name as filename, coding2.selfirst as index1, coding2.selend as index2, coding2.seltext as coding, coding2.selend - coding2.selfirst as CodingLength from coding2 left join freecode on (coding2.cid=freecode.id) left join source on (coding2.fid=source.id) where coding2.status=1 and source.status=1 and freecode.status=1 and coding2.cid=%s",cid))
     }
    if (nrow(ct) != 0) {
        Encoding(ct$codename) <- Encoding(ct$filename) <- Encoding(ct$coding) <- "UTF-8"
     if (!is.null(fid)) ct <- ct[ct$fid %in% fid,]
    }
    class(ct) <- c("codingsByOne","data.frame")
    ct
}


print.codingsByOne <- function (x,...)
{
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
    }

    if (nrow(x) == 0)
      gmessage("No Codings.", container = TRUE)
    else {
        x <-x[order(x$fid,x$index1,x$index2),]
        fid <- unique(x$fid)
        Nfiles <- length(fid)
        Ncodings <- nrow(x)
        title <- sprintf(ngettext(Ncodings, "%i coding from %s %s",
                                  "%i codings from %s %s"), Ncodings,
                         Nfiles, ngettext(Nfiles, "file", "files"))
        tryCatch(eval(parse(text = sprintf("dispose(.rqda$.codingsOf%s)",
                            "codingsByone"))), error = function(e) {
                            })
        .gw <- gwindow(title = title, parent = getOption("widgetCoordinate"),
                       width = getOption("widgetSize")[1], height = getOption("widgetSize")[2])
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        .gw@widget@widget$SetIconFromFile(mainIcon)
        assign(sprintf(".codingsOf%s","codingsByone"), .gw, envir = .rqda)
        .retreivalgui <- gtext(container = .gw)
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(.retreivalgui@widget@widget,font)
        .retreivalgui@widget@widget$SetPixelsBelowLines(5)
        .retreivalgui@widget@widget$SetPixelsInsideWrap(5)
        buffer <- .retreivalgui@widget@widget$GetBuffer()
        buffer$createTag("red", foreground = "red")
        iter <- buffer$getIterAtOffset(0)$iter
        apply(x, 1, function(x) {
            metaData <- sprintf("%s [%i:%i]", x[["filename"]],as.numeric(x[["index1"]]), as.numeric(x[["index2"]]))
            buffer$InsertWithTagsByName(iter, metaData, "red")
            anchorcreated <- buffer$createChildAnchor(iter)
            iter$BackwardChar()
            anchor <- iter$getChildAnchor()
            lab <- gtkLabelNew("Back")
            widget <- gtkEventBoxNew()
            widget$Add(lab)
            gSignalConnect(widget, "button-press-event",
                           ComputeCallbackFun(x[["filename"]],as.numeric(x[["rowid"]])))
            .retreivalgui@widget@widget$addChildAtAnchor(widget, anchor)
            widget$showAll()
            iter$ForwardChar()
            buffer$insert(iter, "\n")
            buffer$InsertWithTagsByName(iter, x[["coding"]])
            buffer$insert(iter, "\n\n")
        })
        buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter)
    }
}

andHelper <- function(d1,d2){
    da11 <- sort(unlist(apply(d1,1,function(i)seq(i[1],i[2]))))
    da22 <- sort(unlist(apply(d2,1,function(i)seq(i[1],i[2]))))
    daAll <- c(da11,da22)
    ta <- table(daAll)
    x <- sort(as.numeric(names(ta)[which(ta==2)]))
    vnl <- rle(diff(x))
    idx2 <- 1+cumsum(vnl$lengths)[which(vnl$value==1)]
    len <- 1+vnl$lengths[which(vnl$value==1)]
    idx1 <- idx2 - len + 1
    x1 <- x[idx1]
    x2 <- x[idx2]
    ans <- data.frame(index1=x1, index2=x2)
    ans
}

and <- function (CT1, CT2)
### much faster than previous version of and()
### can extend to andSmart to handle more codes at the same time
{
    ans <- data.frame()
    fid <- unique(intersect(CT1$fid, CT2$fid))
    if (length(fid) > 0) {
        for (j in fid) {
            tmp <- andHelper(subset(CT1, fid == j, c("index1","index2")),
                                    subset(CT2, fid == j, c("index1","index2"))
                                    )
            if (nrow(tmp)>0) {
            tmp <- cbind(tmp,fid=j, filename=CT1$filename[which(CT1$fid==j)[1]])
            rid1 <- match(tmp$index1,CT1$index1)
            rid1NA <- is.na(rid1)
            tmp$rowid[!rid1NA] <- CT1$rowid[rid1[!rid1NA]]
            rid2 <- match(tmp$index1[rid1NA],CT2$index1)
            tmp$rowid[rid1NA] <- CT2$rowid[rid2]
            ## add rowid so the summary method will work
            ans <- rbind(ans,tmp)
            }
        }
        if (nrow(ans) != 0){
            txt <- apply(ans,1,function(x){
                txt <- RQDAQuery(sprintf("select file from source where id=%s",x[["fid"]]))[1,1]
                Encoding(txt) <- "UTF-8"
                ans <- substr(txt, as.numeric(x[["index1"]])+1, as.numeric(x[["index2"]]))
                ans
            })
            ans$coding <- txt
        }
    }
    class(ans) <- c("codingsByOne", "data.frame")
    ans
}

orHelper <- function(d1,d2){
    da11 <- sort(unlist(apply(d1,1,function(i)seq(i[1],i[2]))))
    da22 <- sort(unlist(apply(d2,1,function(i)seq(i[1],i[2]))))
    daAll <- c(da11,da22)
    x <- sort(unique(daAll))
    vnl <- rle(diff(x))
    idx2 <- 1+cumsum(vnl$lengths)[which(vnl$value==1)]
    len <- 1+vnl$lengths[which(vnl$value==1)]
    idx1 <- idx2 - len + 1
    x1 <- x[idx1]
    x2 <- x[idx2]
    ans <- data.frame(index1=x1, index2=x2)
    ans
}

or <- function (CT1, CT2)
{
    ans <- data.frame(stringsAsFactors=FALSE)
    fid <- unique(union(CT1$fid, CT2$fid))
    if (length(fid) > 0) {
        for (j in fid) {
            tmp <- orHelper(subset(CT1, fid == j, c("index1","index2")),
                            subset(CT2, fid == j, c("index1","index2"))
                            )
            if (nrow(tmp)>0) {
                tmp <- cbind(tmp,fid=j, filename=CT1$filename[which(CT1$fid==j)[1]],stringsAsFactors=FALSE)
                tmp$filename[is.na(tmp$filename)] <- CT2$filename[which(CT2$fid==j)[1]]
                rid1 <- match(tmp$index1,CT1$index1)
                rid1NA <- is.na(rid1)
                tmp$rowid[!rid1NA] <- CT1$rowid[rid1[!rid1NA]]
                rid2 <- match(tmp$index1[rid1NA],CT2$index1)
                tmp$rowid[rid1NA] <- CT2$rowid[rid2]
                ## add rowid so the summary method will work
                ans <- rbind(ans,tmp)
            }
        }
        if (nrow(ans) != 0){
            txt <- apply(ans,1,function(x){
                txt <- RQDAQuery(sprintf("select file from source where id=%s",x[["fid"]]))[1,1]
                Encoding(txt) <- "UTF-8"
                ans <- substr(txt, as.numeric(x[["index1"]])+1, as.numeric(x[["index2"]]))
                ans
            })
            ans$coding <- txt
        }
    }
    class(ans) <- c("codingsByOne", "data.frame")
    ans
}


notHelper <- function(d1,d2){
    da11 <- sort(unlist(apply(d1,1,function(i)seq(i[1],i[2]))))
    da22 <- sort(unlist(apply(d2,1,function(i)seq(i[1]+1,i[2]-1))))
    daAll <- setdiff(da11,da22)
    x <- sort(unique(daAll))
    vnl <- rle(diff(x))
    idx2 <- 1+cumsum(vnl$lengths)[which(vnl$value==1)]
    len <- 1+vnl$lengths[which(vnl$value==1)]
    idx1 <- idx2 - len + 1
    x1 <- x[idx1]
    x2 <- x[idx2]
    ans <- data.frame(index1=x1, index2=x2)
    ans
}

not <- function (CT1, CT2)
{
    ans <- data.frame(stringsAsFactors=FALSE)
    fid <- unique(CT1$fid)
    if (length(fid) > 0) {
        for (j in fid) {
            tmp <- notHelper(subset(CT1, fid == j, c("index1","index2")),
                             subset(CT2, fid == j, c("index1","index2"))
                            )
            if (nrow(tmp)>0) {
                tmp <- cbind(tmp,fid=j, filename=CT1$filename[which(CT1$fid==j)[1]],stringsAsFactors=FALSE)
                rid1 <- match(tmp$index1,CT1$index1)
                rid1NA <- is.na(rid1)
                tmp$rowid[!rid1NA] <- CT1$rowid[rid1[!rid1NA]]
                rid2 <- match(tmp$index1[rid1NA],CT2$index1)
                tmp$rowid[rid1NA] <- CT2$rowid[rid2]
                ans <- rbind(ans,tmp)
            }
        }
        if (nrow(ans) != 0){
            txt <- apply(ans,1,function(x){
                txt <- RQDAQuery(sprintf("select file from source where id=%s",x[["fid"]]))[1,1]
                Encoding(txt) <- "UTF-8"
                ans <- substr(txt, as.numeric(x[["index1"]])+1, as.numeric(x[["index2"]]))
                ans
            })
            ans$coding <- txt
        }
    }
    class(ans) <- c("codingsByOne", "data.frame")
    ans
}

"%and%.codingsByOne" <- function(e1,e2){
    ## and(e1, e2, showCoding=TRUE, method= getOption("andMethod"))
    and(e1, e2)
}

"%or%.codingsByOne" <- function(e1,e2){
  or(e1, e2)
}

"%not%.codingsByOne" <- function(e1,e2){
  not(e1, e2)
}



## and_helper <- function(CT1,CT2,method){
##   ## CT1 and CT2 is from GetCodingTable,each for one code and one file only
##   ridx <- vector()
##   idx <- vector()
##   for (i in 1:nrow(CT1)) {
##     for (j in 1:nrow(CT2)){
##       rel <- relation(as.numeric(CT1[i,c("index1","index2")]),as.numeric(CT2[j,c("index1","index2")]))
##       if (rel$Relation %in% method){
##         ridx <- c(ridx,i,j)
##         idx <- c(idx,rel$OverlapIndex)
##       }
##     }
##   }
##   if (length(ridx) >=2){
##     rf <- ridx[seq(from=1,to=length(ridx),by=2)] ## row index for CT1
##     rs <- ridx[seq(from=2,to=length(ridx),by=2)] ## row index for CT2
##     index1 <- idx[seq(from=1,to=length(idx),by=2)]
##     index2 <- idx[seq(from=2,to=length(idx),by=2)]
##     ans <- cbind(CT1[rf,c("rowid","fid","filename")],index1=index1,index2=index2)
##     ans
##   }
## }


## and <- function(CT1,CT2,showCoding=TRUE, method= c("overlap","exact","inclusion")){
##   ## CT1 and CT2 is from GetCodingTable,each for one code only
##   fid <- intersect(CT1$fid,CT2$fid)
##   if (length(fid)>0) {
##     ans <- lapply(fid,FUN=function(x) {
##       and_helper(CT1=subset(CT1,fid==x),CT2=subset(CT2,fid==x),method=method)
##     }
##       )
##     ans <- do.call(rbind,ans)
##     if (showCoding && !is.null(ans)){
##       txt <- apply(ans,1,function(x){
##         txt <- RQDAQuery(sprintf("select file from source where id==%s",x[["fid"]]))[1,1]
##         Encoding(txt) <- "UTF-8"
##         ans <- substr(txt, as.numeric(x[["index1"]])+1, as.numeric(x[["index2"]]))
##         ans
##       })
##       ans$coding <- txt
##     }
##   }

##   if ((length(fid)==0) || is.null(ans)){
##     ans <- data.frame("rowid"=integer(0),"fid"=integer(0),
##                       "filename"=character(0), "index1"=integer(0),
##                       "index2"=integer(0), "coding"=character(0))
##   }
##   class(ans) <- c("codingsByOne","data.frame")
##   ans
## }


## or <- function(CT1,CT2) {
##   ## revised from mergeCodes() again.
##   ## may use temp database table to do it.
##   orHelperFUN <- function(From,Exist){ ## from and exist are data frame of codings.
##     if (nrow(Exist)==0){## just write to the new code if there is no coding related to that code.
##       ans <- From[,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE]
##     } else {
##       Relations <- apply(Exist[c("index1","index2")],1,FUN=function(x) relation(x,c(From$index1,From$index2)))
##       ## because apply convert data to an array, and Exist containts character -> x is charater rather than numeric
##       Exist$Relation <- sapply(Relations,FUN=function(x) x$Relation) ## add Relation to the data frame as indicator.
##       if (!any(Exist$Relation=="exact")) {
##         ## if they are axact, do nothing;
##         ## if they are not exact, do something. The following lines record meta info
##         Exist$WhichMin <- sapply(Relations,FUN=function(x)x$WhichMin)
##         Exist$Start <- sapply(Relations,FUN=function(x)x$UnionIndex[1])
##         Exist$End <- sapply(Relations,FUN=function(x)x$UnionIndex[2])
##         if (all(Exist$Relation=="proximity")){
##           ## take care of proximity with distance of 0.
##           ## (a not b) or (b) == a
##           dis <- sapply(Relations,function(x) x$Distance)
##           if (all(dis>0)) {
##             ## if there are no overlap in any kind, the result is From+Exist
##             ans <- rbind(From[,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE],
##                          Exist[,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE])
##           } else {
##             idx0 <- which(dis==0)
##             index3 <- unlist(c(From[,c("index1","index2")],Exist[idx0,c("index1","index2")]))
##             From["coding"] <- paste(Exist$coding[idx0][rank(Exist$index1[idx0])],collapse="")
##             From["index1"] <- min(index3)
##             From["index2"] <- max(index3)
##             ans <- rbind(From[,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE],
##                          Exist[which(dis>0),
##                                c("rowid","fid","filename","index1","index2","coding"),drop=FALSE]
##                          )
##           }
##           ## end of handling proximity
##         } else {
##           ## if not proximate, pass to else branch.
##           del1 <- (Exist$Relation =="inclusion" & any(Exist$WhichMin==2,Exist$WhichMax==2))
##           ## ==2 -> take care of NA. Here 2 means From according to how Relations is returned.
##           del2 <- Exist$Relation =="overlap"
##           ## if overlap or inclusion [Exist nested in From] -> delete codings in Exist
##           del <- (del1 | del2) ## index of rows in Exist that should be deleted.
##           if (any(del)){
##             ExistN <- Exist[-which(del),c("rowid","fid","filename","index1","index2","coding")]
##             ## delete codings
##             tt <-   RQDAQuery(sprintf("select file from source where id=='%i'", From$fid))[1,1]
##             Encoding(tt) <- "UTF-8"  ## fulltext of the file
##             Sel <- c(min(Exist$Start[del]), max(Exist$End[del])) ## index to get the new coding
##             ans <- rbind(ExistN,
##                          data.frame(rowid=From$rowid,fid=From$fid,filename=From$filename,
##                                     index1=Sel[1],index2=Sel[2],coding=substr(tt,Sel[1],Sel[2])
##                                     )
##                          )
##           }
##         } ## end of handling overlapping and inclusion
##       }
##     }
##     ans
##   } ## end of helper function.

##   if (any(c(nrow(CT1),nrow(CT2))==0)) stop("One code has empty coding.")
##   CT1 <- CT1[,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE]
##   CT2 <- CT2[,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE]
##   if (nrow(CT1) >= nrow(CT2)) {
##     FromDat <- CT2
##     ToDat <- CT1
##   } else {
##     FromDat <- CT1
##     ToDat <- CT2
##   }

##   fidUnique <- unique(FromDat$fid)
##   Nf <- length(fidUnique)
##   ans <- vector("list",Nf+1)
##   for (j in 1:Nf) {
##     From <- FromDat[FromDat$fid==fidUnique[j],]
##     for (i in seq_len(nrow(From))) {
##       x <- From[i,,drop=FALSE]
##       if (i==1) {
##         Exist <- ToDat[ToDat$fid==fidUnique[j],]
##         ## use original data only for the first
##       }
##       Exist <- orHelperFUN(From=x,Exist=Exist) ## use the result to update Exist
##     }## end of i
##     ans[[j]] <- Exist
##   } ## and of j
##   ans[[j+1]] <- ToDat[!ToDat$fid %in% fidUnique,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE]
##   ans <- do.call(rbind,ans)
##   class(ans) <- c("codingsByOne","data.frame")
##   ans
## }


## not_helper <- function(CT1,CT2){
##   ## CT1 and CT2 is coings for one code and one file.
##   ridx <- vector()
##   idx <- vector()
##   if (nrow(CT1)!=0) { ## if1
##     if (nrow(CT2)==0) {
##       ridx <- c(ridx,nrow(CT1))
##       idx <- c(idx,unlist(as.data.frame(t(CT1[,c("index1","index2")]))))
##     } else { ## else1
##       for (i in 1:nrow(CT1)) {
##         relAll <- apply(CT2,1,function(x)
##                         relation(CT1[i,c("index1","index2"),drop=TRUE],
##                                  as.numeric(x[c("index1","index2")]))
##                         ) ## end of apply
##         Relation <- sapply(relAll,function(x) x$Relation)
##         if (all(Relation=="exact")) {
##           ## do nothing
##         } else { ## else2
##           if (all(Relation=="proximity")){
##             ridx <- c(ridx,i)
##             idx <- c(idx, CT1[i,c("index1","index2"),drop=TRUE])
##           } else { ## else3
##             in.over <- Relation %in% c("inclusion", "overlap") ## index of overlap and inclusion
##             rel.in.over <- relAll[in.over]
##             nested <- sapply(rel.in.over,function(x){
##               if (x$Relation=="inclusion") {
##                 ans <- (!is.na(x$WhichMin) &&  !is.na(x$WhichMax) &&
##                         x$WhichMin==2 &&  x$WhichMax==2)
##               } else {
##                 ans <- FALSE
##               }
##               ans
##             }
##                              ) ## end of sapply
##             if (any(nested)) {
##               ## do nothing
##             } else {## else4
##               over <- Relation %in% c("overlap")
##               if (sum(over)>2) stop("the same text is coded twice by the same code.")

##               for (j in which(over)) {
##                 if (!is.na(relAll[[j]]$WhichMin) &&  relAll[[j]]$WhichMin==2){
##                   CT1[i,"index1"] <- relAll[[j]]$OverlapIndex[2]
##                 }
##                 if (!is.na(relAll[[j]]$WhichMin) &&  relAll[[j]]$WhichMin==1){
##                   CT1[i,"index2"] <- relAll[[j]]$OverlapIndex[1]
##                 }
##               } ## end for j

##               inidx<- Relation %in% c("inclusion")
##               ans <- sapply(relAll[inidx],function(x) x$OverlapIndex)
##               ans <- sort(unlist(c(CT1[i,c("index1","index2"),drop=TRUE],ans)))
##               ridx <- c(ridx,rep(i,length(ans)/2))
##               idx <- c(idx,ans)
##             }## else4
##           } ## else3
##         } ## else 2
##       } ## end of for i
##     } ## end else1
##   }## if1

##   if (length(ridx) >=1){
##     idx <- unlist(idx)
##     index1 <- idx[seq(from=1,to=length(idx),by=2)]
##     index2 <- idx[seq(from=2,to=length(idx),by=2)]
##     ans <- cbind(CT1[ridx,c("rowid","fid","filename")],index1=index1,index2=index2)
##     ## ans <- unique(ans)
##     ans
##   }

## }## end of fun


## not <- function(CT1,CT2,showCoding=FALSE){
##   fid <- unique(CT1$fid)
##   if (length(fid)>0) {
##     ans <- lapply(fid,FUN=function(x) not_helper(CT1=subset(CT1,fid==x),CT2=subset(CT2,fid==x)))
##     ans <- do.call(rbind,ans)
##     if (showCoding && !is.null(ans)){
##       txt <- apply(ans,1,function(x){
##         txt <- RQDAQuery(sprintf("select file from source where id==%s",x[["fid"]]))[1,1]
##         Encoding(txt) <- "UTF-8"
##         ans <- substr(txt, as.numeric(x[["index1"]])+1, as.numeric(x[["index2"]]))
##         ans
##       })
##       ans$coding <- txt
##     }
##   } else {
##     ans <- data.frame("rowid"=integer(0),"fid"=integer(0),
##                       "filename"=character(0), "index1"=integer(0),
##                       "index2"=integer(0), "coding"=character(0))
##   }
##   class(ans) <- c("codingsByOne","data.frame")
##   ans
## }


