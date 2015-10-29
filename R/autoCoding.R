AutoCoding <- function(KeyWord,expansion=6){
  Files <- SearchFiles(paste("%",KeyWord,"%",collapse=""),content=TRUE)
  AnsIndex <- gregexpr(KeyWord,Files$file)
  AnsIndex2 <- lapply(AnsIndex, FUN=function(x) {
    begin <- x-expansion
    begin[begin<0]<-0
    data.frame(begin=begin,end=x+attr(x,"match.length"))
  })
  ## if any index > nchar(Files$file), set to nchar(Files$file)
  ## for each file, simplify the coding index, so erase the overlapping codings or proximity with distance=0
}

insertCoding <- function(fid, cid, start, end, fulltext) {
  Exist1 <-  RQDAQuery(sprintf("select coding.rowid, selfirst, selend,freecode.name from coding, freecode where cid=%i and fid=%i and coding.status=1 and cid=freecode.id",cid,fid))
  DAT <- data.frame(cid=cid,fid=fid,seltext=substr(fulltext,start+1,end),selfirst=start,selend=end,status=1,owner=.rqda$owner,date=date(),memo=NA,stringsAsFactors=FALSE)
  DAT$seltext <- enc(DAT$seltext)

  if (nrow(Exist1)==0){
    try(RQDAQuery(sprintf("insert into coding (cid,fid, seltext, selfirst, selend, status, owner, date) values (%s, %s, '%s', %s, %s, %s, '%s', '%s') ",
                           DAT$cid, DAT$fid,DAT$seltext, DAT$selfirst, DAT$selend, 1, .rqda$owner, as.character(date()))),silent=TRUE)
  } else {
    Exist <- Exist1[,c("selfirst","selend","rowid")]
    Relations <- apply(Exist,1,FUN=function(x) relation(x[c("selfirst","selend")],c(start,end)))
    Exist$Relation <- sapply(Relations,FUN=function(x)x$Relation)
    if (!any(Exist$Relation=="exact")){
      ## if they are axact, do nothing; -> if they are not exact, do something.
      Exist$WhichMin <- sapply(Relations,FUN=function(x)x$WhichMin)
      Exist$Start <- sapply(Relations,FUN=function(x)x$UnionIndex[1])
      Exist$End <- sapply(Relations,FUN=function(x)x$UnionIndex[2])
      if (all(Exist$Relation=="proximity")){
        rowid <- NextRowId("coding")
        try(RQDAQuery(sprintf("insert into coding (cid,fid, seltext, selfirst, selend, status, owner, date) values (%s, %s, '%s', %s, %s, %s, '%s', '%s') ",
                              DAT$cid, DAT$fid, DAT$seltext, DAT$selfirst, DAT$selend, 1, .rqda$owner, as.character(date()))),silent=TRUE)
      } else {
        del1 <-(Exist$Relation =="inclusion" & (is.na(Exist$WhichMin) | Exist$WhichMin==2))
        ## if overlap or inclusion [old nested in new]
        ## then the original coding should be deleted
        ## then write the new coding to table
        del2 <- Exist$Relation =="overlap"
        del <- (del1 | del2)
        if (any(del)){
          Sel <- c(min(Exist$Start[del]), max(Exist$End[del]))
          memo <- RQDAQuery(sprintf("select memo from coding where rowid in (%s)", paste(Exist$rowid[del],collapse=",",sep="")))$memo
          memo <- paste(memo,collapse="",sep="")
          RQDAQuery(sprintf("delete from coding where rowid in (%s)", paste(Exist$rowid[del],collapse=",",sep="")))
          DAT <- data.frame(cid=cid,fid=fid,seltext=substr(fulltext,Sel[1]+1,Sel[2]),selfirst=Sel[1],selend=Sel[2],status=1,owner=.rqda$owner,date=date(),memo=memo,stringsAsFactors=FALSE)
          DAT$seltext <- enc(DAT$seltext)
          rowid <- NextRowId("coding")
          try(RQDAQuery(sprintf("insert into coding (cid,fid, seltext, selfirst, selend, status, owner, date, memo) values (%s, %s, '%s', %s, %s, %s, '%s', '%s','%s') ",
                                DAT$cid, DAT$fid,DAT$seltext, DAT$selfirst, DAT$selend, 1, .rqda$owner, as.character(date()), DAT$memo)),silent=TRUE)
        }
      }
    }
  }
}
  

codingBySearchOneFile <- function(pattern, fid, cid, seperator, concatenate, ...) {
  ## auto coding: when seperator is \n, each paragraph is a analysis unit
  ## by providing approperiate seperator, it allows flexible control on the unit of autocoding
    txt <- RQDAQuery(sprintf("select file from source where status=1 and id=%s",fid))$file
    Encoding(txt) <- "UTF-8"
    pidx <- gregexpr(sprintf("(%s){1,}", seperator),txt)
    idx1 <- c(0,pidx[[1]]+attr(pidx[[1]],"match.length")-1)
    idx2 <- c(pidx[[1]]-1,nchar(txt))
    sidx <- gregexpr(pattern,txt, ...)[[1]]
    if (length(sidx) > 1 || (sidx != -1)) {
        residx <- unique(findInterval(sidx,sort(c(idx1,idx2))))
        idx <- (residx + 1)/2
        if (concatenate)
            removeidx <- which(diff(idx)==1)
        else
            removeidx <- NULL
        
        if (length(removeidx) > 0) {
          selfirst = idx1[idx[-(removeidx+1)]]
          elend   = idx2[idx[-removeidx]]
        } else {
          selfirst = idx1[idx]
          selend   = idx2[idx]
        }
        
        for (c in cid)
          for (i in (1:length(selfirst)))
            insertCoding (fid=fid, cid=c, start=selfirst[i], end=selend[i], txt)
    }
}

codingBySearch <- function(pattern, fid = getFileIds(), cid, seperator="\n", concatenate=FALSE, ...) {
    if (length(fid)> 0) {
        for (i in fid) {
            codingBySearchOneFile(pattern, fid=i, cid=cid, seperator=seperator, concatenate=concatenate, ...)
        }
    }
}
