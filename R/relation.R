relation <- function(index1,index2){
  ## index1 and index2 are length-2 numeric vectors
  ## results:
  ## Relation: type of relation
  ## WhichMin: which argument containts min(c(index1,index2))
  ## WhichMax: which argument containts max(c(index1,index2))
  ## Distance: The distance between to index when Relation is proximity
  ## the index of the overlap of index1 and index2.
  if ( !is.vector(index1) || !is.vector(index1) ) stop("index1 and index2 must be vector.")
  index1 <- as.numeric(index1)
  index2 <- as.numeric(index2)
  if (any(is.na(c(index1,index2)))) stop("index1 or index2 should not have any NA.")
  names(index1) <- names(index2) <- NULL
  if (length(index1)==2 || length(index1)==2){
    Max <- max(c(index1,index2))
    Min <- min(c(index1,index2))
    ans <- list(Relation=NA,WhichMin=NA,WhichMax=NA, Distance=NA,OverlapIndex=c(NA,NA),UnionIndex=c(NA,NA))
    ans$WhichMin <- which(c(index1[1],index2[1])==Min)
    ans$WhichMax <- which(c(index1[2],index2[2])==Max)
    if (sum(index1 %in% c(Min,Max))==2 || sum(index2 %in% c(Min,Max))==2) {
      if (length(ans$WhichMin)==2 && length(ans$WhichMax)==2){
        ans$Relation <- "exact"
        ans$OverlapIndex <- index1
        ans$UnionIndex<- index1
      } else {
        ans$Relation <- "inclusion"
        if (intersect(ans$WhichMin,ans$WhichMax)==1) {
          ans$OverlapIndex <- index2
          ans$UnionIndex <- index1
        } else {
          ans$OverlapIndex <- index1
          ans$UnionIndex<- index2
        }
      }
    } else {
      if (min(index1) <= min(index2) &&
          max(index1) >= min(index2)) {
        ans$Relation <- "overlap"
        ans$OverlapIndex <- c(min(index2),max(index1))
        ans$UnionIndex <- c(min(index1),max(index2))
      }
      if (min(index2) <= min(index1) &&
          max(index2) >= min(index1)) {
        ans$Relation <- "overlap"
        ans$OverlapIndex<- c(min(index1),max(index2))
        ans$UnionIndex<- c(min(index2),max(index1))
      }
      if (max(index1) < min(index2)){
        ans$Relation <- "proximity"
        ans$Distance <- min(index2) -max(index1)
      }
      if (max(index2) < min(index1)){
        ans$Relation <- "proximity"
        ans$Distance <- min(index1) -max(index2)
      }
    }
    if (length(ans$WhichMin)==2) ans$WhichMin <- NA
    if (length(ans$WhichMax)==2) ans$WhichMax <- NA
    ans
  }
}

CrossTwo <- function(cid1, cid2,data,relation=c("overlap","inclusion","exact","proximity"),...)
{
  ## cid1 and cid2 is length-1 numeric, represents the id of codes
  ## data is return by GetCodingTable.
  ## cid1=1; cid2=2
  relation <- match.arg(relation)
  data <- data[data$cid %in% c(cid1,cid2),c("cid","fid","index1","index2")]
  ans <- 0
  fidList <- unique(data[data$cid %in% cid1,"fid"])
  for (fid in fidList) {
    tmpdat1 <- data[data$fid==fid & data$cid==cid1,,drop=FALSE]
    tmpdat2 <- data[data$fid==fid & data$cid==cid2,,drop=FALSE]
    if (nrow(tmpdat2)>0 && nrow(tmpdat1)>0){
      for(i in seq_len(nrow(tmpdat1))){
        for(j in seq_len(nrow(tmpdat2))){
          Relation <- relation(unlist(tmpdat2[j,c("index1","index2")]),unlist(tmpdat1[i,c("index1","index2")]))
          if (Relation$Relation==relation) {
            ans <- ans+1
            ## may add atributes to ans, so to get more information
          }
        }
      }
    }
  }
  ans
}

crossCodes <- CrossCode <- function(relation=c("overlap","inclusion","exact","proximity"),codeList=NULL,data=GetCodingTable(),print=TRUE,...){
## codeList is character vector of codes.
  if (nrow(data)==0) {
    stop("No coding in this project.")
  } else{
    Cid_Name <- unique(data[,c("cid","codename")])
    if (is.null(codeList)) {
        codeList <- gselect.list(Cid_Name$codename,multiple=TRUE)
    } else {
        nList <- length(codeList)
        codeList <- intersect(Cid_Name$codename,codeList)
        if (nList > length(codeList)) cat("Codes without codings dropped.\n")
    }
    if (length(codeList)<2) {
      stop("The codeList should be a vector of length 2 or abvoe.")
    } else {
      cidList <- Cid_Name$cid[match(codeList, Cid_Name$codename)]
      relation <- match.arg(relation)
      ans <- matrix(nrow=length(codeList), ncol=length(codeList),dimnames=list(
                                                                 sprintf("%s(%s)", codeList,cidList),
                                                                 cidList))
      for (i in 1:length(codeList)){
        for (j in i:length(codeList)){
          ans[i,j] <- CrossTwo(cidList[i],cidList[j],data=data,relation=relation)
        }
      }
      class(ans) <- "crossCodes"
      if (print) {print(ans,na.print="")}
      invisible(ans)
    }
  }
}

plot.crossCodes <- function(x, ...){
    colnames(x) <- rownames(x)
    if (all(x==0,na.rm=T)) x <- x + 0.5
    cmG <- igraph::graph.adjacency(x,mode="upper",diag=FALSE,weighted=TRUE)
    ew <- igraph::get.edge.attribute(cmG,"weight")
    igraph::set.edge.attribute(cmG, "color",V(cmG)[ew==1], "green")
    igraph::set.edge.attribute(cmG, "color",V(cmG)[ew==2], "yellow")
    igraph::set.edge.attribute(cmG, "color",V(cmG)[ew==3], "orange")
    igraph::set.edge.attribute(cmG, "color",V(cmG)[ew>3], "red")
    tryCatch(igraph::tkplot(cmG,edge.width=sqrt(igraph::get.edge.attribute(cmG,"weight")),
                             vertex.label=igraph::get.vertex.attribute(cmG,"name"),
                             edge.label=floor(igraph::get.edge.attribute(cmG,"weight"))
                             ), error=function(e){
        plot(cmG,edge.width=sqrt(igraph::E(cmG)$weight),vertex.label=igraph::V(cmG)$CodeName)
    })
}
