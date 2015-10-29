### UpdateTableWidget() and AddTodbTable() are generall version of the previous functions
UpdateTableWidget <- function(Widget,FromdbTable,con=.rqda$qdacon,sortByTime=FALSE,decreasing=FALSE,...)
{
  if (is_projOpen()){
  items <- dbGetQuery(con, sprintf("select name,date from %s where status=1",FromdbTable))
  if (nrow(items)!=0) {
    Encoding(items$name) <- "UTF-8"
    if (!sortByTime) {items <- sort(items$name,decreasing=decreasing)} else {
      items <- items$name[OrderByTime(items$date,decreasing=decreasing)]
    }
  } else items <- NULL
  tryCatch(eval(substitute(W[] <- items,list(W=quote(Widget)))), error=function(e){})
}}


AddTodbTable <- function(item,dbTable,Id="id",field="name",con=.rqda$qdacon,...) {
    ## now handles ' in item
    if (item != ""){
        maxid <- dbGetQuery(con,sprintf("select max(%s) from %s",Id, dbTable))[[1]]
        nextid <- ifelse(is.na(maxid),0+1, maxid+1)
        write <- FALSE
        if (nextid==1){
            write <- TRUE
        } else {
            dup <- dbGetQuery(con,sprintf("select %s from %s where name='%s'",field, dbTable, enc(item)))
            if (nrow(dup)==0) write <- TRUE
        }
        if (write ) {
            dbGetQuery(con,sprintf("insert into %s (%s, %s, status,date,owner)
                                            values ('%s', %i, %i,%s, %s)",dbTable,field,Id,
                                   enc(item),nextid, 1, shQuote(date()),shQuote(.rqda$owner)))
        }
    }
}


#################
AddCodeCatButton <- function(label="ADD"){
    AddCodCatB <- gbutton(label,handler=function(h,...) {
        item <- ginput("Enter new Code Category. ", icon="info")
        if (!is.na(item)){
            Encoding(item) <- "UTF-8"
            AddTodbTable(item,"codecat",Id="catid") ## CODE CATegory
            UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat")
        }
    })
    assign("AddCodCatB",AddCodCatB,envir=button)
    enabled(AddCodCatB) <- FALSE
    AddCodCatB
}



DeleteCodeCatButton <- function(label="Delete")
{
    DelCodCatB <- gbutton(label, handler=function(h,...) {
        del <- gconfirm("Really delete the Code Category?",icon="question")
        if (isTRUE(del)){
            Selected <- svalue(.rqda$.CodeCatWidget)
            Encoding(Selected) <- "UTF-8"
            catid <- RQDAQuery(sprintf("select catid from codecat where status=1 and name='%s'",enc(Selected)))[,1]
            if (length(catid) ==1){
                dbGetQuery(.rqda$qdacon,sprintf("update codecat set status=0 where name='%s'",enc(Selected)))
                ## set status in table freecode to 0
                UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat")
                tryCatch(dbGetQuery(.rqda$qdacon,sprintf("update treecode set status=0 where catid='%s'",catid)),error=function(e){})
                ## should delete all the related codelists
                UpdateCodeofCatWidget() ## update the code of cat widget
            } else gmessage("The Category Name is not unique.",container=TRUE)
        }
    }
                          )
    assign("DelCodCatB",DelCodCatB,envir=button)
    enabled(DelCodCatB) <- FALSE
    DelCodCatB
}


CodeCat_RenameButton <- function(label="Rename",Widget=.rqda$.CodeCatWidget,...)
{
  ## rename of selected code cat.
  CodCatRenB <- gbutton(label,handler=function(h,...) {
      OldName <- svalue(Widget)
      if (length(OldName)==0){
          gmessage("Select a Code Category first.",icon="error",container=TRUE)
      }
      else {
          ## get the new file names
          NewName <- ginput("Enter new Cateory name. ", text=OldName, icon="info")
          if (!is.na(NewName)) {
              Encoding(NewName) <- "UTF-8"
              rename(OldName,NewName,"codecat")
              UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat")
          }
      }
  })
  assign("CodCatRenB",CodCatRenB,envir=button)
  enabled(CodCatRenB) <- FALSE
  CodCatRenB
}

UpdateCodeofCatWidget <- function(con=.rqda$qdacon,Widget=.rqda$.CodeofCat,sort=TRUE)
{
    SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
    if (length(SelectedCodeCat)!=0){
        ## if code cat is selected, then continue
        Encoding(SelectedCodeCat) <- "UTF-8"
        catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from codecat where status=1 and name='%s'",
                                                 enc(SelectedCodeCat)))[,1]
        Total_cid <- dbGetQuery(con,sprintf("select cid from treecode where status=1 and catid=%i",catid))
        if (nrow(Total_cid)!=0){
        items <- dbGetQuery(con,"select name,id,date from freecode where status=1")
        if (nrow(items)!=0) {
            items <- items[items$id %in% Total_cid$cid,c("name","date")]
            items <- items$name[OrderByTime(items$date)] ## sort accoding to date
            Encoding(items) <- "UTF-8"
            if (sort) items <- sort(items)
        } else items <- NULL
    } else items <- NULL
    } else items <- NULL
    tryCatch(Widget[] <- items,error=function(e){})
}

CodeCatAddToButton <- function(label="Add To",Widget=.rqda$.CodeCatWidget,...)
{
    ans <- gbutton(label,handler=function(h,...) {
        ## SelectedCodeCat and its id (table codecat): svalue()-> Name; sql->catid
        SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
        catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from codecat where status=1 and name='%s'",enc(SelectedCodeCat)))[,1]
        ## CodeList and the id (table freecode): sql -> name and id where status=1
        freecode <-  dbGetQuery(.rqda$qdacon,"select name, id from freecode where status=1")
        if (nrow(freecode) == 0){
            gmessage("No free codes yet.",cont=.rqda$.CodeCatWidget)
        } else {
            Encoding(SelectedCodeCat) <- Encoding(freecode[['name']]) <- "UTF-8"
            ## Get CodeList already in the category (table treecode): sql -> cid where catid==catid
            codeofcat <- dbGetQuery(.rqda$qdacon,sprintf("select cid from treecode where status=1 and catid=%i",catid))
            if (nrow(codeofcat)!=0){
                ## compute those not in the category, then push them to select.list()
                codeoutofcat <- subset(freecode,!(id %in% codeofcat$cid))
            } else  codeoutofcat <- freecode
            Selected <- gselect.list(codeoutofcat[['name']],multiple=TRUE, x=getOption("widgetCoordinate")[1])
            if (length(Selected) >1 || Selected != ""){
                ## Selected <- iconv(Selected,to="UTF-8")
                cid <- codeoutofcat[codeoutofcat$name %in% Selected,"id"]
                Dat <- data.frame(cid=cid,catid=catid,date=date(),dateM=date(),memo="",status=1,owner=.rqda$owner)
                ## Push selected codeList to table treecode
                dbWriteTable(.rqda$qdacon,"treecode",Dat,row.names=FALSE,append=TRUE)
                ## update .CodeofCat Widget
                UpdateCodeofCatWidget()
            }
        }})
    gtkWidgetSetTooltipText(getToolkitWidget(ans),"Add code(s) to the selected code category.")
    assign("CodCatAddToB",ans, envir=button)
    enabled(ans) <- FALSE
    return(ans)
}

  ## update .rqda$.CodeofCat[] by click handler on .rqda$.CodeCatWidget

CodeCatDropFromButton <- function(label="Drop From",Widget=.rqda$.CodeofCat,...)
{
    ans <- gbutton(label,handler=function(h,...) {
        ## Get CodeList already in the category (table treecode): svalue()
        CodeOfCat <- svalue(Widget)
        if ((NumofSelected <- length(CodeOfCat)) ==0) {
            gmessage("Please select the Codes you want to delete.",container=TRUE)
        } else {
            ## Give a confirm msg
            del <- gconfirm(sprintf("Delete %i code(s) from this category. Are you sure?",NumofSelected),container=TRUE,icon="question")
            if (isTRUE(del)){
                ## set status==0 for those selected CodeList (table treecode)
                SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
                ## Encoding(SelectedCodeCat) <- "UTF-8"
                catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from codecat where status=1 and name='%s'",enc(SelectedCodeCat)))[,1]
                for (i in CodeOfCat){
                    cid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where status=1 and name='%s'",enc(i)))[,1]
                    dbGetQuery(.rqda$qdacon,sprintf("update treecode set status=0 where catid=%i and cid=%i",catid,cid))
                }
                ## update .CodeofCat Widget
                UpdateCodeofCatWidget()
            }
        }
    }
                   )
    gtkWidgetSetTooltipText(getToolkitWidget(ans),"Drop selected code(s) from code category.")
    assign("CodCatADroFromB",ans, envir=button)
    enabled(ans) <- FALSE
    return(ans)
    return(ans)
}

CodeCatMemoButton <- function(label="Memo",...){
    CodCatMemB <- gbutton(label,handler=function(h,...) {
        MemoWidget("Code Category",.rqda$.CodeCatWidget,"codecat")
        }
                          )
    assign("CodCatMemB", CodCatMemB,envir=button)
    enabled(CodCatMemB) <- FALSE
    CodCatMemB
}
## MemoWidget() is moved to utils.R

plotCodeCategory <-function(parent=NULL){
    if (is.null(parent)) parent <- svalue(.rqda$.CodeCatWidget)
    ans <- RQDAQuery(sprintf("select codecat.name as parent,freecode.name as child from treecode, codecat,freecode
where treecode.status=1 and codecat.status=1 and freecode.status=1
and treecode.catid=codecat.catid and freecode.id=treecode.cid and codecat.name in (%s)",paste(shQuote(parent),collapse=",")))
    Encoding(ans$parent) <- "UTF-8"
    Encoding(ans$child) <- "UTF-8"
    g <- igraph::graph.data.frame(ans)
    tryCatch(igraph::tkplot(g,vertex.label=igraph::V(g)$name),error=function(e){
        igraph::plot.igraph(g,vertex.label=igraph::V(g)$name)
    })
}

d3CodeCategory <-function(parent=NULL){
  if (is.null(parent)) parent <- svalue(.rqda$.CodeCatWidget)
  ans <- RQDAQuery(sprintf("select codecat.name as parent,freecode.name as child from treecode, codecat,freecode
where treecode.status=1 and codecat.status=1 and freecode.status=1
and treecode.catid=codecat.catid and freecode.id=treecode.cid and codecat.name in (%s)",paste(shQuote(parent),collapse=",")))
  Encoding(ans$parent) <- "UTF-8"
  Encoding(ans$child) <- "UTF-8"
  file = paste(tempfile(), "html", sep=".")
  d3Network::d3SimpleNetwork(ans, width = gdkScreenWidth(), height = gdkScreenHeight(), file=file(file, encoding="UTF-8"))
  browseURL(file)
}

getCodingsByCategory <- function(catid=NULL, fid = NULL, codingTable = c("coding", "coding2")){
    if (is.null(catid)) catid <- RQDAQuery(sprintf("select catid from codecat where name = '%s'", enc(svalue(.rqda$.CodeCatWidget))))$catid
    cid <- RQDAQuery(sprintf("select cid from treecode where catid==%s and status==1",catid))$cid
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
    ## class(ct) <- c("codingsByOne", "data.frame")
    ct
}

CodeCatWidgetMenu <- list()
CodeCatWidgetMenu$"Add New Code to Selected Category"$handler <- function(h,...) {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        codename <- ginput("Enter new code. ", icon="info")
        if (!is.na(codename)){
            codename <- enc(codename,encoding="UTF-8")
            addcode(codename)
            CodeNamesUpdate(sortByTime=FALSE)
            cid <- RQDAQuery(sprintf("select id from freecode where status=1 and name='%s'",codename))$id
            ## end of add a new code to free code.
            SelectedCodeCat <- svalue(.rqda$.CodeCatWidget)
            if (length(SelectedCodeCat)==0) {gmessage("Select a code category first.",container=TRUE)} else{
                catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from codecat where status=1 and name='%s'",SelectedCodeCat))[,1]
                ## CodeList and the id (table freecode): sql -> name and id where status==1
                Dat <- data.frame(cid=cid,catid=catid,date=date(),dateM=date(),memo="",
                                  status=1, owner=.rqda$owner)
                ## Push selected codeList to table treecode
                ok <- dbWriteTable(.rqda$qdacon,"treecode",Dat,row.names=FALSE,append=TRUE)
                if (ok) {
                ## update .CodeofCat Widget
                    UpdateCodeofCatWidget()
                } else gmessage("Failed to assign code category")
            }
        }
    }
}
CodeCatWidgetMenu$"Codings of selected category"$handler <- function(h,...){
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
        ct <- getCodingsByCategory(fid=getFileIds(condition=.rqda$TOR))
        print.codingsByOne(ct)
    }
}
CodeCatWidgetMenu$Memo$handler <- function(h,...){
 if (is_projOpen(envir=.rqda,conName="qdacon")) {
 MemoWidget("Code Category",.rqda$.CodeCatWidget,"codecat")
}
}
##psccFun <- function(h,...){
##    plotCodeCategory()
##}
##psccItem <- gaction("Plot Selected Code Category", handler=psccFun,toolkit=guiToolkit("RGtk2"))
## need to manually set the toolkit
##CodeCatWidgetMenu$"Plot Selected Code Category" <- psccItem
CodeCatWidgetMenu$"Plot Selected Code Categories"$handler <- function(h,...){plotCodeCategory()}
CodeCatWidgetMenu$"Plot Selected Code Categories with d3"$handler <- function(h,...){d3CodeCategory()}

CodeCatWidgetMenu$"Sort by created time"$handler <- function(h,...){
 if (is_projOpen(envir=.rqda,conName="qdacon")) {
   UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat")
   ## UpdateCodeofCatWidget() ## wrong function
}
}


##
CodeofCatWidgetMenu <- list()
CodeofCatWidgetMenu$"Rename Selected Code"$handler <- function(h, ...) {
  selectedCodeName <- svalue(.rqda$.CodeofCat)
  if (length(selectedCodeName)==0){
    gmessage("Select a code first.",icon="error",container=TRUE)
  }
  else {
    NewCodeName <- ginput("Enter new code name. ", text=selectedCodeName, icon="info")
    if (!is.na(NewCodeName)) {
      Encoding(NewCodeName) <- Encoding(selectedCodeName) <- "UTF-8"
      rename(selectedCodeName,NewCodeName,"freecode")
      UpdateWidget(".codes_rqda",from=selectedCodeName,to=NewCodeName)
      UpdateWidget(".CodeofCat",from=selectedCodeName,to=NewCodeName)
    }
  }
}
CodeofCatWidgetMenu$"Code Memo"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    MemoWidget("Code",.rqda$.CodeofCat,"freecode")
    }
  }
CodeofCatWidgetMenu$"Sort by created time"$handler <- function(h,...){
 if (is_projOpen(envir=.rqda,conName="qdacon")) {
 UpdateCodeofCatWidget()
}
}
