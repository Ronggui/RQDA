EditVarWidget <- function(ExistingItems=NULL,container=NULL,title=NULL,ID=NULL,saveFUN=NULL,...){
  ## modified from RGtk2 package
  ## ExistingItems: existing data set for a case/file etc. It is data frame of 2 columns, the first is Variable
  ## saveFUN is character.
  ## container: similar to that of gWidget package.
  COLUMN <- c(Variable = 0, Value = 1,  editable = 2)
  articles <- NULL

  create.model <- function()
    {
      ## create the array of data
      articles <<- list()
      ##  create list store
      model <- gtkListStoreNew( "gchararray", "gchararray", "gboolean")
      ## add item from ExistingItems
      ## needs modification
      if (!is.null(ExistingItems)){
        articles <<- c(articles,unlist(apply(ExistingItems,1,function(x) list(list(Variable=x[1],Value=x[2],editable=TRUE))),FALSE))
        for (i in 1:length(articles))
          {
            iter <- model$append()$iter
            model$set(iter, COLUMN["Variable"], articles[[i]]$Variable,
                      COLUMN["Value"], articles[[i]]$Value,
                      COLUMN["editable"], articles[[i]]$editable)
          }
      }
      return(model)
    }

  cell.edited <- function(cell, path.string, new.text, data)
    {
      Encoding(new.text) <- 'UTF-8' ## now atrribute displays correctly for non-english character
      checkPtrType(data, "GtkListStore")
      model <- data
      path <- gtkTreePathNewFromString(path.string)
      column <- cell$getData("column")
      iter <- model$getIter(path)$iter
      if (column==1){
               i <- path$getIndices()[[1]]+1
               articles[[i]]$Value <<- new.text
               model$set(iter, column, articles[[i]]$Value)
             }
    }

  add.columns <- function(treeview)
    {
      model <- treeview$getModel()
      ## Variable column
      renderer <- gtkCellRendererTextNew()
      gSignalConnect(renderer, "edited", cell.edited, model)
      renderer$setData("column", COLUMN["Variable"])
      treeview$insertColumnWithAttributes(-1, "Variable", renderer,text = COLUMN[["Variable"]], editable = COLUMN[["editable"]])
      ## Value column
      renderer <- gtkCellRendererTextNew()
      gSignalConnect(renderer, "edited", cell.edited, model)
      renderer$setData("column", COLUMN["Value"])
      treeview$insertColumnWithAttributes(-1, "Value", renderer, text = COLUMN[["Value"]],editable = COLUMN[["editable"]])
    }

    saveFUN <- get(saveFUN,mode="function")

  ## create window, etc
  window <- gtkWindowNew("toplevel", show = F)
  Encoding(title) <- 'UTF-8'
  window$setTitle(paste("Attribute of:",title))
  window$setBorderWidth(5)
  vbox <- gtkVBoxNew(FALSE, 5)
  window$add(vbox)
  sw <- gtkScrolledWindowNew(NULL, NULL)
  sw$setShadowType("etched-in")
  sw$setPolicy("automatic", "automatic")
  vbox$packStart(sw, TRUE, TRUE, 0)
  ## create model
  model <- create.model()
  ## create tree view
  treeview <- gtkTreeViewNewWithModel(model)
  treeview$setRulesHint(TRUE)
  treeview$getSelection()$setMode("single")
  add.columns(treeview)
  sw$add(treeview)
  ## some buttons
  hbox <- gtkHBoxNew(TRUE, 4)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("Save and Close")
  gSignalConnect(button, "clicked",saveFUN,list(model,window,ExistingItems,list(...)))
  hbox$packStart(button, TRUE, TRUE, 0)
  window$setDefaultSize(300, 350)
  window$Move(size(.rqda$.root_rqdagui)[1],2)
  window$showAll()
  invisible(window)
}

saveFUN4CaseAttr <- function(button,data){
  ## the first arg must button, and data as second.
  ## push dataset into project file.
  model <- data[[1]]
  window <- data[[2]]
  ExistingItems <- data[[3]]
  MoreArgs <- data[[4]]
  IterFirst <- model$getIterFirst()
  cond <- IterFirst[[1]]
  iter <- IterFirst$iter
  ans <- c()
  while(cond) {
    dat <- unlist(model$get(iter, 0, 1))
    ans <- c(ans,dat)
    cond <- model$iterNext(iter)
  }
  n <- length(ans)
  if (n >= 2){
    idx1 <- seq(1,to=n,by=2)
    idx2 <- seq(2,to=n,by=2)
    ans <- data.frame(Variable=ans[idx1],Value=ans[idx2],stringsAsFactors=FALSE)
    ## cal which variable is added and which is modified
    ExistingItems$value[which(is.na(ExistingItems$value))] <- "NA" ## add this line to address NA.
    change_idx <- ans$Value != ExistingItems$value
    mod_idx <- change_idx & (ExistingItems$value!= "NA")
    new_idx <- change_idx & (! mod_idx)
    if (any(mod_idx)) {
    ## alter the table for the modified variable
    vars <- ans[mod_idx,]
    apply(vars,1,FUN=function(x) dbGetQuery(.rqda$qdacon,sprintf("update caseAttr set value = '%s' where variable = '%s' and caseID ='%s' and status =1",x[2],x[1],MoreArgs$caseId)))
    }
    if (any(new_idx)){
    ## add the new variable to table
    vars <- data.frame(variable=ans[new_idx,1],value=ans[new_idx,2],caseID=MoreArgs$caseId,date=date(),dateM=NA,owner=.rqda$owner,status=1)
    dbWriteTable(.rqda$qdacon, "caseAttr", vars, append = TRUE,row.names=FALSE)
  }
  }
  window$Destroy()## close
}

CaseAttrFun <- function(caseId,title=NULL,attrs=svalue(.rqda$.AttrNamesWidget)){
  if (length(attrs)==0) attrs <-  dbGetQuery(.rqda$qdacon,"select name from attributes where status=1")$name
  if (is.null(attrs)) gmessage("add attribute in Attrs Tabe first.",container=TRUE) else{
    attrs2 <- data.frame(variable=attrs,value="NA",stringsAsFactors=FALSE)
    variables <- dbGetQuery(.rqda$qdacon,sprintf("select variable, value from caseAttr where caseID=%i and variable in (%s) and status=1", caseId,paste(shQuote(attrs),collapse=",")))
    if (nrow(variables)!=0){
      Encoding(variables$variable) <- Encoding(variables$value) <- 'UTF-8'
      idx <- match(variables[[1]],attrs2[[1]])
      attrs2[idx,] <- variables
    }
    EditVarWidget(ExistingItems=attrs2,saveFUN="saveFUN4CaseAttr",title=title,caseId=caseId)
    ## get attrs list and turn it to a data frame, pass it to ExistingItems, then call EditVarWidget
  }
}

saveFUN4FileAttr <- function(button,data){
  ## the first arg must button, and data as second.
  ## push dataset into project file.
  model <- data[[1]]
  window <- data[[2]]
  ExistingItems <- data[[3]]
  MoreArgs <- data[[4]]
  IterFirst <- model$getIterFirst()
  cond <- IterFirst[[1]]
  iter <- IterFirst$iter
  ans <- c()
  while(cond) {
    dat <- unlist(model$get(iter, 0, 1))
    ans <- c(ans,dat)
    cond <- model$iterNext(iter)
  }
  n <- length(ans)
  if (n >= 2){
    idx1 <- seq(1,to=n,by=2)
    idx2 <- seq(2,to=n,by=2)
    ans <- data.frame(Variable=ans[idx1],Value=ans[idx2],stringsAsFactors=FALSE)
    ## cal which variable is added and which is modified
    change_idx <- ans$Value != ExistingItems$value
    mod_idx <- change_idx & (ExistingItems$value!= "NA")
    new_idx <- change_idx & (! mod_idx)
    if (any(mod_idx)) {
    ## alter the table for the modified variable
    vars <- ans[mod_idx,]
    apply(vars,1,FUN=function(x) dbGetQuery(.rqda$qdacon,sprintf("update fileAttr set value = '%s' where variable = '%s' and fileID ='%s'and status=1",x[2],x[1],MoreArgs$fileId)))
    }
    if (any(new_idx)){
    ## add the new variable to table
    vars <- data.frame(variable=ans[new_idx,1],value=ans[new_idx,2],fileID=MoreArgs$fileId,date=date(),dateM=NA,owner=.rqda$owner,status=1)
    dbWriteTable(.rqda$qdacon, "fileAttr", vars, append = TRUE,row.names=FALSE)
    }
  }
  window$Destroy()## close
}

FileAttrFun <- function(fileId,title=NULL,attrs=svalue(.rqda$.AttrNamesWidget)){
  if (length(attrs)==0) attrs <-  dbGetQuery(.rqda$qdacon,"select name from attributes where status=1")$name
  if (is.null(attrs)) gmessage("add attribute in Attrs Tabe first.",container=TRUE) else{
    Encoding(attrs) <- 'UTF-8'
    attrs2 <- data.frame(variable=attrs,value="NA",stringsAsFactors=FALSE)
    variables <- dbGetQuery(.rqda$qdacon,sprintf("select variable, value from fileAttr where fileID=%i and variable in (%s) and status=1",fileId,paste(shQuote(attrs),collapse=",")))
    if (nrow(variables)!=0){
      Encoding(variables$variable) <- Encoding(variables$value) <- 'UTF-8'
      idx <- match(variables[[1]],attrs2[[1]])
      attrs2[idx,] <- variables
    }
    EditVarWidget(ExistingItems=attrs2,saveFUN="saveFUN4FileAttr",title=title,fileId=fileId)
    ## get attrs list and turn it to a data frame, pass it to ExistingItems, then call EditVarWidget
  }
}


## change the name of Variables.R to Attributes.R

AttrNamesUpdate <- function(Widget=.rqda$.AttrNamesWidget,sortByTime=FALSE,decreasing=FALSE,...)
{
  if (is_projOpen()){
    attr <- dbGetQuery(.rqda$qdacon, "select name, date from attributes where status=1")
    if (nrow(attr)==0) {
      attr <- NULL
    } else {
      attr <- attr$name
      Encoding(attr) <- "UTF-8"
      if (!sortByTime) {attr <- sort(attr)} else {
        attr <- attr[OrderByTime(attr$date,decreasing=decreasing)]
      }
    }
    tryCatch(Widget[] <- attr, error=function(e){})
  }
}

AddAttrNames <- function(name,...) {
  if (name != ""){
    con <- .rqda$qdacon
    dup <- dbGetQuery(con,sprintf("select name from attributes where name='%s'",name))
    if (nrow(dup)==0) {
      dbGetQuery(con,sprintf("insert into attributes (name,status,date,owner) values ('%s', %i,%s, %s)",
                             name,1, shQuote(date()),shQuote(.rqda$owner)))
    }
  }
}

AddAttrButton <- function(label="ADD"){
  AddAttB <- gbutton(label,handler=function(h,...) {
    AttrName <- ginput("Enter new Attr Name. ", icon="info")
    if (!is.na(AttrName)) {
      Encoding(AttrName) <- "UTF-8"
      invalid <- grepl("'",AttrName)
      if (invalid) {
        gmessage("Attribute should NOT contain '.",container=TRUE)
      } else {
        if (AttrName %in% c("fileID","caseID")) {
          gmessage("This is a reserved keyword.",container=TRUE)
        } else{
          AddAttrNames(AttrName)
          AttrNamesUpdate()
        }
      }
    }
  }
                     )
  assign("AddAttB",AddAttB,envir=button)
  enabled(AddAttB) <- FALSE
  AddAttB
}


DeleteAttrButton <- function(label="Delete"){
  DelAttB <- gbutton(label,handler=function(h,...) {
    del <- gconfirm("Really delete the Attribute?",icon="question")
    if (isTRUE(del)){
      Selected <- svalue(.rqda$.AttrNamesWidget)
      Selected <- enc(Selected,"UTF-8")
      dbGetQuery(.rqda$qdacon,sprintf("update attributes set status=0 where name='%s'",Selected))
      RQDAQuery(sprintf("update caseAttr set status=0 where variable='%s'",Selected))
      RQDAQuery(sprintf("update fileAttr set status=0 where variable='%s'",Selected))
      AttrNamesUpdate()
    }
  }
                     )
  assign("DelAttB",DelAttB,envir=button)
  enabled(DelAttB) <- FALSE
  DelAttB
}


RenameAttrButton <- function(label="Rename"){
  RenAttB <- gbutton(label,handler=function(h,...) {
    selected <- svalue(.rqda$.AttrNamesWidget)
    NewName <- ginput("Enter new attribute name. ", text=selected, icon="info")
    if (!is.na(NewName)){
      Encoding(NewName) <- "UTF-8"
      selected <- enc(selected,encoding="UTF-8")
      invalid <- grepl("'",NewName)
      if (invalid) {
        gmessage("Attribute should NOT contain '.",container=TRUE)
      } else {
        exists <- dbGetQuery(.rqda$qdacon, sprintf("select * from attributes where name = '%s' ",NewName))
        if (nrow(exists) > 0 ){
          gmessage("Name duplicated. Please use anaother name.",cont=TRUE)
        } else {
          dbGetQuery(.rqda$qdacon, sprintf("update attributes set name = '%s' where name = '%s' ",NewName,selected))
          dbGetQuery(.rqda$qdacon, sprintf("update caseAttr set variable = '%s' where variable = '%s' ",NewName,selected))
          dbGetQuery(.rqda$qdacon, sprintf("update fileAttr set variable = '%s' where variable = '%s' ",NewName,selected))
          AttrNamesUpdate()
        }
      }
    }
  }
                     )
  assign("RenAttB",RenAttB,envir=button)
  enabled(RenAttB) <- FALSE
  RenAttB
}

AttrMemoButton <- function(label="Memo"){
  AttMemB <- gbutton(label,handler=function(h,...) {
    MemoWidget("Attributes",.rqda$.AttrNamesWidget,"attributes")
  }
                     )
  assign("AttMemB",AttMemB,envir=button)
  enabled(AttMemB) <- FALSE
  AttMemB
}

viewCaseAttr <- function(){
  DF <- dbGetQuery(.rqda$qdacon,"select variable,value, caseId from caseAttr where status=1")
  DF <- reshape(DF,v.names="value",idvar="caseID",direction="wide",timevar="variable")
  names(DF) <- gsub("^value.","",names(DF))
  caseName <- dbGetQuery(.rqda$qdacon,"select name,id from cases where status=1")
  if (nrow(caseName)!=0){
    names(caseName) <- c("case","caseID")
    Encoding(caseName$case) <- "UTF-8"
    DF <- merge(caseName,DF)
    gtable(DF,container=TRUE)
  }
}

viewFileAttr <- function(){
  DF <- dbGetQuery(.rqda$qdacon,"select variable,value, fileId from fileAttr where status=1")
  DF <- reshape(DF,v.names="value",idvar="fileID",direction="wide",timevar="variable")
  names(DF) <- gsub("^value.","",names(DF))
  fileName <- dbGetQuery(.rqda$qdacon,"select name,id from source where status=1")
  if (nrow(fileName)!=0){
    names(fileName) <- c("file","fileID")
    Encoding(fileName$file) <- "UTF-8"
    DF <- merge(fileName,DF)
    gtable(DF,container=TRUE)
  }
}


GetAttr <- function(type=c("case","file"),attrs=svalue(.rqda$.AttrNamesWidget),subset){
  if (is_projOpen()){
  type <-  match.arg(type)
  if (length(attrs)==0) attrs <- NULL
  inClause <- ifelse(is.null(attrs),"",sprintf("where status=1 and variable in (%s)",paste(shQuote(attrs),collapse=",")))
  if (type == "case"){
    RQDAQuery("delete from caseAttr where value='NA'")
    RQDAQuery("delete from caseAttr where value=''") ## clean the table
    DF <- dbGetQuery(.rqda$qdacon,sprintf("select variable,value, caseId from caseAttr %s",inClause))
    if (nrow(DF) > 0 ){
    Encoding(DF$variable) <- Encoding(DF$value) <- "UTF-8"
    DF <- reshape(DF,v.names="value",idvar="caseID",direction="wide",timevar="variable")
    names(DF) <- gsub("^value.","",names(DF))
    caseName <- dbGetQuery(.rqda$qdacon,"select name,id from cases where status=1")
    if (nrow(caseName)!=0){
      names(caseName) <- c("case","caseID")
      Encoding(caseName$case) <- "UTF-8"
      DF <- merge(caseName,DF)
      class(DF) <- c("CaseAttr","data.frame")
    }}
  } else if (type=="file"){
    RQDAQuery("delete from fileAttr where value='NA'")
    RQDAQuery("delete from fileAttr where value=''") ## clean the table
    DF <- dbGetQuery(.rqda$qdacon,sprintf("select variable,value, fileId from fileAttr %s",inClause))
    if (nrow(DF) > 0 ){
    Encoding(DF$variable) <- Encoding(DF$value) <- "UTF-8"
    DF <- reshape(DF,v.names="value",idvar="fileID",direction="wide",timevar="variable")
    names(DF) <- gsub("^value.","",names(DF))
    fileName <- dbGetQuery(.rqda$qdacon,"select name,id from source where status=1")
    if (nrow(fileName)!=0){
      names(fileName) <- c("file","fileID")
      Encoding(fileName$file) <- "UTF-8"
      DF <- merge(fileName,DF)
      class(DF) <- c("FileAttr","data.frame")
    }}
  }
  tt <- RQDAQuery("select name, class from attributes")
  attrs <- tt[tt$class=="numeric","name"]
  idx <- which(names(DF) %in% attrs)
  DF[,idx]<-as.data.frame(apply(DF[,idx,drop=FALSE],2,as.numeric))
  if (missing(subset)) DF else {
      r <- eval(substitute(subset),DF)
      if (!is.logical(r))
          stop("'subset' must evaluate to logical")
      r <- r & !is.na(r)
      DF <- DF[r,,drop=FALSE]
      DF
  }
}}

SetAttrClsButton <- function(label="Class"){
  ans <- gbutton(label,handler=function(h,...) {
    setAttrType()
  }
                 )
  gtkWidgetSetTooltipText(getToolkitWidget(ans),"Set class of selected attribute.\nIt can be 'numeric' or 'character'.")
  assign("SetAttClsB",ans,envir=button)
  enabled(ans) <- FALSE
  ans
}


setAttrType <- function() {
    Selected <- enc(svalue(.rqda$.AttrNamesWidget),encoding="UTF-8")
    oldCls <- tryCatch(dbGetQuery(.rqda$qdacon,sprintf("select class from attributes where status=1 and name='%s'",Selected))[1,1],
                       error=function(e){
                         dbGetQuery(.rqda$qdacon, "alter table attributes add column class text")
                         dbGetQuery(.rqda$qdacon,sprintf("select class from attributes where status=1 and name='%s'",Selected))[1,1]
                       })
    if (is.null(oldCls)||is.na(oldCls)) {
      items <- c("unspecified","numeric","character")
      idx <- 1
    } else {
      items <- c("numeric","character")
      idx <- which (items %in%  oldCls)
    }
    w <- gwindow("Type of attributes",height=30,width=150)
    gp <- ggroup(horizontal=FALSE,container=w)
    rb <- gradio(items,idx,horizontal=TRUE, container=gp)
    gbutton("OK",container=gp,handler=function(h,...){
      if ((newCls <- svalue(rb))!= "unspecified"){
        dbGetQuery(.rqda$qdacon,sprintf("update attributes set class='%s' where status=1 and name='%s'",newCls,Selected))
      }
      dispose(w)
    })}


importAttr <- function(data, type='file', filename){
  idx <- match(filename, names(data))
  dat <- data[, -idx,drop=FALSE]
  fn <- getFiles()
  fid <- getFiles(names=F)
  fnuser <- data[,filename]
  if (!all(fnuser %in% fn)) stop("some files are not in the rqda project.")
  fid <- fid[match(data[[filename]],fn)]
  allAtt <- RQDAQuery("select name from attributes where status=1")$name
  if (!all(names(dat) %in% allAtt)) stop("some attributes are in not the rqda project.")
  for (att in names(dat)) {
    attval <- dat[[att]]
    if (mode(attval) == "character" && Encoding(attval) != "UTF-8") attval <- iconv(attval, to='UTF-8')
    for (i in 1:nrow(dat)) {
      exist <- RQDAQuery(sprintf("select value from fileAttr where variable='%s' and fileID=%i and status=1", att, fid[i]))
      if (nrow(exist)==0 && !is.na(attval[i])) {
        RQDAQuery(sprintf("insert into fileAttr (variable, value, fileID, date, owner, status) 
                          values ('%s','%s','%s','%s','rghuang',1)",att, attval[i], fid[i], as.character(date())))
        }
    }
    }
}

#######################
## Defunct functions
#######################

## AddVarWidget <- function(ExistingItems=NULL,container=NULL,title=NULL,ID=NULL){
##   ## modified from RGtk2 package
##   ## ExistingItems: existing data set for a case/file etc. It is data frame of 2 columns, the first is Variable
##   ## container: similar to that of gWidget package.
##   COLUMN <- c(Variable = 0, Value = 1,  editable = 2)
##   articles <- NULL

##   create.model <- function()
##     {
##       ## create the array of data
##       articles <<- list()
##       ##  create list store
##       model <- gtkListStoreNew( "gchararray", "gchararray", "gboolean")
##       ## add item from ExistingItems
##       ## needs modification
##       if (!is.null(ExistingItems)){
##         articles <<- c(articles,unlist(apply(ExistingItems,1,function(x) list(list(Variable=x[1],Value=x[2],editable=TRUE))),FALSE))
##         for (i in 1:length(articles))
##           {
##             iter <- model$append()$iter
##             model$set(iter, COLUMN["Variable"], articles[[i]]$Variable,
##                       COLUMN["Value"], articles[[i]]$Value,
##                       COLUMN["editable"], articles[[i]]$editable)
##           }
##       }
##       return(model)
##     }

##   add.item <- function(button, data)
##     {
##       stopifnot(!is.null(articles))
##       foo <- list(Variable = "New Var Name", Value = "NA", editable = TRUE)
##       articles <<- c(articles, foo)
##       iter <- model$append()$iter
##       model$set(iter, COLUMN["Variable"], foo$Variable,
##                 COLUMN["Value"], foo$Value,
##                 COLUMN["editable"], foo$editable)
##     }

##   remove.item <- function(widget, data)
##     {
##       checkPtrType(data, "GtkTreeView")
##       treeview <- data
##       model <- treeview$getModel()
##       selection <- treeview$getSelection()
##       selected <- selection$getSelected()
##       if (selected[[1]])
##         {
##           iter <- selected$iter
##           path <- model$getPath(iter)
##           i <- path$getIndices()[[1]]
##           model$remove(iter)
##           articles <<- articles[-i]
##         }
##     }

##   cell.edited <- function(cell, path.string, new.text, data)
##     {
##       checkPtrType(data, "GtkListStore")
##       model <- data
##       path <- gtkTreePathNewFromString(path.string)
##       column <- cell$getData("column")
##       iter <- model$getIter(path)$iter
##       switch(column+1,
##              {
##                old.text <- model$get(iter, column)
##                i <- path$getIndices()[[1]]+1
##                articles[[i]]$Variable <<- new.text
##                model$set(iter, column, articles[[i]]$Variable)
##              },
##              {
##                i <- path$getIndices()[[1]]+1
##                articles[[i]]$Value <<- new.text
##                model$set(iter, column, articles[[i]]$Value)
##              }
##              )
##     }

##   add.columns <- function(treeview)
##     {
##       model <- treeview$getModel()
##       ## Variable column
##       renderer <- gtkCellRendererTextNew()
##       gSignalConnect(renderer, "edited", cell.edited, model)
##       renderer$setData("column", COLUMN["Variable"])
##       treeview$insertColumnWithAttributes(-1, "Variable", renderer,text = COLUMN[["Variable"]], editable = COLUMN[["editable"]])
##       ## Value column
##       renderer <- gtkCellRendererTextNew()
##       gSignalConnect(renderer, "edited", cell.edited, model)
##       renderer$setData("column", COLUMN["Value"])
##       treeview$insertColumnWithAttributes(-1, "Value", renderer, text = COLUMN[["Value"]],editable = COLUMN[["editable"]])
##     }

##   save.project <- function(button,data){
##     ## push dataset into project file.
##      IterFirst <- data$getIterFirst()
##      cond <- IterFirst[[1]]
##      iter <- IterFirst$iter
##      ans <- c()
##      while(cond) {
##        dat <- unlist(data$get(iter, 0, 1))
##        ans <- c(ans,dat)
##        cond <- data$iterNext(iter)
##      }
##      n <- length(ans)
##      idx1 <- seq(1,to=n,by=2)
##      idx2 <- seq(2,to=n,by=2)
##      ans <- data.frame(Variable=ans[idx1],Value=ans[idx2])
##      ans <- cbind(ans,ID)
##      dbGetQuery(.rqda$qdacon,sprintf("delete from caseAttr where caseid='%s'",ID))
##      dbWriteTable(.rqda$qdacon, "caseAttr", ans, append = TRUE,row.names=FALSE)
##      window$Destroy()## close
##    }

##   ## create window, etc
##   window <- gtkWindowNew("toplevel", show = F)
##   window$setTitle(paste("Var:",title))
##   window$setBorderWidth(5)
##   vbox <- gtkVBoxNew(FALSE, 5)
##   window$add(vbox)
##   sw <- gtkScrolledWindowNew(NULL, NULL)
##   sw$setShadowType("etched-in")
##   sw$setPolicy("automatic", "automatic")
##   vbox$packStart(sw, TRUE, TRUE, 0)
##   ## create model
##   model <- create.model()
##   ## create tree view
##   treeview <- gtkTreeViewNewWithModel(model)
##   treeview$setRulesHint(TRUE)
##   treeview$getSelection()$setMode("single")
##   add.columns(treeview)
##   sw$add(treeview)
##   ## some buttons
##   hbox <- gtkHBoxNew(TRUE, 4)
##   vbox$packStart(hbox, FALSE, FALSE, 0)
##   button <- gtkButtonNewWithLabel("Add")
##   gSignalConnect(button, "clicked", add.item, model)
##   hbox$packStart(button, TRUE, TRUE, 0)
##   button <- gtkButtonNewWithLabel("Remove")
##   gSignalConnect(button, "clicked", remove.item, treeview)
##   hbox$packStart(button, TRUE, TRUE, 0)
##   button <- gtkButtonNewWithLabel("Save")
##   gSignalConnect(button, "clicked",save.project,model)
##   hbox$packStart(button, TRUE, TRUE, 0)
##   window$setDefaultSize(150, 350)
##   window$showAll()
##   invisible(window)
## }
