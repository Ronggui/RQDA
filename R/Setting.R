addSettingGUI <- function(container,width=12){
  colorsList <- colors()
  if (Sys.info()["user"]!="") assign("owner",Sys.info()["user"],envir=.rqda)
  Setting <- list(type = "ggroup",
                  horizontal = FALSE,
                  children = list(
                    list(type="fieldset",
                         columns = 2,
                         label = "Settings",
                         label.pos = "top",
                         label.font = c(weight="bold"),
                         children = list(
                           list(name = "owner",
                                label = "Name of Coder",
                                type = "gedit",width=width,
                                text = .rqda$owner
                                ),
                           list(name = "encoding",
                                label = "File Encoding",
                                type = "gedit",width=width,
                                text = .rqda$encoding
                                ),
                           list(name = "fore.col",
                                label = "Color for Coding",
                                ## type = "gedit",width=width,
                                ## text = .rqda$fore.col
                                type = "gcombobox",
                                items=c(.rqda$fore.col,colorsList)
                                ),
                           list(name = "back.col",
                                label = "Color for Case",
                                ## type = "gedit",width=width,
                                ## text = .rqda$back.col
                                type = "gcombobox",
                                items=c(.rqda$back.col,colorsList)
                                ),
                           list(name = "codingTable",
                                label = "Current coding table",
                                type = "gcombobox",
                                items=c(.rqda$codingTable,"coding2")
                                ),
                           list(name = "BOM",
                                label = "Byte Order Mark",
                                type = "gcombobox",## width=width,
                                items = c(FALSE, TRUE)
                                ),
                           list(name = "SFP",
                                label = "Show File Property",
                                type = "gcombobox",## width=width,
                                items = c(FALSE, TRUE)
                                ),
                           list(name = "TOR",
                                type="gcombobox",
                                label = "Type of Retrieval",
                                items = c(.rqda$TOR, "case", "filecategory","both")
                                )
                           )
                         )
                    )
                  )

  ans <- glabel("Click to set font",container = container,handler=function(h,...) setFont(default=.rqda$font))## set font for widget
  gtkWidgetSetTooltipText(getToolkitWidget(ans),"Set fonts for memo widgets.")

  SettingFL <- gformlayout(Setting, container = container, expand=TRUE)

  ButtonContainer <- ggroup(container = container) ##, width=100) ## not necessary to set width here
  addSpring(ButtonContainer)
  resetButton <- gbutton("Default", container = ButtonContainer)
  okButton <- gbutton("OK", container = ButtonContainer)

  addHandlerChanged(okButton, function(h,...) {
    out <- svalue(SettingFL)
    tryCatch(ClearMark(.rqda$.root_edit,0,nchar(svalue(.rqda$.openfile_gui)),TRUE,TRUE),error=function(e){})
    for (i in names(out)) assign(i,out[[i]],envir=.rqda)
  })

  addHandlerChanged(resetButton, function(h,...) {
    tryCatch(ClearMark(.rqda$.root_edit,0,nchar(svalue(.rqda$.openfile_gui)),TRUE,TRUE),error=function(e){})
    tryCatch(svalue(SettingFL[]$BOM) <- FALSE,error=function(e){})
    tryCatch(svalue(SettingFL[]$SFP) <- FALSE,error=function(e){})
    tryCatch(svalue(SettingFL[]$encoding) <- "unknown",error=function(e){})
    tryCatch(svalue(SettingFL[]$owner) <- "default",error=function(e){})
    tryCatch(svalue(SettingFL[]$back.col) <- "gold",error=function(e){})
    tryCatch(svalue(SettingFL[]$fore.col) <- "blue",error=function(e){})
    tryCatch(svalue(SettingFL[]$codingTable) <- "coding",error=function(e){})
    tryCatch(svalue(SettingFL[]$TOR) <- "unconditional",error=function(e){})
    assign("BOM",FALSE,envir=.rqda)
    assign("SFP",FALSE,envir=.rqda)
    assign("encoding","unknown",envir=.rqda)
    assign("owner","default",envir=.rqda)
    assign("back.col","gold",envir=.rqda)
    assign("fore.col","blue",envir=.rqda)
    assign("codingTable","coding",envir=.rqda)
    assign("TOR","unconditional",envir=.rqda)
    assign("font","Sans 11",envir=.rqda)
  })}

setFont <- function(default="Sans 11"){
  font <- gtkFontButtonNew()
  gtkFontButtonSetFontName(font,default)
  g <-glayout(container=gwindow(width=50,height=30,parent=getOption("widgetCoordinate")),homogeneous=TRUE)
  g[1,1:2] <- font
  g[2,1] <- gbutton("Ok",handler=function(h,...){
    ans <- font$GetFontName()
    assign("font",ans, envir=.rqda)
    dispose(g)
  })
  g[2,2] <- gbutton("Cancel",handler=function(h,...) dispose(g))
}
