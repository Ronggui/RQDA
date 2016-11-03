setColor <- function(currentColor="gold"){
  currentColor <- gdkColorParse(currentColor)$color
  colorDA <- gtkDrawingAreaNew()
  colorDA$modifyBg("normal", currentColor)
  g <-glayout(container=gwindow(width=50,height=20,parent=getOption("widgetCoordinate")),homogeneous=TRUE,title=gettext("Change color.", domain = "R-RQDA"))
  g[1,1:3] <- colorDA
  g[2,1] <- gbutton(gettext("Select Color", domain = "R-RQDA"),handler=function(h,...){
  dialog <- gtkColorSelectionDialogNew(gettext("Changing color", domain = "R-RQDA"), show=T)
  colorsel <- dialog[["colorsel"]]
  colorsel$setPreviousColor(currentColor)
  colorsel$setCurrentColor(currentColor)
  colorsel$setHasPalette(TRUE)
  response <- dialog$run()
  if (response == GtkResponseType["ok"])
    {
      currentColor <- colorsel$getCurrentColor()$color
      colorString <- gdkColorToString(currentColor)
      colorDA$modifyBg("normal", currentColor)
    }
  dialog$destroy()
  })
  g[2,2] <- gbutton(gettext("OK", domain = "R-RQDA"),handler=function(h,...) {
   dispose(g)})
  g[2,3] <- gbutton(gettext("Cancel", domain = "R-RQDA"),handler=function(h,...) {
   dispose(g)})
}
