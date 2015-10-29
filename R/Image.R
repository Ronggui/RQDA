ViewImage <- function(file,width=800,height=600,...){
  da <- gtkDrawingArea()
  env <- new.env()
  img <- gdkPixbufNewFromFile(file)$retval
  expose_event <- function(widget,event,data){
    assign("drawable",widget[["window"]],envir=env)
    gdkDrawPixbuf(env$drawable, gc = NULL, pixbuf=data$img,
                  event[["area"]][["x"]], event[["area"]][["y"]],
                  event[["area"]][["x"]], event[["area"]][["y"]],
                  event[["area"]][["width"]], event[["area"]][["height"]])
    return(FALSE)
  }

  imgW <- img$GetWidth()
  imgH <- img$GetHeight()
  scale <- min(c(width/imgW,height/imgH))
  scale <- ifelse(scale>1,1,scale)
  width <- imgW*scale
  height <-imgH*scale
  img <- img$ScaleSimple(width,height,'bilinear')
  gSignalConnect(da,"expose-event",expose_event,data=list(img=img))

  w<-gtkWindow(show=F)
  w$SetSizeRequest(width,height)
  w$SetResizable(FALSE)
  w$Add(da)
  w$Show()
  return(env) ## so can pass it to gdkDraw* function.
}

AddRectangle <- function(obj,x=10,y=10,width=100,height=100,...){
  ## draw a rectangle on top of image
  ## obj is an object from ViewImage()$drawable
 dgc <- gdkGCNew(obj)
 ##gdkGCSetForeground(dgc,"red")
 ##gdkGCSetBackground(dgc,"white")
 gdkGCSetLineAttributes(dgc, line.width=3, line.style="double-dash","round","round")
 gdkDrawRectangle(obj,dgc,FALSE,x,x,width,height)
 TRUE
}

