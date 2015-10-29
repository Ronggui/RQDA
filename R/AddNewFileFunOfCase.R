AddNewFileFunOfCase <- function ()
{
    updateCaseLink <- function(fid){
        SelectedCase <- svalue(.rqda$.CasesNamesWidget)
        SelectedCase <- enc(SelectedCase,"UTF-8")
        caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",SelectedCase))$id
        content <- RQDAQuery(sprintf("select file from source where id=%s", fid))$file
        Encoding(content) <- "UTF-8"
        selend <- nchar(content)
        Dat <- data.frame(caseid=caseid,fid=fid,selfirst=0,selend=selend,status=1,owner=.rqda$owner,date=date(),memo=NA)
        dbWriteTable(.rqda$qdacon,"caselinkage",Dat,row.names=FALSE,append=TRUE)
        UpdateFileofCaseWidget()
    }

    gw <- gwindow(title = "Add a new file to selected case", parent = getOption("widgetCoordinate"),
                  width = getOption("widgetSize")[1], height = getOption("widgetSize")[2])
    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    gw@widget@widget$SetIconFromFile(mainIcon)
    gp <- gpanedgroup(horizontal = FALSE, container=gw)

    saveFileFun <- function() {
        Ftitle <- ginput("Enter the title", icon = "info")
        if (!is.na(Ftitle)) {
            Ftitle <- enc(Ftitle, "UTF-8")
            if (nrow(dbGetQuery(.rqda$qdacon, sprintf("select name from source where name='%s'", Ftitle))) != 0) {
                Ftitle <- paste("New", Ftitle)
            }
            content <- svalue(textW)
            content <- enc(content, encoding = "UTF-8")
            maxid <- dbGetQuery(.rqda$qdacon, "select max(id) from source")[[1]]
            nextid <- ifelse(is.na(maxid), 0 + 1, maxid + 1)
            ans <- dbGetQuery(.rqda$qdacon, sprintf("insert into source (name, file, id, status,date,owner ) values ('%s', '%s',%i, %i, '%s', '%s')",
                                                    Ftitle, content, nextid, 1, date(), .rqda$owner))
            if (is.null(ans)) {
                svalue(textW) <- ""
                FileNamesUpdate()
                enabled(button$AddNewFilBC) <- FALSE
                updateCaseLink(fid=nextid)
            }
            return(TRUE)
        }
        else {
            return(FALSE)
        }
    }

    gl <- glayout(homogeneous = TRUE, container = gp)
    AddNewFilBC <- gbutton("Save", handler = function(h, ...) {
        suc <- saveFileFun()
        if (suc)
            dispose(gw)
    })
    enabled(AddNewFilBC) <- FALSE
    assign("AddNewFilBC", AddNewFilBC, envir = button)
    gl[1, 1] <- AddNewFilBC
    tmp <- gtext(container = gp)
    font <- pangoFontDescriptionFromString(.rqda$font)
    gtkWidgetModifyFont(tmp@widget@widget, font)
    assign(".AddNewFileWidgetW", tmp, envir = .rqda)
    textW <- get(".AddNewFileWidgetW", envir = .rqda)
    addHandlerKeystroke(.rqda$.AddNewFileWidgetW, handler = function(h, ...) {
        enabled(button$AddNewFilBC) <- TRUE
    })
    addhandlerunrealize(.rqda$.AddNewFileWidgetW, handler = function(h, ...) {
        rm("AddNewFilBC", envir = button)
        rm(".AddNewFileWidgetW", envir = .rqda)
        FALSE
    })
}
