RQDA <- function() {
########################### GUI FOR ROOT
###########################
options("guiToolkit"='RGtk2')
## avoid menual selection when more than one toolkits have been installed
if (isTRUE(.rqda$isLaunched)) {
 message("RQDA has been launched.")
 } else
{
  ".root_rqdagui" <- gwindow(title = gettext("RQDA: Qualitative Data Analysis", domain = "R-RQDA"),parent=c(2,2),
                             width=300,height=(gdkScreenHeight()-65),
                             visible=FALSE,handler=function(h,...){
                               closeProject(assignenv=.rqda)
                             }
                             )

  mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
  .root_rqdagui@widget@widget$SetIconFromFile(mainIcon)
  ## set an icon for the main programme.
  ".nb_rqdagui" <- gnotebook(4,container=.root_rqdagui,closebuttons=FALSE)

########################### GUI FOR PROJECT
###########################
  ".proj_gui" <- ggroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Project\n", domain = "R-RQDA"))
  NewProjectButton(container=.proj_gui)
  OpenProjectButton(container=.proj_gui)
  CloseProjectButton(container=.proj_gui)
  Proj_MemoButton(label = gettext("Project Memo", domain = "R-RQDA"), container = .proj_gui)
  ## project memo button
  BackupProjectButton(container=.proj_gui)
  saveAsButt(label=gettext("Save Project As", domain = "R-RQDA"), container=.proj_gui)
  CleanProjButton(container=.proj_gui)
  CloseAllCodingsButton(container=.proj_gui)
  ##gbutton(gettext("About", domain = "R-RQDA"),container=.proj_gui, handler=function(h,...) {browseURL("http://rqda.r-forge.r-project.org/")})

  gseparator(container=.proj_gui)
  glabel(gettext("Path of current project:", domain = "R-RQDA"),container=.proj_gui)
  ".currentProj" <- glabel(gettext("No project is open.", domain = "R-RQDA"),container=.proj_gui)

  gseparator(container=.proj_gui)
  glabel(gettext("Author: <ronggui.huang@gmail.com>", domain = "R-RQDA"),container=.proj_gui)
  glabel(gettext("Help: click to join rqda-help mailing list", domain = "R-RQDA"),
         container=.proj_gui, handler=function(h,...){
             browseURL("https://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/rqda-help")
         })
  gseparator(container=.proj_gui)
  glabel(gettext("License: BSD", domain = "R-RQDA"),
         container=.proj_gui, handler=function(h,...){
           gtext(paste(readLines((system.file("License",package="RQDA")), warn=FALSE) , collapse="\n"),
                 container=gwindow(title="License"))
         })
  glabel(
         paste(gettext("Version:", domain = "R-RQDA"), packageDescription("RQDA")$Version, gettext(" Year:", domain = "R-RQDA"), substr(packageDescription("RQDA")$Date,1,4)),
         container=.proj_gui, handler=function(h,...){
             gtext(format(citation("RQDA"), "textVersion"), container=gwindow(title=gettext("Please cite this package.", domain = "R-RQDA")))
         })
  glabel(gettext("About", domain = "R-RQDA"),
         container=.proj_gui, handler=function(h,...){
             browseURL("http://rqda.r-forge.r-project.org/")
         })

########################### GUI for FILES
###########################
  ".files_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Files\n", domain = "R-RQDA"))
  ".files_button" <- ggroup(container=.files_pan,horizontal=TRUE)
  ##".fnames_rqda"<-gtable("Click Here to see the File list.",container=.files_pan,multiple=TRUE)
  ##.fnames_rqda[] <-NULL # get around of the text argument.
  ".fnames_rqda" <- gtable(character(0),container=.files_pan,multiple=TRUE)
  names(.fnames_rqda) <- gettext("Files", domain = "R-RQDA")
  ImportFileButton(gettext("Import", domain = "R-RQDA"),container=.files_button)
  NewFileButton(gettext("New", domain = "R-RQDA"),container=.files_button)
  DeleteFileButton(gettext("Delete", domain = "R-RQDA"),container=.files_button)
  ViewFileButton(gettext("Open", domain = "R-RQDA"),container=.files_button)
  File_MemoButton(label=gettext("Memo", domain = "R-RQDA"), container=.files_button,FileWidget=.fnames_rqda)
  ## memo button of selected file. The code of File_Memo buttion has been moved into memo.R
  File_RenameButton(label=gettext("Rename", domain = "R-RQDA"), container=.files_button,FileWidget=.fnames_rqda)
  ## rename a selected file.
  FileAttribute_Button(label=gettext("Attribute", domain = "R-RQDA"), container=.files_button,FileWidget=.fnames_rqda)

########################### GUI for CODES
###########################
  ".codes_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Codes\n", domain = "R-RQDA"))
  ".codes_button" <- glayout(container=.codes_pan)
  ".codes_rqda" <- gtable(character(0),container=.codes_pan)
  names(.codes_rqda) <- gettext("Codes List", domain = "R-RQDA")
  .codes_button[1,1]<- AddCodeButton()
  .codes_button[1,2]<- DeleteCodeButton()
  .codes_button[1,3] <- FreeCode_RenameButton(label=gettext("Rename", domain = "R-RQDA"),CodeNamesWidget=.codes_rqda)
  .codes_button[1,4] <- CodeMemoButton(label=gettext("Memo", domain = "R-RQDA"))
  .codes_button[2,1] <-  AnnotationButton(gettext("Anno", domain = "R-RQDA"))
  ## .codes_button[2,1]<- CodingMemoButton(label="C2Memo")
  .codes_button[2,2]<- RetrievalButton(gettext("Coding", domain = "R-RQDA"))
  .codes_button[2,3]<- Unmark_Button(name="UnMarB1")
  .codes_button[2,4]<- Mark_Button(name="MarCodB1")


######################### GUI  for code categories
#########################
  ".codecat_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Code\nCategories", domain = "R-RQDA"))
  ".codecat_buttons" <- glayout(container=.codecat_pan)
  ".Ccat_PW" <- ggroup(container=.codecat_pan,horizontal = FALSE)## parent Widget of C-cat
  ".CodeCatWidget" <- gtable(character(0),container=.Ccat_PW,expand=TRUE,multiple=TRUE)
  names(.CodeCatWidget)<- gettext("Code Category", domain = "R-RQDA")
  ".CodeofCat" <- gtable(gettext("Please click Update", domain = "R-RQDA"),container=.Ccat_PW,expand=TRUE,multiple=TRUE)
  .CodeofCat[] <- NULL;names(.CodeofCat)<-gettext("Codes of This Category", domain = "R-RQDA")
  .codecat_buttons[1,1] <- AddCodeCatButton(gettext("Add", domain = "R-RQDA"))
  .codecat_buttons[1,2] <- DeleteCodeCatButton(gettext("Delete", domain = "R-RQDA")) ## should take care of treecode table
  .codecat_buttons[1,3] <- CodeCat_RenameButton(gettext("Rename", domain = "R-RQDA"))
  .codecat_buttons[2,1] <- CodeCatAddToButton(gettext("Add To", domain = "R-RQDA"))
  .codecat_buttons[2,2] <- CodeCatDropFromButton(gettext("Drop From", domain = "R-RQDA"))
  .codecat_buttons[1,4] <- CodeCatMemoButton()
  .codecat_buttons[2,3] <- Unmark_Button(label=gettext("UnMark", domain = "R-RQDA"), codeListWidget=.rqda$.CodeofCat,name="UnMarB2")
  .codecat_buttons[2,4] <- Mark_Button(label=gettext("Mark", domain = "R-RQDA"), codeListWidget=".CodeofCat",name="MarCodB2")


######################### GUI  for cases
#########################
  ".case_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Cases\n", domain = "R-RQDA"))
  ".case_buttons" <- glayout(container=.case_pan)
  ".case_PW" <- ggroup(container=.case_pan,horizontal = FALSE)
  ".CasesNamesWidget" <- gtable(character(0),container=.case_PW,expand=TRUE,multiple=TRUE)
  names(.CasesNamesWidget) <- gettext("Cases", domain = "R-RQDA")
  ".FileofCase" <- gtable(character(0),container=.case_PW,expand=TRUE,multiple=TRUE)
  names(.FileofCase)<-gettext("Files of This Case", domain = "R-RQDA")
  .case_buttons[1,1] <- AddCaseButton()
  .case_buttons[1,2] <- DeleteCaseButton()
  .case_buttons[1,3] <- Case_RenameButton()
  ##.case_buttons[1,4] <- CaseMemoButton()
  .case_buttons[1,4] <- CaseUnMark_Button(gettext("Unlink", domain = "R-RQDA"))
  .case_buttons[1,5] <- CaseMark_Button(gettext(" Link ", domain = "R-RQDA"))
  .case_buttons[2,1] <- CaseAttribute_Button(gettext("Attribute", domain = "R-RQDA"))
  .case_buttons[2,2] <- prof_mat_Button(gettext("Profile", domain = "R-RQDA"))
  ##.case_buttons[2,3] <- AddWebSearchButton("WebSearch") # use popup menu instead

########################### GUI for Attributes
###########################
  ".attr_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Attributes\n", domain = "R-RQDA"))
  ".attr_buttons" <- glayout(container=.attr_pan)
  ".attr_PW" <- ggroup(container=.attr_pan,horizontal = FALSE)
  ".AttrNamesWidget" <- gtable(character(0),container=.attr_PW,expand=TRUE,multiple=TRUE)
  names(.AttrNamesWidget) <- gettext("Attributes", domain = "R-RQDA")
  .attr_buttons[1,1] <- AddAttrButton()
  .attr_buttons[1,2] <- DeleteAttrButton()
  .attr_buttons[1,3] <- RenameAttrButton()
  .attr_buttons[1,4] <- AttrMemoButton()
  .attr_buttons[1,5] <- SetAttrClsButton()


######################### GUI  for File categories
#########################
  ".filecat_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("File\nCategories", domain = "R-RQDA"))
  ".filecat_buttons" <- glayout(container=.filecat_pan)
  ".Fcat_PW" <- ggroup(container=.filecat_pan,horizontal = FALSE)## parent Widget of F-cat
  ".FileCatWidget" <- gtable(character(0),container=.Fcat_PW,expand=TRUE,multiple=TRUE)
  names(.FileCatWidget) <- gettext("File Category", domain = "R-RQDA")
  ".FileofCat" <- gtable(character(0),container=.Fcat_PW,expand=TRUE,multiple=TRUE)
  names(.FileofCat) <- gettext("Files of This Category", domain = "R-RQDA")
  .filecat_buttons[1,1] <- AddFileCatButton(gettext("Add", domain = "R-RQDA"))
  .filecat_buttons[1,2] <- DeleteFileCatButton(gettext("Delete", domain = "R-RQDA")) ## should take care of treecode table
  .filecat_buttons[1,3] <- FileCat_RenameButton(gettext("Rename", domain = "R-RQDA"))
  .filecat_buttons[2,3] <- FileCatMemoButton()
  .filecat_buttons[2,1] <- FileCatAddToButton(gettext("Add To", domain = "R-RQDA"))
  .filecat_buttons[2,2] <- FileCatDropFromButton(gettext("Drop From", domain = "R-RQDA"))

########################### GUI for Search
###########################
##   ".fsearch_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label="F-Search")
##  ".fsearch_rqda" <- glabel(gettext("Use SearchFiles function to search files.\nSee ?SearchFiles for more.", domain = "R-RQDA"),container=.fsearch_pan)
##  ".fsearch_rqda" <- gtable(gettext("Click Here to see the File list.", domain = "R-RQDA"),container=.fsearch_pan,multiple=TRUE,expand=TRUE)
##  .fsearch_rqda[] <-NULL # get around of the text argument.
##  names(.fsearch_rqda) <- "Files Search"

########################### GUI for Journal
###########################
  ".journal_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Journals\n", domain = "R-RQDA"))
  ".journal_buttons" <- glayout(container=.journal_pan)
  ".journal_PW" <- ggroup(container=.journal_pan,horizontal = FALSE)
  ".JournalNamesWidget" <- gtable(character(0),container=.journal_PW,expand=TRUE,multiple=FALSE)
  names(.JournalNamesWidget) <- gettext("Journals", domain = "R-RQDA")
  .journal_buttons[1,1] <- AddJournalButton()
  .journal_buttons[1,2] <- DeleteJournalButton()
  .journal_buttons[1,3] <-  OpenJournalButton()
  .journal_buttons[1,4] <-  RenameJournalButton()

######################### GUI  for settings
#########################
  ".settings_gui" <- ggroup(container=.nb_rqdagui,horizontal=FALSE,label=gettext("Settings\n", domain = "R-RQDA"))
  addSettingGUI(container=.settings_gui)

######################### Put them together
  if (is.null(getOption("widgetCoordinate"))) {
      options(widgetCoordinate=c(size(.root_rqdagui)[1]+12,2))
  }
#########################
  visible(.root_rqdagui) <- TRUE
  svalue(.nb_rqdagui) <- 1 ## make sure the project tab gain the focus.

##########################
  ## add documentation here
  assign(".root_rqdagui",.root_rqdagui,envir=.rqda)
  assign(".files_button",.files_button,envir=.rqda)
  assign(".codes_rqda",.codes_rqda,envir=.rqda)
  assign(".fnames_rqda",.fnames_rqda,envir=.rqda)
  ##assign(".fsearch_rqda",.fsearch_rqda,envir=.rqda)
  assign(".CasesNamesWidget",.CasesNamesWidget,envir=.rqda)
  assign(".AttrNamesWidget",.AttrNamesWidget,envir=.rqda)
  assign(".JournalNamesWidget",.JournalNamesWidget,envir=.rqda)
  assign(".FileofCase",.FileofCase,envir=.rqda)
  assign(".CodeCatWidget",.CodeCatWidget,envir=.rqda)
  assign(".CodeofCat",.CodeofCat,envir=.rqda)
  assign(".FileCatWidget",.FileCatWidget,envir=.rqda)
  assign(".FileofCat",.FileofCat,envir=.rqda)
  assign(".currentProj",.currentProj,envir=.rqda)
  assign(".SettingsGui",.settings_gui,envir=.rqda)
  assign("font","Sans 11",envir=.rqda)
  assign("isLaunched",TRUE,envir=.rqda)

##########################
  gtkWidgetSetSensitive(.fnames_rqda@widget@widget,FALSE)
  enabled(.JournalNamesWidget) <- FALSE
  enabled(.rqda$.codes_rqda) <- FALSE
  enabled(.rqda$.SettingsGui) <- FALSE
  enabled(.rqda$.CodeCatWidget) <- FALSE
  enabled(.rqda$.CodeofCat) <- FALSE
  enabled(.rqda$.CasesNamesWidget) <- FALSE
  enabled(.rqda$.FileofCase) <- FALSE
  enabled(.rqda$.AttrNamesWidget) <- FALSE
  enabled(.rqda$.FileCatWidget) <- FALSE
  enabled(.rqda$.FileofCat) <- FALSE
  enabled(button$profmatB) <- FALSE

##########################
### set the positions
###  svalue(.codes_pan) <- 0.09
###  svalue(.codecat_pan)<-0.09
###  svalue(.filecat_pan)<-0.09
###  svalue(.case_pan)<-0.04
###  svalue(.attr_pan)<-0.04
###  svalue(.journal_pan)<-0.04
### The effect depends on the screen size, which makes it difficult to look elegent for all PCs.

##########################
  AddHandler()
}}
## end of function RQDA


AddHandler <- function(){
  ## add handler function for GUIs

  addHandlerUnrealize(.rqda$.root_rqdagui, handler = function(h,...) {
    ## handler for Root
    ## make sure is the project should be closed by issuing a confirm window.
    val <- gconfirm(gettext("Really EXIT?\n\nYou can use RQDA() to start this program again.", domain = "R-RQDA"), parent=h$obj)
    if(as.logical(val)) {
      assign("isLaunched",FALSE,envir=.rqda)
      return(FALSE) # destroy
    } else {
      return(TRUE) # don't destroy
    }
  }
                      )

  ## handler for .fnames_rqda (gtable holding the file names)
  addHandlerClicked(.rqda$.fnames_rqda, handler <- function(h, ...) {
    if (isTRUE(.rqda$SFP)) ShowFileProperty(focus=FALSE)
    Fid <- GetFileId(,"select")
    if (!is.null(Fid) && length(Fid)==1) {
      names(.rqda$.fnames_rqda) <- sprintf(gettext("Selected File id is %s", domain = "R-RQDA"),Fid)
      gtkWidgetSetSensitive(button$DelFilB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$VieFilB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$FilMemB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$FilRenB@widget@widget,TRUE)
      ## dynamically change the label of attribute(s)
      if ((nattr <- length(.rqda$.AttrNamesWidget[]))!=0) {
          enabled(button$FileAttrB) <- TRUE
          if (length(svalue(.rqda$.AttrNamesWidget))>1 || nattr>1) {
              svalue(button$FileAttrB) <- gettext("Attributes", domain = "R-RQDA")
          }
      }
  }
}
                    )

  add3rdmousepopupmenu(.rqda$.fnames_rqda, GetFileNamesWidgetMenu())
  ## right click to add file to a case category

  addhandlerdoubleclick(.rqda$.fnames_rqda, handler <- function(h,...) {
    ViewFileFun(FileNameWidget=.rqda$.fnames_rqda)
  }
                        )

  ## addhandlerdoubleclick(.rqda$.fsearch_rqda, handler <- function(h,...) ViewFileFun(FileNameWidget=.rqda$.fsearch_rqda))

  ## handler for .codes_rqda
  addhandlerdoubleclick(.rqda$.codes_rqda,handler=function(h,...) {
    if (is_projOpen(envir=.rqda,conName="qdacon")) {
      if (length(Fid <- GetFileId(condition=.rqda$TOR,type="coded"))>0){
       retrieval(Fid=Fid,CodeNameWidget=.rqda$.codes_rqda)
      } else {
        gmessage(gettext("No coding associated with this code.", domain = "R-RQDA"),container=TRUE)
        }
  }
    }
                        )

  add3rdmousepopupmenu(.rqda$.codes_rqda, GetCodesNamesWidgetMenu())

  addHandlerClicked(.rqda$.codes_rqda,handler <- function(h,...){
    ClickHandlerFun(.rqda$.codes_rqda,buttons=c("MarCodB1"),codingTable=.rqda$codingTable)
    if (length(svalue(.rqda$.codes_rqda))==1) {
      enabled(button$RetB) <- TRUE
      enabled(button$DelCodB) <- TRUE
      enabled(button$codememobuton) <- TRUE
      enabled(button$FreCodRenB) <- TRUE
    }
  }
                    )

  ## handler for .CodeofCat
  addHandlerClicked(.rqda$.CodeofCat,handler <- function(h,...){
    ClickHandlerFun(.rqda$.CodeofCat,buttons=c("MarCodB2","UnMarB2"),codingTable=.rqda$codingTable)
    if (length(svalue(.rqda$.CodeofCat))>0){enabled(button$CodCatADroFromB) <- TRUE }
  }
                    )

  addhandlerdoubleclick(.rqda$.CasesNamesWidget, handler=function(h,...) {
    MemoWidget(gettext("Case", domain = "R-RQDA"),.rqda$.CasesNamesWidget,"cases")
    }
                        )

  addHandlerClicked(.rqda$.CasesNamesWidget,handler <- function(h,...){
    SelectedCase <- svalue(.rqda$.CasesNamesWidget)
    if (length(SelectedCase)!=0) {
      enabled(button$DelCasB) <- TRUE
      enabled(button$CasRenB) <- TRUE
      enabled(button$profmatB) <- TRUE
      if ((nattr <- length(.rqda$.AttrNamesWidget[]))!=0) {
          enabled(button$CasAttrB) <- TRUE
          if (length(svalue(.rqda$.AttrNamesWidget))>1 || nattr>1) {
              svalue(button$CasAttrB) <- gettext("Attributes", domain = "R-RQDA")
          }
      }
      enabled(.rqda$.FileofCase) <- TRUE
      enabled(button$CasMarB) <-
        (exists(".root_edit",envir=.rqda) && isExtant(.rqda$.root_edit))
      Encoding(SelectedCase) <- "UTF-8"
      currentCid <- RQDAQuery(sprintf("select id from cases where name='%s'",
                                      enc(SelectedCase)
                                      )
                              )[,1]
      freq <- RQDAQuery(sprintf("select count(distinct fid) as freq from caselinkage where status=1 and caseid=%s", currentCid))$freq
      names(.rqda$.CasesNamesWidget) <- sprintf(gettext("Selected case id is %i__%i files", domain = "R-RQDA"), currentCid, freq)
      if (exists(".root_edit",envir=.rqda) && isExtant(.rqda$.root_edit)) {
        SelectedFile <- svalue(.rqda$.root_edit)
        Encoding(SelectedFile) <- "UTF-8"
        currentFid <- RQDAQuery(sprintf(
                                        "select id from source where name='%s'",
                                        enc(SelectedFile)
                                        )
                                )[,1]
        ## following code: Only mark the text chuck according to the current code.
        coding.idx <- RQDAQuery(sprintf("select selfirst,selend from coding where
                                        fid=%i and status=1",currentFid
                                        )
                                )
        anno.idx <- RQDAQuery(sprintf("select position from annotation where
                                       fid=%i and status=1",currentFid))$position
        allidx <- unlist(coding.idx,anno.idx)
        sel_index <-  RQDAQuery(sprintf("select selfirst, selend from caselinkage where
                                                   caseid=%i and fid=%i and status=1",
                                        currentCid, currentFid))
        Maxindex <- RQDAQuery(sprintf("select max(selend) from caselinkage where fid=%i",
                                      currentFid))[1,1]
        if (!is.null(allidx) && length(allidx)>0) Maxindex <- Maxindex + sum(allidx<=Maxindex)
        ClearMark(.rqda$.openfile_gui,min=0,max=Maxindex,clear.fore.col=FALSE,clear.back.col=TRUE)
        if (nrow(sel_index)>0){
          if (!is.null(allidx)){
            sel_index[,"selfirst"] <- sapply(sel_index[,"selfirst"],FUN=function(x) {
              x + sum(allidx <= x)
            })
            sel_index[,"selend"] <- sapply(sel_index[,"selend"],FUN=function(x) {
              x + sum(allidx <= x)
            })
          }
          HL(.rqda$.openfile_gui,index=sel_index,fore.col=NULL,back.col=.rqda$back.col)
          enabled(button$CasUnMarB) <-
            (exists(".root_edit",envir=.rqda) && isExtant(.rqda$.root_edit))
          ## end of mark text chuck
        }}
      UpdateFileofCaseWidget()
    }
  }
                    )

  addHandlerClicked(.rqda$.CodeCatWidget,handler <- function(h,...){
      if ((ncc <- length(svalue(.rqda$.CodeCatWidget))) != 0) {
          enabled(.rqda$.CodeofCat) <- TRUE
          enabled(button$DelCodCatB) <- TRUE
          enabled(button$CodCatMemB) <- TRUE
          enabled(button$CodCatRenB) <- TRUE
          enabled(button$CodCatAddToB) <- TRUE
          enabled(button$MarCodB2) <- FALSE
          enabled(button$UnMarB2) <- FALSE
          catid <- RQDAQuery(sprintf("select catid from codecat where name = '%s'",
                                 enc(svalue(.rqda$.CodeCatWidget))
                                     )
                             )$catid
          if (!is.null(catid) && length(catid)==1) {
              names(.rqda$.CodeCatWidget) <- sprintf(gettext("Selected category id is %s", domain = "R-RQDA"),catid)
          }}
      UpdateCodeofCatWidget(con=.rqda$qdacon,Widget=.rqda$.CodeofCat)
      ## if (ncc>1) {
      ##     psccItem <- CodeCatWidgetMenu$"Plot Selected Code Category"
      ##     svalue(psccItem) <- "Plot Selected Code Categories"
      ## }
  })

  addhandlerdoubleclick(.rqda$.AttrNamesWidget, handler=function(h,...) {
    MemoWidget(gettext("Attributes", domain = "R-RQDA"),.rqda$.AttrNamesWidget,"attributes")
  }
                        )

  addHandlerClicked(.rqda$.AttrNamesWidget, handler=function(h,...) {
    if (length(svalue(.rqda$.AttrNamesWidget))!=0){
      enabled(button$DelAttB) <- TRUE
      enabled(button$RenAttB) <- TRUE
      enabled(button$AttMemB) <- TRUE
      enabled(button$SetAttClsB) <- TRUE
      if (length(svalue(.rqda$.AttrNamesWidget))>1) {
          svalue(button$CasAttrB) <- svalue(button$FileAttrB) <- gettext("Attributes", domain = "R-RQDA")
      } else {
          svalue(button$CasAttrB) <- svalue(button$FileAttrB) <- gettext("Attribute", domain = "R-RQDA")
      }
  }
}
                    )

  addhandlerdoubleclick(.rqda$.CodeCatWidget, handler=function(h,...) {
    MemoWidget(gettext("Code Category", domain = "R-RQDA"),.rqda$.CodeCatWidget,"codecat")
  }
                        )

  add3rdmousepopupmenu(.rqda$.CodeCatWidget, GetCodeCatWidgetMenu())

  addhandlerdoubleclick(.rqda$.CodeofCat,handler=function(h,...) {
    retrieval(Fid=GetFileId(condition=.rqda$TOR,type="coded"),CodeNameWidget=.rqda$.CodeofCat)
  }
                        )

  add3rdmousepopupmenu(.rqda$.CodeofCat, GetCodeofCatWidgetMenu())

  addHandlerClicked(.rqda$.FileCatWidget,handler <- function(h,...){
    if (length(svalue(.rqda$.FileCatWidget))>0){
      UpdateFileofCatWidget2(con=.rqda$qdacon,Widget=.rqda$.FileofCat)
      enabled(button$DelFilCatB) <- TRUE
      enabled(button$FilCatRenB) <- TRUE
      enabled(button$FilCatMemB) <- TRUE
      enabled(button$FilCatAddToB) <- TRUE
      enabled(.rqda$.FileofCat) <- TRUE
    }})

  addhandlerdoubleclick(.rqda$.FileCatWidget, handler=function(h,...) {
    MemoWidget(gettext("File Category", domain = "R-RQDA"),.rqda$.FileCatWidget,"filecat")
  }
                        )

  add3rdmousepopupmenu(.rqda$.FileCatWidget, GetFileCatWidgetMenu())

  addhandlerdoubleclick(.rqda$.FileofCat, handler <- function(h,...) {
    ViewFileFun(FileNameWidget=.rqda$.FileofCat)
    }
                        )

  addHandlerClicked(.rqda$.FileofCat, handler <- function(h, ...) {
    if (length(svalue(.rqda$.FileofCat))>0){
      enabled(button$FilCatDroFromB) <- TRUE
      names(.rqda$.FileofCat) <- sprintf(gettext("Selected file id is %s", domain = "R-RQDA"),GetFileId("filecat","selected"))
      if (isTRUE(.rqda$SFP)) {
        ShowFileProperty(Fid = GetFileId("file", "selected"),focus=FALSE)
      }
    }
  }
                    )

  add3rdmousepopupmenu(.rqda$.FileofCat, GetFileofCatWidgetMenu())

  add3rdmousepopupmenu(.rqda$.CasesNamesWidget, GetCaseNamesWidgetMenu())
  ## popup menu by right-click on CaseNamesWidget

  add3rdmousepopupmenu(.rqda$.FileofCase, GetFileofCaseWidgetMenu())

  addhandlerdoubleclick(.rqda$.FileofCase, handler <- function(h,...) {
    ViewFileFun(FileNameWidget=.rqda$.FileofCase)
    HL_Case()
    enabled(button$CasUnMarB) <- TRUE
    enabled(button$CasMarB) <- TRUE
  }
                        )

  addHandlerClicked(.rqda$.FileofCase, handler <- function(h, ...) {
    if (length(svalue(.rqda$.FileofCase))>0) {
      names(.rqda$.FileofCase) <- sprintf(gettext("Selected File id is %s", domain = "R-RQDA"),GetFileId("case","selected"))
     }
    if (isTRUE(.rqda$SFP)) ShowFileProperty(Fid = GetFileId("case", "selected"),focus=FALSE)
  }
                    )

  addhandlerdoubleclick(.rqda$.JournalNamesWidget, handler <- function(h,...) {
    ViewJournalWidget()
  }
                        )

  addHandlerClicked(.rqda$.JournalNamesWidget, handler <- function(h,...) {
    if (length(svalue(.rqda$.JournalNamesWidget))!=0){
      enabled(button$DelJouB) <- TRUE
      enabled(button$RenJouB) <- TRUE
      enabled(button$OpeJouB) <- TRUE
    }
  }
                    )

}## end of AddHandler()
