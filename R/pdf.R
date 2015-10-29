importPDFHL <- function(file, type=c("Highlight"), engine="rjpod"){
    if (missing(file)) {
        file <- gfile(text="select a pdf file", type="open", filter=list("PDF"=list(patterns=c("*.PDF"))))
    }
    fileName <- basename(file)
    fileName <- enc(fileName)
    maxid <- RQDAQuery("select max(id) from source")[[1]]
    nextid <- ifelse(is.na(maxid),0+1, maxid+1)
    write <- FALSE
    if (nextid==1) {
        write <- TRUE
    } else {
        if (nrow(RQDAQuery(sprintf("select name from source where name='%s'", fileName)))==0) {
            write <- TRUE
        } else {
            gmessage("A file withe the same name exists in the database!")
        }
    }
    if (write ) {
        if (engine == "rjpod") {
            cat("Extracting highlight ...\n")
            ans <- rjpod::pdfAnnotations(file, type = type)
            cat("Highlight extracted\nExtracting XMP ...\n")
            finfo <-  rjpod::pdfXMP(file, jabrefOnly=TRUE)
            cat("XMP extracted\n")
            finfo <- gsub("^bibtex/","", finfo)
            finfo <- paste(sort(finfo), collapse=",\n")
        }
        RQDAQuery(sprintf("insert into source (name, file, id, status,date,owner, memo )
                             values ('%s', '%s',%i, %i, '%s', '%s', '%s')",
                          fileName,enc(ans), nextid, 1, date(), .rqda$owner, enc(finfo)))
        FileNamesUpdate()
    }
}
