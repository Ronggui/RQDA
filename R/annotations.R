getAnnos <- function(){
    anno <- RQDAQuery("select annotation.fid, annotation, annotation.date, source.name from annotation join source where annotation.fid==source.id and annotation != '' ")
    Encoding(anno$annotation) <- Encoding(anno$name) <- "UTF-8"
    attr(anno,"field.name") <- "annotation"
    attr(anno, "descr") <- sprintf("%i %s", nrow(anno), ngettext(nrow(anno), "Annotation", "Annotations", domain = "R-RQDA"))
    class(anno) <- c("annotations", "Info4Widget", "data.frame")
    anno
}
