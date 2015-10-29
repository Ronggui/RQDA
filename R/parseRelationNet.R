 parseRelationNet <- function(mapStr, plot=FALSE){
     mapStr <- strsplit(mapStr, "\n")[[1]]
     mapStr <- gsub("(^[[:space:]]{1,})|([[:space:]]{1,}$)", "", mapStr) ## strip white space
     mapStr <- mapStr[!grepl("^#", mapStr)] ## strip comment lines
     mapStr <- mapStr[map!=""] # throw the empty lines
     nrelations <- sapply(gregexpr("->",mapStr), length)
     if (!all(nrelations==1))  stop("Each line represents one relation only")
     ix1 <- regexpr("\\[",mapStr)
     ix2 <- regexpr("\\]",mapStr)
     relation <- substr(mapStr,ix1,ix2)
     mapStr <- gsub("[[:space:]]{0,}\\[(.)+\\]$", "", mapStr)
     mapL <- strsplit(mapStr, "->")
     mapDF <- as.data.frame(do.call(rbind, mapL))
     mapDF$relation <- relation
     map <- igraph::graph.data.frame(mapDF)
     if (plot) {
         igraph::tkplot(map,vertex.label=igraph::get.vertex.attribute(map,"name"), vertex.size=10,
                         edge.label=igraph::get.edge.attribute(map,"relation"), edge.label.cex=0.8)
     }
     map
 }

#map <- "# expression, performance and collaboration  -> participation [is cause of]\ninformation -> online/offline discussion, coversation, interaction\nonline/offline discussion, coversation, interaction -> participation\ninformation -> political knowledge and understanding\npolitical knowledge and understanding -> participation\nDisposition like political interest -> participation\nPolitical and interpersonal trust -> political discussion\nSocial networks -> informational trust"
#map <- parseRelationNet(map)
# ?igraph.plotting
