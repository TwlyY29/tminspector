getDocContent <- function(filename, col="red", highlight=NULL, highlight_stem=F){
  txt <- CORPUS$meta$rawtext[which(CORPUS$meta$doc_id == filename)]
  if(!is.null(highlight)){
    if(highlight_stem){
      find <- gsub(pattern = "_",replacement = "\\\\w*\\\\b.*", highlight,ignore.case = T)
      find <- c(find, highlight[grep("_",highlight,invert = T)])
      find <- paste0("\\w*",find, "\\w*\\b",collapse="|")
      find <- paste0("(",find,")",collapse="" )
    }else{
      find <- highlight[which(sapply(highlight,nchar)>=3)]
      find <- gsub(pattern = "_",replacement = "\\\\w*\\\\b.*", find,ignore.case = T)
      find <- paste0("\\b",find, "\\b",collapse="|")
      find <- paste0("(",find,")",collapse="" )
    }
    txt <- gsub(txt, pattern=find, replacement = paste0("<span style='color:",col,";font-weight:bold;'>\\1</span>"), ignore.case = T, perl = T)
    txt <- gsub(txt, pattern="\\n", replacement="<br\\>")
  }
  txt
}
