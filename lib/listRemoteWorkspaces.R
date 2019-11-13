listRemoteWorkspaces <- function(filepat = "*.RData"){
  if(str_starts(BASE_DATA_PATH,"www|http")){ 
    filenames <- getURL(BASE_DATA_PATH, dirlistonly=T)
    table <- readHTMLTable(filenames)[[1]]
    table <- na.omit(table)
    table$Name[str_detect(table$Name,filepat)]
  }else{
    tryCatch(list.files(path = sprintf("%s/",BASE_DATA_PATH), pattern=filepat),
             error=function(e){
               grep(filepat, list.files(path = sprintf("%s/",BASE_DATA_PATH)), perl=T, value=T)
             })
  }
}

