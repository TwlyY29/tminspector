downloadAndLoad <- function(file, outpath="."){
  if(str_starts(BASE_DATA_PATH,"www|http")){
    if(!file.exists(file)){
      url <- sprintf("%s/%s",BASE_DATA_PATH,file)
      download.file(url, file)
    }
    p<-sprintf("%s/%s",outpath, file)
  }else{
    p<-sprintf("%s/%s",BASE_DATA_PATH, file)
  }
  load(p, .GlobalEnv)
}
