getDocumentTopicRelations <- function(topic, date, is_date=F, country=NULL){
  if(is_date){
    docs <- grep(date,CORPUS$meta$date)
  }else{ # is year only
    docs <- which(CORPUS$meta$year == date)
  }
  if(!is.null(country)){
    selected_m <- which(CORPUS$meta$country == country)
    docs <- docs[docs %in% selected_m]
  }
  data.frame(row.names = CORPUS$meta$doc_id[docs], "dtd" = THETA[docs,topic], "country" = CORPUS$meta$country[docs])
}
