getTopicWords <- function(topic, n_words, type=NULL){
  if(is.null(type)){
    TOPIC_TOP_WORDS$prob[topic,1:n_words]
  }else{
    TOPIC_TOP_WORDS[[type]][topic,1:n_words]
  }
}
