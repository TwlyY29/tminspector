lemmatize_reverse <- function(lemma,warn_me_not=F){
  pos <- which(lemmata$lemma == lemma)
  if(length(pos)!=0){
    lemmata$wort[pos]
  }else{
    if(!warn_me_not){warning(paste0("no flexed forms for '",lemma,"'"))}
    lemma
  }
}

tokens_lemmatize_reverse <- function(x){
  out <- c()
  for(w_ in x){
    out <- c(out,lemmata$wort[which(w_==lemmata$lemma)],w_)
  }
  out
}

