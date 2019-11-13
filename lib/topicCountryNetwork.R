topicCountryNetwork <- function(t_min = min(GLB_ALL_YEARS), t_max = max(GLB_ALL_YEARS)){
  w <- which(CORPUS$meta$year %in% c(t_min:t_max))
  d <- THETA[w,]
  colnames(d) <- 1:N_TOPICS
  rownames(d) <- CORPUS$meta$doc_id[w]
  d <- as.data.frame(as.table(d))
  colnames(d) <- c("cntr","topc","weights")
  d$cntr <- CORPUS$meta$country[match(unlist(d$cntr), CORPUS$meta$doc_id)] # replace doc-names with countries
  d <- aggregate(weights ~ cntr+topc, data = d, FUN = sum) # aggregate weights for countries
}
