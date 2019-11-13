getTopicsForCountry <- function(country, weight_margin){
  subset(TOPIC_COUNTRY_NETWORK, select=topc, subset=cntr==country&weights>=weight_margin)$topc
}
