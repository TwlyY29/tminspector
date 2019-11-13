## libraries
library(shiny)
library(ggplot2)
library(plotly)
library(stm)

library(DT)
library(RCurl)
library(XML)
library(igraph)
library(visNetwork)
library(stringr)
library(text2vec)
library(utils)
library(servr)

library(tidyverse)

# init some global variables
SHINY_TOPIC <<- 1
SHINY_CLICK_TD <<- NULL
SHINY_INTRCT_COMBO_NO_UPDATE <<- F
FILE_PAT <<- "^db_(afg|cmplt)_\\d+(?!_model_raw)\\.RData"
THE_TITLE <<- ""
THE_HEADER <<- tags$div(" ")
BASE_DATA_PATH <<- NULL

## load helper functions from subdir lib
path <- getwd()
setwd(sprintf("%s/lib",path))
source("getDocContent.R")
source("listRemoteWorkspaces.R")
source("downloadAndLoad.R")
source("getDocumentTopicRelations.R")
source("getTopicWords.R")
source("getTopicsForCountry.R")
source("topicCountryNetwork.R")
source("lemmatize_reverse.R")
setwd(path)
rm(path)


isActiveTopicDevelopmentPlotTab <- function(tab){
  return(tab=="Explore Topics")
}

initTmInspector <- function(SOURCE_URL){
  parts <- str_split(SOURCE_URL,"/")[[1]]
  file <- tail(parts,n=1)
  BASE_DATA_PATH <<- str_remove(SOURCE_URL,file)
  print(paste0("loading '",file,"' from '",BASE_DATA_PATH,"'"))
  # load data
  downloadAndLoad(file)
}