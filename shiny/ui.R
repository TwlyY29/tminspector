  ui <- navbarPage(THE_TITLE,id = "thepage",
     tabPanel("Explore Topics",
              fluidRow(width=12,
                       tags$div(THE_HEADER,
                                align="right",
                                style="width:100%;
          background-color:#F8F8F8;
          font-size:x-small;
          margin-top:-20px;")
              ),
              fluidRow(
                width = 12,
                column(width = 2,align="left",
                       tableOutput("currentTopic")),
                column(width = 10,
                       fluidRow(align="center",
                                plotOutput(outputId = "topicDevelopment", click = "topicDevelopmentClick")
                       ),
                       fluidRow(align="center",
                                h3(textOutput("analyzingTopicHeadline"))
                       ),
                       fluidRow(
                         column(width=6,align="center",
                                h4("Document-Topic-Relation"),
                                div(DT::dataTableOutput('documents'), 
                                    style = "font-size: 75%; width: 75%")),
                         column(width=6,align="left",
                                h4("Document Body"),
                                htmlOutput("documentContent")
                         )
                       )
                )
              )
     ),
    tabPanel("Topic Country Network",
             fluidRow(width=12,
                      tags$div(THE_HEADER,
                               align="right",
                               style="width:100%;
                        background-color:#F8F8F8;
                        font-size:x-small;
                        margin-top:-20px;")
             ),
             fluidRow(align="right", downloadButton("downloadTCNetworkEL", "Download Network as Edgelist"),
                                     downloadButton("downloadTCNetworkGML", "Download Network as GraphML File")),
             visNetworkOutput("topic_country_network"),
             fluidRow(align="center", 
                      sliderInput("minctassign","Adjust minimum Country-Topic Assignment in %", min = 0, max = 100, value = 25),
                      sliderInput("timespan","Adjust timespan for Country-Topic Assignment", min = min(GLB_ALL_YEARS), max = max(GLB_ALL_YEARS), value = c(min(GLB_ALL_YEARS), max(GLB_ALL_YEARS)), dragRange = T),
                      h4(textOutput("topic_country_topictable_headline"))),
             fluidRow(tableOutput("topic_country_topictable"))
    ),
    tabPanel("Document Topic Assignments",
              fluidRow(width=12,
                       tags$div(THE_HEADER,
                                align="right",
                                style="width:100%;
          background-color:#F8F8F8;
          font-size:x-small;
          margin-top:-20px;")
              ),
              fluidRow(
                column(width = 2,
                       h4("Configure Table"),
                       selectInput(inputId = "show_dtm_country",
                                   label = "Filter Country:",
                                   choices = c("All",GLB_ALL_COUNTRIES)),
                       selectInput(inputId = "show_dtm_year",
                                   label = "Filter Year:",
                                   choices = c("All",GLB_ALL_YEARS)),
                       br(),
                       h4("Columns in Document-Topic-Table to show:"),
                       actionButton("show_dtm_vars_all", "Select All"),
                       actionButton("show_dtm_vars_none", "Select None"),
                       checkboxGroupInput("show_dtm_vars", label = "",c(), width='100%')),
                column(width = 9,
                       fluidRow(column(width=12, align="center",
                                h4("Document-Topic-Details"),
                                div(DT::dataTableOutput('doc_topic_assignments'), 
                                    style = "font-size: 75%; width: 95%")
                       )),
                       fluidRow(column(width=7, offset=1, align="center",
                               h4("Document Body"),
                               fluidRow(align="left",
                               verbatimTextOutput("documentContentTopicTable", placeholder = T))
                                ))
                )
              )
     ),
    tabPanel("Configure",
             fluidRow(width=12,
                      tags$div(THE_HEADER,
                               align="right",
                               style="width:100%;
                        background-color:#F8F8F8;
                        font-size:x-small;
                        margin-top:-20px;")
             ),
             fluidRow(width = 12,
                      column(width=4,
                             column(width = 12,sidebarLayout(sidebarPanel(width=12, 
                                                                          h3("Number of Words"),
                                                                          p("Adjust the number of words shown for a selected topic."),
                                                                          sliderInput("nwords","",
                                                                                      min = 10, max = TOPIC_TOP_WORDS_MAX, value = 25),
                                                                          h3("Top Words Category"),
                                                                          p("How should top words be selected?"),
                                                                          radioButtons("top_words_type", "", 
                                                                                       choiceValues=c("prob", "frex", "lift", "score"),
                                                                                       choiceNames = c("Highest probability words","Highest ranking frex words","Highest scoring words by lift", "Best words by score"))
                             ),mainPanel( width = 0))),
                             column(width = 12,sidebarLayout(sidebarPanel(width=12, 
                                                                          h3("Doc Topic Assignments"),
                                                                          checkboxInput("dtm_highlight_max",
                                                                                        label="Highlight maximum assignment in table?",
                                                                                        value = F)
                             ),mainPanel( width = 0)))
                      ),
                      column(width=8,
                             column(width = 12,sidebarLayout(sidebarPanel(width=12,
                                                                          htmlOutput("downloadAndLoadText"),
                                                                          selectInput(inputId = "remoteWorkspaces",
                                                                                      label = "Available Sets:",
                                                                                      choices = listRemoteWorkspaces(FILE_PAT)),
                                                                          actionButton("load", "Load Selected Set")
                             ),mainPanel( width = 0))),
                             column(width = 12,sidebarLayout(sidebarPanel(width=12,
                                                                          h3("Switch Time Resolution"),
                                                                          p("Select monthly or yearly aggregation of topic developments."),
                                                                          radioButtons(inputId = "timeResolution",
                                                                                      label = "",
                                                                                      choices = c("yearly","monthly"))
                             ),mainPanel( width = 0)))
                             
                      )
             )
    ),
    tags$style("body{overflow: scroll;}
               #documents{height:400px; overflow-y:hidden;}
               #documentContent{height:400px; overflow-y:scroll;}
               #currentTopic{height:890px; overflow-y:scroll;}
               .table .alignRight {color: blue; text-align:right;}")
  )