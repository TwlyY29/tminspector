if(exists("TMINSPCT_SOURCE_URL")){
  source("./lib/lib.R")
  initTmInspector(TMINSPCT_SOURCE_URL)
  source("./shiny/ui.R")
  source("./shiny/server.R")
  return(shinyApp(ui=ui, server=server))
}else{
  ui <- fluidPage(
    
    fluidRow(
      column(width=8,offset=2,align="left",
             br(),
             h2("Attention"),
             br(),
             p("You need to specify which tminspector object to load before starting the app:"),
             tags$div("library(shiny)",br(),"TMINSPCT_SOURCE_URL <- \"http://point.me/to/some.RData\"",br(),"runGitHub(\"tminspector\",\"TwlyY29\")",class="codelisting"),
             p("Alternatively:"),
             tags$div("library(shiny)",br(),"TMINSPCT_SOURCE_URL <- \"http://point.me/to/some.RData\"",br(),"runApp(\".\")",class="codelisting")
      )
    ),
    tags$style(".codelisting{background: #f4f4f4;
    border: 1px solid #ddd;
    border-left: 3px solid #FF7400;
    color: #666;
    page-break-inside: avoid;
    font-family: monospace;
    font-size: 15px;
    line-height: 1.6;
    margin-bottom: 1.6em;
    max-width: 100%;
    overflow: auto;
    padding: 1em 1.5em;
    display: block;
    word-wrap: break-word;}")
  )
  server <- function(input,output,session){}
  return(shinyApp(ui=ui,server=server))
}