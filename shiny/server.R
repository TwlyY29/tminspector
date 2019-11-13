  server <- function(input, output, session) {
    dataset <- reactive({
      input$load
      if(input$timeResolution == "monthly"){
        dat <- as.table(t(TOPIC_DEVELOPMENT_MONTHLY))
        rownames(dat) <- as.character(GLB_ALL_MONTHS)
      }else if(input$timeResolution == "yearly"){
        dat <- as.table(t(TOPIC_DEVELOPMENT))
        rownames(dat) <- as.character(GLB_ALL_YEARS)
      }
      colnames(dat) <- paste("T",seq(1,ncol(dat)),"   ")
      dat <- as.data.frame(dat)
      names(dat) <- c("Year", "Topic", "Prevalence")
      dat
    })
    dtrelation <- reactive({
      t <- selectedTopic()
      y <- selectedYear()
      if(input$timeResolution == "monthly"){
        getDocumentTopicRelations(t,y,is_date = T)
      }else if(input$timeResolution == "yearly"){
        getDocumentTopicRelations(t,y)
      }
    })
    
    topic_dlpmt_click_saved <- reactiveValues(theclick = NULL)
    observeEvent(eventExpr = input$topicDevelopmentClick, handlerExpr = { topic_dlpmt_click_saved$theclick <- input$topicDevelopmentClick })
    
    
    
    selectedYearIndex <- reactive({
      round(topic_dlpmt_click_saved$theclick$x)
    })
    selectedYear <- reactive({
      input$load
      req(input$topicDevelopmentClick)
      if(input$timeResolution == "monthly"){
        GLB_ALL_MONTHS[round(topic_dlpmt_click_saved$theclick$x)]
      }else if(input$timeResolution == "yearly"){
        GLB_ALL_YEARS[round(topic_dlpmt_click_saved$theclick$x)]
      }
    })
    selectedTopic <- function(){
      input$load
      input$interactionTopic
      if(isActiveTopicDevelopmentPlotTab(input$thepage) & isTruthy(input$topicDevelopmentClick)){
        req(input$topicDevelopmentClick)
        newx <- topic_dlpmt_click_saved$theclick$x
        newy <- topic_dlpmt_click_saved$theclick$y
        if(!is.null(SHINY_CLICK_TD) & !is.null(SHINY_TOPIC)){
          if(SHINY_CLICK_TD$x == newx & SHINY_CLICK_TD$y == newy){
            updateSelectInput(session, "interactionTopic", selected = SHINY_TOPIC)
            return(SHINY_TOPIC)
          }
        }
        
        SHINY_CLICK_TD <<- topic_dlpmt_click_saved$theclick
        tmp <- dataset()
        y <- selectedYearIndex()
        if(input$timeResolution == "monthly"){
          tmp <- tmp[tmp$Year == as.character(GLB_ALL_MONTHS[y]),]
        }else if(input$timeResolution == "yearly"){
          tmp <- tmp[tmp$Year == as.character(GLB_ALL_YEARS[y]),]
        }
        for(i in rev(seq(length(tmp$Prevalence)))){
          j <- if(i < length(tmp$Prevalence)) i+1 else i
          if(i == j & i == length(tmp$Prevalence)){
            if (newy <= sum(tmp$Prevalence[i:length(tmp$Prevalence)])){
              updateSelectInput(session, "interactionTopic", selected = i)
              SHINY_TOPIC <<- i
              return(i)
            }
          }else{
            if (newy <= sum(tmp$Prevalence[i:length(tmp$Prevalence)]) & newy > sum(tmp$Prevalence[j:length(tmp$Prevalence)])){
              updateSelectInput(session, "interactionTopic", selected = i)
              SHINY_TOPIC <<- i
              return(i)
            }
          }
        }
      }else{
        SHINY_TOPIC
      }
    }
    top_words <- reactive({
      t <- selectedTopic()
      getTopicWords(t, input$nwords, input$top_words_type)
    })
    lengthMenu <- function(mby, what){
      ndocs <- if(is.data.frame(what)) nrow(what) else length(what)
      seq(from=min(mby,ndocs),to=ndocs,by=mby)
    }
    output$analyzingTopicHeadline <- renderText({
      if(isTruthy(topic_dlpmt_click_saved$theclick)){
        y <- selectedYear()
        if(input$timeResolution == "monthly"){
          y <- format(as.Date(paste0(y,"-01")), "%b %Y")
        }else if(input$timeResolution == "yearly"){
          y <- as.character(y)
        }
        t <- selectedTopic()
        nd <- sum(dtrelation()$dtd > 0.0)
        sprintf("Assignments for %d documents in %s to topic %d",nd,y,t)
      }else{
        sprintf("Please click the plot to select a topic and year")
      }
    })
    output$topicDevelopment <- renderPlot({
      data <- dataset()
      fac <- .13
      if(input$timeResolution == "monthly"){
        click_areas <- c(seq(GLB_ALL_MONTHS)+fac, seq(GLB_ALL_MONTHS)-fac)
      }else if(input$timeResolution == "yearly"){
        click_areas <- c(seq(GLB_ALL_YEARS)+fac, seq(GLB_ALL_YEARS)-fac)
      }
      ggplot(data, aes(x=Year,y=Prevalence,group=Topic,fill=Topic)) + 
        geom_area() + 
        scale_fill_manual(values = GLB_TOPIC_COLS) + 
        geom_vline(xintercept=click_areas, color="lightgrey", linetype="dotted") +
        ggtitle(paste("Development of Topics for k=",N_TOPICS," Topics", sep=""))
    })
    output$documents <- DT::renderDataTable(dtrelation(),
                        server = TRUE, selection = 'single',
                        options = list(
                          columnDefs = list(list(className = 'dt-right', targets = c(1,2))),
                          pageLength = 10,
                          lengthMenu = lengthMenu(10, dtrelation()),
                          order = list(list(1, 'desc')
                        ),
                        dom = 'pt'
    ))
    output$documentContent <- renderUI({
      req(input$documents_rows_selected)
      basename <- rownames(dtrelation())[input$documents_rows_selected]
      # t <- selectedTopic()
      words <- tokens_lemmatize_reverse(top_words())
      HTML(getDocContent(basename, highlight=words))
    })
    output$currentTopic <- renderTable({
      t <- selectedTopic()
      if(!is.null(t)){
        dat <- data.frame(t=top_words())
        names(dat) <- paste("Top",input$nwords,"words for topic",t)
        dat
      }
    })
    
    ### DTM Table starting here
    
    document_topic_table <- reactive({
      DOC_TOPIC_TABLE
    })
    dtm_vars <- reactive({
      n <- colnames(document_topic_table())
      n
    })
    dtm_rows <- reactive({
      rows <- T
      if(input$show_dtm_country != "All"){
        rows <- CORPUS$meta$country == input$show_dtm_country
      }
      if(input$show_dtm_year != "All"){
        rows <- rows & CORPUS$meta$year == as.numeric(input$show_dtm_year)
      }
      rows
    })
    observe({
      updateCheckboxGroupInput(session, "show_dtm_vars", 
                               choices=dtm_vars(), 
                               selected=dtm_vars())
    })
    observeEvent(input$show_dtm_vars_all, {
      updateCheckboxGroupInput(session, "show_dtm_vars", 
                               choices=dtm_vars(), 
                               selected=dtm_vars())
    })
    observeEvent(input$show_dtm_vars_none, {
      updateCheckboxGroupInput(session, "show_dtm_vars", 
                               choices=dtm_vars(), 
                               selected=c("Country"))
    })
    output$documentContentTopicTable <- renderText({
      req(input$doc_topic_assignments_rows_selected)
      basename <- rownames(document_topic_table()[dtm_rows(),])[input$doc_topic_assignments_rows_selected]
      getDocContent(basename)
    })
    output$doc_topic_assignments <- renderDataTable({
      rows <- dtm_rows()
      tab <- document_topic_table()
      cols <- which(input$show_dtm_vars != "Country")
      if(input$dtm_highlight_max & length(cols)>1){
        the_max <- DOC_TOPIC_TABLE_MAXASSIGN[rows]
        DT::datatable(tab[rows, input$show_dtm_vars, drop = FALSE],
                      selection = 'single', options=list( pageLength=15))%>% 
        formatStyle(input$show_dtm_vars, backgroundColor = styleEqual(the_max, rep("#FF7400",length(the_max))))
      }else{
        DT::datatable(tab[rows,input$show_dtm_vars, drop = FALSE],
                      selection = 'single', options=list( pageLength=15))
      }
    })
    
    ### topic country network starting here
    tc_network_wgt_margin <- reactive({
      weight_margin <- max(TOPIC_COUNTRY_NETWORK$weights) * (input$minctassign / 100)
    })
    
    tc_network <- reactive({
      input$load
      tc_network_wgt_margin()
      t <- input$timespan
      
      TOPIC_COUNTRY_NETWORK <<- topicCountryNetwork(min(t), max(t))
      d <- TOPIC_COUNTRY_NETWORK[TOPIC_COUNTRY_NETWORK$weights >= tc_network_wgt_margin(), ]
      if(nrow(d)>0){
        g <- graph_from_data_frame(d, directed = FALSE)
        rm(d)
        g <- toVisNetworkData(g)
        colnames(g$edges) <- c("from","to","value")
        
        evals_topic <- aggregate(value~to,data = g$edges,FUN = sum)
        evals_topic$value <- evals_topic$value / max(evals_topic$value) * 15
        evals_cat <- aggregate(value~from,data = g$edges,FUN = sum)
        evals_cat$value <- evals_cat$value / max(evals_cat$value) * 20
        g$nodes <- cbind(g$nodes, 
                         group=sapply(g$nodes$id,function(x) if(x %in% 1:N_TOPICS) "topic" else "country"), 
                         value=sapply(g$nodes$id,function(x) if(x %in% 1:N_TOPICS) evals_topic$value[evals_topic$to == x] else evals_cat$value[evals_cat$from == x]),
                         shape=sapply(g$nodes$id,function(x) if(x %in% 1:N_TOPICS) "square" else "triangle"))
        g$nodes$label=sapply(g$nodes$id,function(x) if(x %in% 1:N_TOPICS) paste("Topic ",x,sep="") else x)
        g
      }
    })
    
    output$topic_country_network <- renderVisNetwork({
      g <- tc_network()
      if(!is.null(g)){
        visNetwork(nodes=g$nodes, edges=g$edges, width="100%") %>% 
          visEdges(color = list(color = "lightgray", highlight = "gray")) %>%
          visNodes(shadow=T, shapeProperties = list(useBorderWithImage = F)) %>%
          visGroups(groupname = "topic", color = "darkblue", shape = "square", shadow = list(enabled = TRUE)) %>%
          visGroups(groupname = "country", color = "red", shape = "triangle", shadow = list(enabled = TRUE)) %>%
          visOptions(highlightNearest = list(enabled = T),  nodesIdSelection=T) %>%
          visIgraphLayout(layout = "layout_with_fr") # %>%
      }
    })
    output$topic_country_topictable_headline <- renderText({
      input$topic_country_network_selected # clicked
      input$loadCCAssignment
      input$appendCCAssignment
      std <- "Click a node in the network or select one to show corresponding topics"
      if(isTruthy(input$topic_country_network_selected)){
        topic <- as.numeric(input$topic_country_network_selected)
        if(!is.na(topic)){
          # user clicked a topic-node
          paste("Listing words for topic",topic)
        }else{
          # user clicked a country-node
          paste("Listing topics to which",input$topic_country_network_selected,"is connected")
        }
      }else{
        std
      }
    })
    output$topic_country_topictable <- renderTable({
      input$topic_country_network_selected # clicked
      n_words <- input$nwords
      type <- input$top_words_type
      
      if(isTruthy(input$topic_country_network_selected)){
        dat <- data.frame(t=character(0), words=character(0))
        topic <- as.numeric(input$topic_country_network_selected)
        if(!is.na(topic)){
          # user clicked a topic-node
          dat <- rbind(dat, data.frame(t=as.character(topic), words=paste(getTopicWords(topic, n_words, type), collapse=", ")))
        }else{
          # user clicked a country-node
          topics <- getTopicsForCountry(input$topic_country_network_selected, tc_network_wgt_margin())
          if(length(topics) > 1){
            dat <- data.frame(t=topics, words=apply(getTopicWords(topics, n_words, type),1,function(x){paste(x, collapse=", ")}))
          }
          else{
            dat <- data.frame(t=topics, words=apply(t(as.matrix(getTopicWords(topics, n_words, type))),1,function(x){paste(x, collapse=", ")}))
          }
        }
        names(dat) <- c("Topic",paste("Top",n_words,"words"))
        dat
      }
    })
    output$downloadTCNetworkEL <- downloadHandler(
      filename = "topic-country-network.edgelist",
      content = function(file) {
        write.csv(tc_network()$edges, file, row.names = FALSE)
      }
    )
    output$downloadTCNetworkEL <- downloadHandler(
      filename = "topic-country-network.edgelist",
      content = function(file) {
        write.csv(tc_network()$edges, file, row.names = FALSE)
      }
    )
    output$downloadTCNetworkGML <- downloadHandler(
      filename = "topic-country-network.graphml",
      content = function(file) {
        netw <- tc_network()
        print(head(netw))
        write_graph(graph_from_data_frame(netw$edges,directed = F,vertices = netw$nodes), file, format="graphml")
      }
    )
    
    ### configuration stuff starting here
    output$downloadAndLoadText <- renderText({
      title <- "<h3>Available Analyses</h3>"
      paste(title,"<p>Select one of the predefined settings below and click load<p><br/><p>Be careful: Loading a setting means discarding anything you have done so far.</p>")
    })
    observeEvent(input$load, {
      f <- input$remoteWorkspaces
      id <- showNotification(paste("Downloading and loading configuration ",f,". It is ready as soon as this notification disappears.",sep=""), duration=NULL, type="warning")
      downloadAndLoad(f)
      removeNotification(id)
      SHINY_TOPIC <<- 1
    })
  }