## server.R ##

library(tidyverse)
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)

shinyServer(
  # Allow CSV files up to.... 100 MB?
  # max_file_size_mb <- 100,
  # options(shiny.maxRequestSize=max_file_size_mb*1024^2),

  function(input, output, session){

    data_internal <- reactiveValues(
      raw = NULL,
      cols = NULL,
      # filter_present = FALSE,
      filtered = NULL
    )


    # DATA TAB
    # if no data are available but input$sample_or_real == 'sample', show into text
    output$start_text <- renderPrint({
      if(is.null(data_internal$raw) & input$sample_or_real == 'user'){
        cat("<h2>About Systematic Maps</h2><br>
           Systematic Maps are overviews of the quantity and quality of evidence in relation to a broad (open) question of policy or management relevance. The process and rigour of the mapping exercise is the same as for systematic review except that no evidence synthesis is attempted to seek an answer to the question. A critical appraisal of the quality of the evidence is strongly encouraged but may be limited to a subset or sample of papers when the quantity of articles is very large (and even be absent in exceptional circumstances). Authors should note that all systematic maps published in Environmental Evidence will have been conducted according to the CEE process. Please contact the Editors at an early stage of planning your review. More guidance can be found <a href='http://www.environmentalevidence.org' target='_blank' rel='noopener'>here</a>.<br><br>
           For systematic maps to be relevant to policy and practice they need to be as up-to-date as possible. Consequently, at the time of acceptance for publication, the search must be less than two years old. We therefore recommend that systematic maps should be submitted no later than 18 months after the search was conducted."
        )
      }else{
        cat("<h2>Attributes of uploaded data:</h2>")
      }
    })

    # if data are supplied, add them to data_internal
    observeEvent(input$sysmapdata_upload, {
      data_internal$raw <- read.csv(
        file = input$sysmapdata_upload$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        fileEncoding = input$upload_encoding)
      data_internal$cols <- colnames(data_internal$raw)
    })

    # if user switches back to internal data, supply info on that instead
    observeEvent(input$sample_or_real, {
      if(input$sample_or_real == "sample"){
        data_internal$raw <- eviatlas::pilotdata
        data_internal$cols <- colnames(eviatlas::pilotdata)
      }else{
        data_internal$raw <- NULL
        data_internal$cols <- NULL
      }
    })

    # give an outline of what that dataset contains
    output$data_summary <- renderPrint({
      if(!is.null(data_internal$raw)){
        cat(paste0(
          "Dataset containing ",
          nrow(data_internal$raw),
          " rows and ",
          ncol(data_internal$raw),
          " columns. Column names as follows:<br>",
          paste(data_internal$cols, collapse = "<br>")
        ))
      }
    })

    # FILTER TAB
    output$filter_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        checkboxGroupInput(
          "selected_variable",
          label = "Select Columns to Display:",
          choices = c(data_internal$cols),
          selected = c(data_internal$cols),
          inline=T
        )
      }
    })

    # select levels of a variable to display
    # output$value_selector <- renderUI({
    #   if(any(names(input) == "selected_variable")){
    #     if(input$selected_variable != "none"){
    #       checkboxGroupInput(
    #         "selected_level",
    #         label = "Show level:",
    #         # choices = c("something", "nothing"),
    #         choices = sort(unique(data_internal$raw[[input$selected_variable]])),
    #         inline = TRUE
    #       )
    #     }
    #   }
    # })
    # # works for numeric variables, but not characters (!?)


    output$go_button <- renderUI({
      if(any(names(input) == "selected_variable")){
        if(input$selected_variable != "none"){
          actionButton("go_subset", "Apply Subset")
        }
      }
    })

    observeEvent(input$go_subset, {
      if(any(names(input) == "selected_variable")){
        if(input$selected_variable != "none"){
          # rows <- which(data_internal$raw[[input$selected_variable]] %in% input$selected_variable)
          # data_internal$filtered <- data_internal$raw[rows, ]
          data_internal$filtered <- data_internal$raw %>% select(input$selected_variable)
        }else{
          data_internal$filtered <- NULL
        }
      }
    })

    output$filtered_table <- renderDataTable({
      if(is.null(data_internal$filtered)){
        DT::datatable(data_internal$raw, filter = c('top'))
      }else{
        DT::datatable(data_internal$filtered, filter = c('top'))
      }
    })

    # map UI
    output$map_columns <- renderUI({
      if(!is.null(data_internal$cols)){
        div(
          list(
            div(
              style = "display: inline-block; width = '10%'",
              br()
            ),
            div(
              style = "display: inline-block; width = '20%'",
              selectInput(
                inputId = "map_lat_select",
                label = h4("Select Latitude Column"),
                choices = data_internal$cols,
                width = "250px"
              )
            ),
            div(
              style = "display: inline-block; width = '20%'",
              selectInput(
                inputId = "map_lng_select",
                label = h4("Select Longitude Column"),
                choices = data_internal$cols,
                width = "250px"
              )
            ),
            div(
              style = "display: inline-block; width = '20%'",
              selectInput(
                inputId = "map_popup_select",
                label = h4("Select Popup Info"),
                choices = data_internal$cols,
                width = "250px"
              )
            ),
            div(
              style = "display: inline-block; width = '20%'",
              selectInput(
                inputId = "map_link_select",
                label = h4("Select Link Column"),
                choices = data_internal$cols,
                width = "250px"
              )
            )
          )
        )
      }
    })


    # Show the first "n" observations ----
    # The use of isolate() is necessary because we don't want the table
    # to update whenever input$obs changes (only when the user clicks
    # the action button)
    output$view <- renderTable({
      head(datasetInput(), n = isolate(input$obs))
    })

    output$plot1 <- renderPlot({
      eviatlas::GenTimeTrend(pilotdata)
    })

    output$plot2 <- renderPlot({
      c2 <- eviatlas::GenLocationTrend(data_internal$raw, c(17,16),10)
      c2
    })

    output$heatmap <- renderPlot({
      eviatlas::GenHeatMap(data_internal$raw, c(input$heat_select_x, input$heat_select_y))
    })

    output$heat_x_axis <- renderPrint({ input$heat_select_x })
    output$heat_y_axis <- renderPrint({ input$heat_select_y })

    output$map <- renderLeaflet({
      sys_map(data_internal$raw, input$map_lat_select, input$map_lng_select,
              popup_user=input$map_popup_select, links_user=input$map_link_select)
    })

    observe({
      leafletProxy("map")
    })

    #customize data table output
    cityinfo <- reactive({
      pilotdata
    })


  })
