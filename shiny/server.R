## server.R ##

library(tidyverse)
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)

shinyServer(

  function(input, output, session){

    output$uploaded_file <- renderDataTable({
      file_in <- input$sysmapdata_upload

      if (is.null(file_in)) {
        return(NULL)
      }
      read.csv(file_in$datapath, fileEncoding = input$upload_encoding)
    })

    output$uploaded_preview <- renderDataTable({
      file_in <- input$sysmapdata_upload

      if (is.null(file_in$datapath)) {
        return(NULL)
      }

      head(read.csv(file_in$datapath, fileEncoding = input$upload_encoding))
    })


    #customize data table output
    data.explorer.table <- reactive({
      pilotdata %>%
        filter(Country %in% input$filter_table_countries)
    })

    # show data using DataTable
    output$table <- renderDataTable({
      DT::datatable(data.explorer.table(), rownames = c(data.explorer.table)) %>%
        formatStyle(input$selected, background = "skyblue",
                    fontWeight = 'bold')
    })


    datasetInput <- eventReactive(input$pilotdata, {
      switch(input$dataset,
             colnames(pilotdata))
    }, ignoreNULL = FALSE)

    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
      dataset <- datasetInput()
      summary(dataset)
    })

    # Filter by a user-specified variable
    filtered_data <- reactiveValues(subset = NULL)
    # Note: later functions can call filtered_data rather than pilotdata if req.

    output$value_selector <-renderUI({
      if(input$selected_variable != "none"){
        if(is.factor(pilotdata[[input$selected_variable]])){
          variable_levels <- levels(pilotdata[[input$selected_variable]])
        }else{
          variable_levels <- sort(unique(pilotdata[[input$selected_variable]]))
        }
        checkboxGroupInput(
          "selected_level",
          label = "Show level:",
          choices = variable_levels,
          inline = TRUE
        )
      }
    })

    output$go_button <-renderUI({
      if(input$selected_variable != "none"){
        actionButton("go_subset", "Apply Subset")
      }
    })

    observeEvent(input$go_subset, {
      if(input$selected_variable != "none"){
        rows <- which(pilotdata[[input$selected_variable]] %in% input$selected_level)
        filtered_data$subset <- pilotdata[rows, ]
      }else{
        filtered_data$subset <- NULL
      }
    })

    output$filtered_table <- renderDataTable({
      if(is.null(filtered_data$subset)){
        pilotdata
      }else{
        filtered_data$subset
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
      GenTimeTrend(pilotdata)
    })

    output$plot2 <- renderPlot({
      c2 <- GenLocationTrend(pilotdata, c(17,16),10)
      c2
    })

    output$heatmap <- renderPlot({
      GenHeatMap(pilotdata, c(input$heat_select_x, input$heat_select_y))
    })

    output$heat_x_axis <- renderPrint({ input$heat_select_x })
    output$heat_y_axis <- renderPrint({ input$heat_select_y })

    output$map <- renderLeaflet({
      sys_map(pilotdata, pilotdata$Plotted.lat., pilotdata$Plotted.long., popup_user = input$map_popup_select)
    })

    observe({
      leafletProxy("map")
    })

    #customize data table output
    cityinfo <- reactive({
      pilotdata
    })


  })
