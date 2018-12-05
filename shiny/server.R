## server.R ##

library(tidyverse)
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)

shinyServer(

  function(input, output, session){

    # server logic to read user uploaded file ----
    output$user_data <- renderDataTable({
      # input$sysmapdata_upload will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.

      if(input$sample_or_real == 'sample') {
        eviatlas::pilotdata
      } else {
        req(input$sysmapdata_upload)

        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error

        tryCatch(
          {
            df <- read.csv(input$sysmapdata_upload$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote,
                           fileEncoding = input$upload_encoding)
          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
          }
        )
      }
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
      ggplot(pilotdata,aes_string(x=input$select_x1))+
        geom_bar(alpha=0.9, stat="count",fill="light blue") + #I have gonr for geom_bar rather than geom_histogram so that non-continous vriables can be plotted - is that sensible
        labs(y="No of studies") + ggtitle("") +
        theme_bw() +
        theme(axis.line =element_line(colour = "black"),
                       panel.background =element_blank(),
                       plot.title = element_text(hjust = .5),
                       text = element_text(size=14),
                      axis.text.x = element_text(angle=45, hjust=1))
      })

    output$heatmap <- renderPlot({
      eviatlas::GenHeatMap(pilotdata, c(input$heat_select_x, input$heat_select_y))
    })

    output$heat_x_axis <- renderPrint({ input$heat_select_x })
    output$heat_y_axis <- renderPrint({ input$heat_select_y })

    output$map <- renderLeaflet({
      sys_map(pilotdata, pilotdata$latplot, pilotdata$lngplot, popup_user = input$map_popup_select)
    })

    observe({
      leafletProxy("map")
    })

    #customize data table output
    cityinfo <- reactive({
      pilotdata
    })


  })
