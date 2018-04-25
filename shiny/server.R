## server.R ##

library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(shinydashboard)
library(shiny)

shinyServer(

  function(input, output, session){
  #customize data table output
  data.explorer.table <- reactive({
    pilotdata %>%
      filter(Country %in% input$filter_table_countries)
  })

  output$filevalue <- renderPrint({
    str(input$file)
  })

  output$heat_x_axis <- renderPrint({ input$heat_select_x })
  output$heat_y_axis <- renderPrint({ input$heat_select_y })


    # show data using DataTable
  output$table <- renderDataTable({
    DT::datatable(data.explorer.table(), rownames = c(data.explorer.table)) %>%
      formatStyle(input$selected, background = "skyblue",
                  fontWeight = 'bold')
  })

  # throwaway code for
  output$CLCLCL <- renderText(input$filter_table_countries)


  graph1xlims <- reactive({
    xlim(input$xrange[1], input$xrange[2])
  })

  graph1ylims <- reactive({
    ylim(input$yrange[1], input$yrange[2])
  })

  output$plot1 <- renderPlot({
    GenTimeTrend(pilotdata)
  })

  output$plot2 <- renderPlot({
    c2 <- ggplot() +
      geom_point(alpha = .5) +
      ggtitle("EviAtlas Plot 2")
    c2
  })

  output$plot3 <- renderPlot({
    c3 <- ggplot() +
      geom_point() +
      ggtitle("EviAtlas Plot 3")
    c3
  })

  output$plot4 <- renderPlot({
    ggplot() +
      geom_violin(aes()) +
      ggtitle("EviAtlas Plot 4")
  })

  output$heatmap <- renderPlot({
      GenHeatMap(heatmap_test, c(input$heat_select_x, input$heat_select_y))
    # heatmp
  })


  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(18.0955533, 59.3338257, zoom = 3)
  })

  observe({

    leafletProxy("map")
  })

  #customize data table output
  cityinfo <- reactive({
    pilotdata
  })


})
