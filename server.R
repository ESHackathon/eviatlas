## server.R ##

# load functions
source("GenHeatMap.R")
source("GenLocationTrend.R")
source("GenTimeTrend.R")
source("get_obs.R")
source("sys_map.R")
source("get_link_cols.R")

# Allow CSV files up to 100 MB
max_file_size_mb <- 100
options(shiny.maxRequestSize = max_file_size_mb*1024^2)

shinyServer(

  function(input, output, session){

    data_internal <- reactiveValues(
      raw = NULL,
      cols = NULL,
      short_cols = NULL,
      filtered = NULL
    )


    # DATA TAB
    # if no data are available but input$sample_or_real == 'sample', show intro text
    output$start_text <- renderPrint({
      if(is.null(data_internal$raw) & input$sample_or_real == 'user'){
        cat("EviAtlas is an open-source tool for creating systematic maps, a key element of systematic reviews. Upload a systematic review dataset (csv format) using the panel on the right, and then use the left sidebar to view a systematic map generated from your dataset, as well as some common plots used in systematic reviews.
           <h3>About Systematic Maps</h3><br>
           Systematic Maps are overviews of the quantity and quality of evidence in relation to a broad (open) question of policy or management relevance. The process and rigour of the mapping exercise is the same as for systematic review except that no evidence synthesis is attempted to seek an answer to the question. A critical appraisal of the quality of the evidence is strongly encouraged but may be limited to a subset or sample of papers when the quantity of articles is very large (and even be absent in exceptional circumstances). Authors should note that all systematic maps published in Environmental Evidence will have been conducted according to the CEE process. Please contact the Editors at an early stage of planning your review. More guidance can be found <a href='http://www.environmentalevidence.org' target='_blank' rel='noopener'>here</a>.<br><br>
           For systematic maps to be relevant to policy and practice they need to be as up-to-date as possible. Consequently, at the time of acceptance for publication, the search must be less than two years old. We therefore recommend that systematic maps should be submitted no later than 18 months after the search was conducted."
        )
      }else{
        cat("<h3>Attributes of uploaded data:</h3>")
      }
    })


    # if data are supplied, add them to data_internal
    observeEvent(input$sysmapdata_upload, {
      data_internal$raw <- read.csv(
        file = input$sysmapdata_upload$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        fileEncoding = input$upload_encoding,
        stringsAsFactors = F)
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
          "Dataset containing ", nrow(data_internal$raw),
          " rows and ", ncol(data_internal$raw),
          " columns. Column names as follows:<br>",
          paste(data_internal$cols, collapse = "<br>")
        ))
      }
    })

    # FILTER TAB
    output$filter_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        shinyWidgets::pickerInput(
          "selected_variable",
          label = "Select Columns to Display:",
          choices = colnames(data_internal$raw),
          selected = data_internal$cols,
          width = '100%', options = list(`actions-box` = TRUE, `selectedTextFormat`='static'),
          multiple = T
        )
      }
    })

    output$go_button <- renderUI({
      if(any(names(input) == "selected_variable")){
        if(!is.null(input$selected_variable)){
          actionButton("go_subset", "Apply Subset")
        }
      }
    })

    observeEvent(input$go_subset, {
      if(any(names(input) == "selected_variable")){
        if(input$selected_variable != ""){
          data_internal$filtered <- data_internal$raw %>% select(!!!input$selected_variable)
        }else{
          data_internal$filtered <- NULL
        }
      }
    })

    output$filtered_table <- DT::renderDataTable({
      if(is.null(data_internal$filtered)){
        DT::datatable(data_internal$raw, filter = c('top'),
                      style='bootstrap', options = list(scrollX = TRUE, responsive=T))
      }else{
        DT::datatable(data_internal$filtered, filter = c('top'),
                      style='bootstrap', options = list(scrollX = TRUE, responsive=T))
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
                label = "Select Latitude Column",
                choices = data_internal$cols,
                width = "250px"
              )
            ),
            div(
              style = "display: inline-block; width = '20%'",
              selectInput(
                inputId = "map_lng_select",
                label = "Select Longitude Column",
                choices = data_internal$cols,
                width = "250px"
              )
            ),
            div(
              style = "display: inline-block; width = '30%'",
              selectizeInput(
                inputId = "map_popup_select",
                label = "Select Popup Info",
                selected = data_internal$cols[1],
                choices = data_internal$cols,
                width = "250px",
                multiple = T
              )
            ),
            div(
              style = "display: inline-block; width = '20%'",
              selectInput(
                inputId = "map_link_select",
                label = "Select Link Column (in pop-up)",
                choices = c("None", get_link_cols(data_internal$raw)),
                selected = "None",
                width = "250px"
              )
            ),
            div(
              style = "display: inline-block; width = '20%'",
              materialSwitch(
                inputId = "map_cluster_select",
                label = "Cluster Map Points?",
                value = TRUE,
                status = "primary"
              )
            )
          )
        )
      } else {wellPanel('To use the map, upload data in the "About EviAtlas" tab!')}
    })

    # BARPLOT
    output$barplot_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        selectInput(
          inputId = "select_x1",
          label = "Select variable",
          choices = data_internal$cols,
          selected = data_internal$cols[1]
        )
      }
    })

    # Location Frequency Plot
    output$location_plot_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        selectInput(
          inputId = "select_loc_col",
          label = "Select Country/Region/Location Variable",
          choices = data_internal$cols,
          selected = NULL
        )
      }
    })

    ## HEATMAP
    output$heatmap_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        div(
          list(
            div(
              style = "display: inline-block; width = '10%'",
              br()
            ),
            div(
              style = "display: inline-block; width = '40%'",
              selectInput(
                inputId = "heat_select_x",
                label = "Select X variable",
                choices = data_internal$cols,
                selected = data_internal$cols[1]
              )
            ),
            div(
              style = "display: inline-block; width = '40%'",
              selectInput(
                inputId = "heat_select_y",
                label = "Select Y variable",
                choices = data_internal$cols,
                selected = data_internal$cols[2]
              )
            )
          )
        )
      }
    })


    #geom_bar rather than geom_histogram so that non-continous variables can be plotted
    output$plot1 <- renderPlot({
      ggplot(data_internal$raw, aes_string(x = input$select_x1))+
        geom_bar(
          alpha = 0.9,
          stat = "count",
          fill = "light blue"
        ) +
        labs(y = "No of studies") +
        ggtitle("") +
        theme_bw() +
        theme(
          axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          plot.title = element_text(hjust = .5),
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })

    output$plot2 <- renderPlot({
      if (!is.null(input$location_plot_selector)){
        GenLocationTrend(data_internal$raw, input$location_column)
      }
    })
    
    output$save_plot_1_button <- renderUI({
      if(!is.null(data_internal$raw)){
        output$save_plot_1 <- downloadHandler(
          filename = 'EviAtlas1.png',
          content = function(file) {
            device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
            ggsave(file, plot = input$plot1, device = device)
          }
        )
      }
    })
    
    observeEvent(input$save_plot_1_button, {
      ggplot2::ggsave("evitatlas_plot_1.pdf", plot=input$plot1, device="pdf")
    })
    
    output$save_plot_2_button <- renderUI({
      if(!is.null(data_internal$raw)){
        actionButton("save_plot_2", "Save Plot 2 (bottom)")
      }
    })
    
    observeEvent(input$save_plot_1_button, {
      ggplot2::ggsave("evitatlas_plot_2.pdf", plot=input$plot2, device="pdf")
    })
    

    output$heatmap <- renderPlot({
      eviatlas::GenHeatMap(data_internal$raw, c(input$heat_select_x, input$heat_select_y))
    })

    output$heat_x_axis <- renderPrint({ input$heat_select_x })
    output$heat_y_axis <- renderPrint({ input$heat_select_y })

    output$map <- renderLeaflet({
      # Try to generate map; if that fails, show blank map
      tryCatch(sys_map(data_internal$raw, input$map_lat_select,
                       input$map_lng_select,
                       popup_user = input$map_popup_select,
                       links_user = input$map_link_select,
                       cluster_points = input$map_cluster_select),
               error = function(x) {leaflet::leaflet() %>% leaflet::addTiles()}
               )

    })

    observe({
      leafletProxy("map")
    })


  })
