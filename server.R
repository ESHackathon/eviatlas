## server.R ##

# load functions
source("src/GenHeatMap.R")
source("src/GenLocationTrend.R")
source("src/GenTimeTrend.R")
source("src/sys_map.R")
source("src/get_link_cols.R")
source("src/get_coord_cols.R")

# load data + text
load("data/pilotdata.rda")
start_text <- read_file("html/AboutEvi.html")
about_sysmap_text <- read_file("html/AboutSysMap.html")
how_works_text <- read_file("html/HowEviWorks.html")

# maximum upload size 100 MB-- could be increased if proves problematic for users and we have server space
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
      cat(start_text)
    })    
    output$about_sysmap_text <- renderPrint({
      cat(about_sysmap_text)
    })    
    output$how_works_text <- renderPrint({
      cat(how_works_text)
    })
    output$uploaded_attributes <- renderPrint({
      if(is.null(data_internal$raw) & input$sample_or_real == 'user'){
        cat("Upload a dataset using the panel to the right -->")
      } else {
        cat("<h3>Attributes of uploaded data:</h3>")
      }
    })    
    
    # output$help_text <- renderPrint({
    #   cat("<br><br><br><br>Find a bug? Have a suggestion for future improvements? Feel free to contact Neal Haddaway (Research Fellow at the Stockholm Environment Institute): <a href='mailto:neal.haddaway@sei.org'>neal.haddaway@sei.org</a></span>"
    #     )
    # })


    # if data are supplied, add them to data_internal
    observeEvent(input$sysmapdata_upload, {
      data_internal$raw <- read.csv(
        file = input$sysmapdata_upload$datapath,
        header = input$header,
        sep = input$sep,
        dec = input$dec, 
        quote = input$quote,
        fileEncoding = input$upload_encoding,
        stringsAsFactors = F)
      data_internal$cols <- colnames(data_internal$raw)
      data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
    })

    # if user switches back to internal data, supply info on that instead
    observeEvent(input$sample_or_real, {
      if(input$sample_or_real == "sample"){
        data_internal$raw <- eviatlas_pilotdata
        data_internal$cols <- colnames(eviatlas_pilotdata)
        data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
      }else{
        data_internal$raw <- NULL
        data_internal$filtered <- NULL
        data_internal$cols <- NULL
      }
    })

    # give an outline of what that dataset contains
    output$data_summary <- renderPrint({
      if(!is.null(data_internal$raw)){
        cat(paste0(
          "You've uploaded a dataset containing ", nrow(data_internal$raw),
          " rows and ", ncol(data_internal$raw),
          " columns. If this is not what you expected, you might want to adjust the CSV properties settings on the right and try again.<br>",
          "<br> Detected column names as follows:<br>",
          paste(data_internal$cols, collapse = "<br>")
        ))
      }
    })

    # FILTER TAB
    output$filter_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        shinyWidgets::pickerInput(
          "selected_variable",
          label = "Select Columns:",
          choices = colnames(data_internal$raw),
          selected = data_internal$cols[1:10],
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
      } else {wellPanel('To start, upload data in the "About EviAtlas" tab.')}
    })

    observeEvent(input$go_subset, {
      if(any(names(input) == "selected_variable")){
        if(input$selected_variable != ""){
          data_internal$filtered <- data_internal$raw %>% 
            select(!!!input$selected_variable)
        }else{
          data_internal$filtered
        }
      }
    })
    
    output$filtered_table <- DT::renderDataTable(
      DT::datatable(data_internal$filtered, filter = c('top'),
                    caption = "Use the boxes below column headers to filter data",
                    class = c('display', 'compact'), 
                    style='bootstrap',
                    options = list(scrollX = TRUE, 
                                   scrollY = TRUE, 
                                   responsive=T,
                                   columnDefs = list(list(
                                     targets = c(1:min(25, ncol(data_internal$filtered))), # will apply render function to lesser of first 25 columns or number of columns in displayed data
                                     render = JS( # limits character strings longer than 50 characters to their first 30 chars, and has whole string appear as a tooltip
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 50 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                       "}")
                                     )))),
      server = T)
    
    # download the filtered data
    output$download_filtered = downloadHandler(
      'eviatlas-datatable-filtered.csv',
      content = function(file) {
        s = input$filtered_table_rows_all
        write.csv(data_internal$filtered[s, , drop = FALSE], file)
        }
      )
    
    # map UI
    
    output$map_columns <- renderUI({
      if(!is.null(data_internal$cols)) {
        div(list(
          div(
            selectInput(
              inputId = "map_lat_select",
              label = "Select Latitude Column",
              choices = data_internal$cols,
              selected = get_latitude_cols(data_internal$raw)
            )
          ),
          div(
            selectInput(
              inputId = "map_lng_select",
              label = "Select Longitude Column",
              choices = data_internal$cols,
              selected = get_longitude_cols(data_internal$raw)
            )
          ))
        )
      } else {wellPanel('To use the map, upload data in the "About EviAtlas" tab.')}
    })
    
    output$atlas_filter <- renderUI({
      req(data_internal$raw)
      div(
        title = "Use the Map Database tab to subset data",
        shinyWidgets::materialSwitch(
          inputId = "map_filtered_select",
          label = "Use filtered data?",
          value = FALSE,
          inline = T,
          status = "primary"
        )
      )
    })

    output$atlas_link_popup <- renderUI({
      req(data_internal$raw)
      div(
        title = "If your dataset has a link to each study, you can include it in the popup when a point is clicked with the mouse",
        selectInput(
          inputId = "map_link_select",
          label = "Select Link Column (in pop-up)",
          choices = c("", get_link_cols(data_internal$raw)),
          selected = ""
        )
      )

    })
    
    
  ############################################################################################################################################  
    output$atlas_selectmap <- renderUI({                                       
            req(data_internal$raw)
            div(
                    title = "You can change the default basemap, for example to change the language",
                    selectInput(
                            inputId = "map_basemap_select",                      
                            label = "Select Basemap",                            
                            choices = c("", OpenStreetMap, OpenTopoMap, Stamen.TonerLite, Esri.WorldStreetMap),
                            selected = ""
                    )
            )
    
    })
    #########################################################################################################################################

    output$atlas_popups <- renderUI({
      req(data_internal$raw)
      div(
        title = "Multiple columns are allowed as popups",
        selectizeInput(
          inputId = "map_popup_select",
          label = "Select Popup Info",
          selected = data_internal$cols[1],
          choices = data_internal$cols,
          multiple = T
        )
      )
    })

    output$cluster_columns <- renderUI({
      req(data_internal$raw)
      div(
        title = "Toggle displaying points in relative geographic clusters",
        shinyWidgets::materialSwitch(
          inputId = "map_cluster_select",
          label = "Cluster Map Points?",
          value = FALSE,
          status = "primary"
        )
      )
    })
    
    output$cluster_size <- renderUI({
      req(data_internal$raw)
      div(
        title = "Adjust cluster size",
          shinyWidgets::noUiSliderInput(
            inputId = "cluster_size_select",
            label = "Cluster Distance",
            value = 2,
            step = 1,
            min = 2,
            max = 8)
      )
    })
    
    output$atlas_color_by <- renderUI({
      req(data_internal$raw)
      div(
        title="Select variable to color points by",
        selectInput(
          inputId = "atlas_color_by_select",
          label = "Color points by:",
          choices = c("", data_internal$cols),
          selected = ""
        )
      )
    })
      
    
    observeEvent(input$map_filtered_select, {
      # Change values for map inputs whenever button is toggled
      updateSelectInput(
        session,
        "map_lat_select",
        choices = if (input$map_filtered_select) {
          colnames(data_internal$filtered)
        } else {
          colnames(data_internal$raw)
        },
        selected = if (input$map_filtered_select) {
          get_latitude_cols(data_internal$filtered)
        } else {
          get_latitude_cols(data_internal$raw)
        }
      )
      
      updateSelectInput(
        session,
        "map_lng_select",
        choices = if (input$map_filtered_select) {
          colnames(data_internal$filtered)
        } else {
          colnames(data_internal$raw)
        },
        selected = if (input$map_filtered_select) {
          get_longitude_cols(data_internal$filtered)
        } else {
          get_longitude_cols(data_internal$raw)
        }
      )
      
      updateSelectInput(session, "map_link_select",
                        choices = c("", if (input$map_filtered_select) {
                          get_link_cols(data_internal$filtered)
                        } else {
                          get_link_cols(data_internal$raw)
                        }))
      
      updateSelectInput(
        session,
        "map_popup_select",
        choices = if (input$map_filtered_select) {
          colnames(data_internal$filtered)
        } else {
          data_internal$cols
        },
        selected = if (input$map_filtered_select) {
          colnames(data_internal$filtered)[1]
        } else {
          data_internal$cols[1]
        }
      )
    })
    
    # BARPLOT
    output$barplot_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        selectInput(
          inputId = "select_timetrend_col",
          label = "Select Year variable",
          choices = c("", data_internal$cols),
          selected = ""
        )
      }
    })

    # Location Frequency Plot
    output$location_plot_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        selectInput(
          inputId = "select_loc_col",
          label = "Select Country/Region/Location Variable",
          choices = c("", data_internal$cols),
          selected = ""
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
                choices = c("", data_internal$cols),
                selected = ""
              )
            ),
            div(
              style = "display: inline-block; width = '40%'",
              selectInput(
                inputId = "heat_select_y",
                label = "Select Y variable",
                choices = c("", data_internal$cols),
                selected = ""
              )
            )
          )
        )
      }
    })

    #geom_bar rather than geom_histogram so that non-continous variables can be plotted
    gen_time_trend_plot <- reactive({
      ggplot(data_internal$raw, aes_string(x = input$select_timetrend_col)) +
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
    
    gen_location_trend_plot <- reactive({
      GenLocationTrend(data_internal$raw, input$select_loc_col)
    })
    
    output$plot1 <- renderPlot({
      req(input$select_timetrend_col)
      gen_time_trend_plot()
    })

    output$plot2 <- renderPlot({
      req(input$select_loc_col)
      gen_location_trend_plot()
    })
    
    output$save_plot_1 <- downloadHandler(
          filename = 'eviatlas1.png',
          content = function(file) {
            device <- function(..., width, height) {
              grDevices::png(..., width = width, height = height,
                             res = 300, units = "in")
            }
            ggsave(file, plot = gen_time_trend_plot(), device = device)
          }
        )
    
    output$save_plot_2 <- downloadHandler(
      filename = 'eviatlas2.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file, plot = gen_location_trend_plot(), device = device)
      }
    )
    
    gen_heatmap <- reactive({
      GenHeatMap(data_internal$raw, c(input$heat_select_x, input$heat_select_y))
    })
    
    output$heatmap <- renderPlot({
      req(input$heat_select_x)
      req(input$heat_select_y)
      gen_heatmap()
    })

    output$heat_x_axis <- renderPrint({ input$heat_select_x })
    output$heat_y_axis <- renderPrint({ input$heat_select_y })
    
    output$save_heatmap <- downloadHandler(
      filename = 'eviatlasHeatmap.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = width,
                         res = 300, units = "in")
        }
        ggsave(file, plot = gen_heatmap(), device = device)
      }
    )
    
    generate_systematic_map <- reactive({
      # Try to generate map; if that fails, show blank map
      tryCatch(
        sys_map(if(input$map_filtered_select) {data_internal$filtered[input$filtered_table_rows_all, , drop = FALSE]} else {data_internal$raw},
                input$map_lat_select,
                input$map_lng_select,
                popup_user = input$map_popup_select,
                links_user = input$map_link_select,
                cluster_size_user = input$cluster_size_select,
                cluster_points = input$map_cluster_select,
                color_user = input$atlas_color_by_select,
                basemap_user = input$map_basemap_select,
                map_title=input$map_title_select), 
        error = function(x) {
          leaflet::leaflet() %>%
            leaflet::addTiles()
        }
      )
    })
    
    output$savemap_interactive <- downloadHandler(
      filename = "eviatlasMap.html",
      content = function(file){
        saveWidget(
          widget = generate_systematic_map(), file = file
        )
      }
    )
    
    output$savemap_pdf <- downloadHandler(
      filename = 'eviatlasMap.pdf',
      content = function(file) {
        mapview::mapshot(generate_systematic_map(), 
                         file = file)
      }
    )
    
    output$savemap_png <- downloadHandler(
      filename = 'eviatlasMap.png',
      content = function(file) {
        mapview::mapshot(generate_systematic_map(), 
                         file = file)
      }
    )

    output$map <- renderLeaflet({
      generate_systematic_map() %>%
        onRender(
          "function(el, x) {
            L.easyPrint({
              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
              filename: 'EviAtlasMap',
              exportOnly: true,
              hideControlContainer: true
            }).addTo(this);
            }"
        )
    })
    
    

    observe({
      leafletProxy("map")
    })

    outputOptions(output, "cluster_columns", suspendWhenHidden = FALSE)  
    
  })
