## server.R ##

# load functions
source("src/GenHeatMap.R")
source("src/GenLocationTrend.R")
source("src/GenTimeTrend.R")
source("src/sys_map.R")
source("src/sys_map_shapefile.R")
source("src/get_link_cols.R")
source("src/get_coord_cols.R")
source("src/get_histogram_viable_cols.R")

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


    # if CSV data are supplied, add them to data_internal
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
    
    # if shapefile data are supplied, add them to data_internal
    observeEvent(input$shape, {
      req(input$shape)
      shpdf <- input$shape
      tempdirname <- dirname(shpdf$datapath[1])
      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
      }
      data_internal$raw <- sf::st_read(
        paste(tempdirname,
              shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
              sep="/")
        )
      
      data_internal$cols <- colnames(data_internal$raw)
      data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
    })
    
    data_active <- reactive({
      req(data_internal$raw)

      d_out <- if(input$map_filtered_select == TRUE) {data_internal$filtered} else {data_internal$raw}
      d_out
    })

    # if user switches back to internal data, supply info on that instead
    observeEvent(input$sample_or_real, {
      if(input$sample_or_real == "sample"){
        data_internal$raw <- eviatlas_pilotdata
        data_internal$cols <- colnames(eviatlas_pilotdata)
        data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
      } else {
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
          " columns. If this is not what you expected, you might want to",
          " adjust the CSV properties settings on the right and try again.<br>",
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
      DT::datatable(data_active(), #filter = c('top'),
                    # caption = "Use the boxes below column headers to filter data",
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
      req(input$sample_or_real != 'shapefile')
      
      if(!is.null(data_internal$cols)) {
        div(list(
          div(
            selectInput(
              inputId = "map_lat_select",
              label = "Select Latitude Column",
              choices = colnames(data_active()),
              selected = get_latitude_cols(data_active())
            )
          ),
          div(
            selectInput(
              inputId = "map_lng_select",
              label = "Select Longitude Column",
              choices = colnames(data_active()),
              selected = get_longitude_cols(data_active())
            )
          ))
        )
      } else {wellPanel('To use the map, upload data in the "About EviAtlas" tab.')}
    })
    
    output$atlas_filter <- renderUI({
      # req(data_internal$raw)
      
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
      # req(data_internal$raw)
      
      div(
        title = "If your dataset has a link to each study, you can include it in the popup when a point is clicked with the mouse. If you have any hyperlinks you wish to display in the pop-up (e.g. email addresses or URLs), select them here.",
        selectInput(
          inputId = "map_link_select",
          label = "Select Link Column (in pop-up)",
          choices = c("", get_link_cols(data_internal$raw)),
          selected = ""
        )
      )

    })
    
    
    output$atlas_selectmap <- renderUI({                                       
            req(data_internal$raw)
      
            div(
              title = "You can change the default basemap to highlight different geographical features or change the language of map labels",
              selectInput(
                inputId = "map_basemap_select",
                label = "Select Basemap",
                choices = c("OpenStreetMap", "OpenTopoMap", "Stamen.TonerLite", "Esri.WorldStreetMap"),
                selected = "OpenStreetMap"
                )
              )
            })

    
    output$atlas_popups <- renderUI({
      # req(data_internal$raw)
      
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
      # req(data_internal$raw)
      div(
        title = "Adjust cluster size",
          shinyWidgets::noUiSliderInput(
            inputId = "cluster_size_select",
            label = "Cluster Distance",
            value = 4,
            step = 1,
            min = 4,
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
        choices = colnames(data_active()),
        selected = get_latitude_cols(data_active())
      )
      
      updateSelectInput(
        session,
        "map_lng_select",
        choices = colnames(data_active()),
        selected = get_longitude_cols(data_active())
      )
      
      updateSelectInput(session, "map_link_select",
                        choices = c("", get_link_cols(data_active()) )
                        )
      
      updateSelectInput(session, "map_popup_select",
        choices = colnames(data_active()),
        selected = colnames(data_active())[1]
        )
    })
    
    # BARPLOT
    output$barplot_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        selectInput(
          inputId = "select_timetrend_col",
          label = "Select variable 1",
          choices = c("", get_histogram_viable_columns(data_active())),
          selected = ""
        )
      }
    })

    # Location Frequency Plot
    output$location_plot_selector <- renderUI({
      if(!is.null(data_internal$cols)){
        selectInput(
          inputId = "select_loc_col",
          label = "Select Variable 2",
          choices = c("", get_histogram_viable_columns(data_active())),
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
              title = "Select which categorical variable you wish to cross tabulate along the x axis in a heat map. Values must be discrete categories (i.e. not free text and not decimal)", 
              selectInput(
                inputId = "heat_select_x",
                label = "Select X variable",
                choices = c("", get_histogram_viable_columns(data_active())),
                selected = ""
              )
            ),
            div(
              style = "display: inline-block; width = '40%'",
              title = "Select which categorical variable you wish to cross tabulate along the y axis in a heat map. Values must be discrete categories (i.e. not free text and not decimal)",
              selectInput(
                inputId = "heat_select_y",
                label = "Select Y variable",
                choices = c("", get_histogram_viable_columns(data_active())),
                selected = ""
              )
            )
          )
        )
      }
    })

    #geom_bar rather than geom_histogram so that non-continous variables can be plotted
    gen_time_trend_plot <- reactive({
      GenTimeTrend(data_active(), input$select_timetrend_col)
    })
    
    gen_location_trend_plot <- reactive({
      GenLocationTrend(data_active(), input$select_loc_col)
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
      GenHeatMap(data_active(), c(input$heat_select_x, input$heat_select_y))
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
      if (input$sample_or_real == "shapefile") {
        sys_map_shapefile(data_active() )
      } else {
        sys_map(data_active() )
      }
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
      req(!is.null(input$map_link_select)) #could be anything in the evidence atlas pane

      if (input$map_link_select != "") {
        links_input <- sapply(data_active()[input$map_link_select], as.character)
        links = paste0("<strong><a target='_blank' rel='noopener noreferrer' href='", 
                       links_input, "'>Link to paper</a></strong>")
      } else {links <- ""}
      
      popup_string <- ''
      for (popup in input$map_popup_select) {
        popup_string = paste0(popup_string, "<strong>", popup, '</strong>: ',
                              data_active()[, popup], "<br/>")
      }

      radiusby <- input$atlas_radius_select
      
      lat_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(input$map_lat_select)))
      lng_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(input$map_lng_select)))
        
      leafletProxy("map", data = data_active()) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                  popup = ~paste(popup_string, links), 
                                  radius = as.numeric(radiusby)
        ) 
      })
      # popup_user = input$map_popup_select,
      # links_user = input$map_link_select,
      # cluster_size_user = input$cluster_size_select,
      # cluster_points = input$map_cluster_select,
      # color_user = input$atlas_color_by_select
      # 

      # 
      # if (input$atlas_color_by_select != "") {
      #   color_user <- input$atlas_color_by_select
      #   factpal <- colorFactor(RColorBrewer::brewer.pal(9, 'Set1'), data_active()$color_user)
      #   colorby <- ~factpal(data_active()$color_user)
      # } else {colorby <- "blue"}
      # 

      # 
      # if (input$map_cluster_select == T) {
      #   leafletProxy("map") %>%
      #     leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
      #                               popup = ~paste(popup_string, links),
      #                               radius = ~as.numeric(radiusby * 3),
      #                               color = colorby,
      #                               stroke = FALSE, fillOpacity = 0.7,
      #                               clusterOptions = markerClusterOptions(freezeAtZoom = input$cluster_size_select) )
      # } else {
      #   leafletProxy("map") %>%
      #     leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
      #                               popup = ~paste(popup_string, links),
      #                               radius = ~as.numeric(radiusby),
      #                               color = colorby,
      #                               label = ~popup_string %>% lapply(shiny::HTML)
      #     )
      # }
      
      
    # })
    
    observeEvent(input$map_title_select, {
      
      leafletProxy("map") %>%
        leaflet::removeControl("atlas_title") %>%
        leaflet::addControl(input$map_title_select, 
                            position = "topright", 
                            className="map-title",
                            layerId = "atlas_title")
      
    })    
    
    observeEvent(input$map_basemap_select, {
      leafletProxy("map") %>%
        leaflet::removeTiles("atlas_basemap") %>%
        leaflet::addProviderTiles(input$map_basemap_select, 
                                  layerId = "atlas_basemap")
      
    })
  
    # colorpal <- reactive({
    #   color_column <- input$atlas_color_by_select
    #   
    #   if (color_column != "") {
    #     factpal <- colorFactor(RColorBrewer::brewer.pal(9, 'Set1'), color_column)
    #     colorby <- ~factpal(data_active()$color_column)
    #     } else {
    #       colorby <- "blue"
    #     }
    #   colorby
    # })
    # 
    # observeEvent(input$atlas_color_by_select, {
    #   
    #   lat_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(Latitude)))
    #   lng_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(Longitude)))
    #   
    #   leafletProxy("map") %>%
    #     leaflet::removeMarker("atlas_marker") %>%
    #     leaflet::addCircleMarkers(lat = 45, lng = 45,
    #                               color = colorpal(),
    #                               layerId = "atlas_marker")
    # })

    outputOptions(output, "cluster_columns", suspendWhenHidden = FALSE)  
    
  })
