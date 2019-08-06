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
load("data/pilotdata.rdata")
start_text <- read_file("html/AboutEvi.html")
about_sysmap_text <- read_file("html/AboutSysMap.html")
how_cite_text <- read_file("html/HowCiteEvi.html")
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
    output$how_cite_text <- renderPrint({
      cat(how_cite_text)
    })
    
    output$uploaded_attributes <- renderPrint({
      if(is.null(data_internal$raw) & input$sample_or_real == 'user'){
        cat("Upload a dataset using the panel to the right -->")
      } else {
        cat("<h3>Attributes of uploaded data:</h3>")
      }
    })    
    
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
      
      data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
    })
    
    data_active <- reactive({
      req(data_internal$raw)
      
      dataset_gateway <- (!is.null(input$map_filtered_select))
      d_out <- if (dataset_gateway && input$map_filtered_select == TRUE) {
        data_internal$filtered
      } else {
          data_internal$raw
        }
      
      d_out
    })

    # if user switches to internal data, clear in-app data
    observeEvent(input$sample_or_real, {
      if(input$sample_or_real == "sample"){
        data_internal$raw <- eviatlas_pilotdata
        data_internal$filtered <- data_internal$raw #instantiate filtered table with raw values
      } else {
        data_internal$raw <- NULL
        data_internal$filtered <- NULL
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
          paste(colnames(data_internal$raw), collapse = "<br>")
        ))
      }
    })

    # FILTER TAB
    output$filter_selector <- renderUI({
      req(data_internal$raw)        
      
      shinyWidgets::pickerInput(
        "selected_variable",
        label = "Select Columns:",
        choices = colnames(data_internal$raw),
        selected = colnames(data_active())[1:10],
        width = '100%', options = list(`actions-box` = TRUE, `selectedTextFormat`='static'),
        multiple = T
      )
    })

    output$go_button <- renderUI({
      if(!is.null(data_internal$raw)){
          actionButton("go_subset", "Apply Filter")
      } else {wellPanel('To start, upload data in the "About EviAtlas" tab.')}
    })

    observeEvent(input$go_subset, {
      data_internal$filtered <- filtered_df()
      
      updateMaterialSwitch(session = session, inputId = "mapdatabase_filter_select",
                           value = TRUE)
    })
    
    ##### begin dynamic filter #####

    fields <- reactive({
      c(colnames(data_internal$raw))
    })
    
    # filter_by <- function (df, ...) {
    #   filter_conditions <- quos(...)
    #   df %>% dplyr::filter(!!!filter_conditions)
    # }

    # filter on 1 column
    filter1_by <- function(df, fcol1, fv1) {
    filter_var1 <- dplyr::quo(fcol1)
    df %>% 
      filter_at(vars(!!filter_var1), all_vars(. == fv1))
    }
    
    # filter on 2 columns
    filter2_by <- function(df, fcol1, fv1, fcol2, fv2) {
      filter_var1 <- dplyr::quo(fcol1)
      filter_var2 <- dplyr::quo(fcol2)
      
      df %>%
        filter_at(vars(!!filter_var1), all_vars(. == fv1)) %>%
        filter_at(vars(!!filter_var2), all_vars(. == fv2))
      }
    
    # filter on 3 columns
    filter3_by <- function(df, fcol1, fv1, fcol2, fv2, fcol3, fv3) {
      filter_var1 <- dplyr::quo(fcol1)
      filter_var2 <- dplyr::quo(fcol2)
      filter_var3 <- dplyr::quo(fcol3)
      
      df %>% 
        filter_at(vars(!!filter_var1), all_vars(. == fv1)) %>% 
        filter_at(vars(!!filter_var2), all_vars(. == fv2)) %>%
        filter_at(vars(!!filter_var3), all_vars(. == fv3))
    }
    
    filtered_df <- reactive({  
      # case when all three filters are used
      if (input$filter3req & input$filter2req) {
        filter3_by(data_internal$raw, input$filter1, input$filter1val, 
                   input$filter2, input$filter2val,
                   input$filter3, input$filter3val) 
      } else if (input$filter2req) {
        # case when two filters are used
        filter2_by(data_internal$raw, input$filter1, input$filter1val, 
                   input$filter2, input$filter2val) 
      } else {
        # case when only one filter is used   
        filter1_by(data_internal$raw, input$filter1, input$filter1val)
      }})
    
    # vector of picklist values for the first selected filter 
    choicevec1 <- reactive({
      req(data_internal$raw)
      
      if (any(class(data_internal$raw) == 'sf')) {
        data_internal$raw %>%  
          sf::st_drop_geometry() %>%
          dplyr::select(input$filter1) %>% 
          unique()
      } else {
        data_internal$raw %>%  
          dplyr::select(input$filter1) %>% 
          unique()
      }
      
    })
    
  
    # select first filter column from fields vector 
    output$filter1eval <- renderUI({
      selectInput("filter1", "Select filter criteria 1:", choices = fields())
    })
    # renders the picklist for the first selected filter
    output$filter1choice <- renderUI(
      selectizeInput(
        "filter1val",
        "Select filter 1 condition:",
        choices = choicevec1(),
        multiple = TRUE
      )
    )
    # second column chosen from all remaining fields
    output$filter2eval <- renderUI({
      selectInput("filter2", "Select filter criteria 2:", 
                  choices = fields()[fields() != input$filter1])
    })
    # vector of picklist values for the second selected filter
    choicevec2 <- reactive({
      req(data_internal$raw)
      
      if (any(class(data_internal$raw) == 'sf')) {
        filter1_by(sf::st_drop_geometry(data_internal$raw), input$filter1, input$filter1val) %>%
          dplyr::select(input$filter2) %>%
          unique()
        } else {
          filter1_by(data_internal$raw, input$filter1, input$filter1val) %>%
            dplyr::select(input$filter2) %>%
            unique()      
        }
    })
    # renders picklist for filter 2
    output$filter2choice <- renderUI(
      selectizeInput(
        "filter2val",
        "Select filter 2 condition:",
        choices = choicevec2(),
        multiple = TRUE
      )
    )
    # third column selected from remaining fields
    output$filter3eval <- renderUI({
      selectInput("filter3", 
                  "Select filter criteria 3:",
                  choices = fields()[!fields() %in% c(input$filter1, input$filter2)])
    })
    # vector of picklist values for third selected column
    choicevec3 <- reactive({
      req(data_internal$raw)
      
      if (any(class(data_internal$raw) == 'sf')) {
        filter2_by(sf::st_drop_geometry(data_internal$raw), 
                   input$filter1, input$filter1val,
                   input$filter2, input$filter2val) %>% 
          dplyr::select(input$filter3) %>% 
          unique()
      } else {
        filter2_by(data_internal$raw, input$filter1, 
                   input$filter1val, input$filter2, 
                   input$filter2val) %>% 
          dplyr::select(input$filter3) %>%
          unique()
      }
      
    })
    
    # render picklist for filter 3
    output$filter3choice <- renderUI(
      selectizeInput("filter3val", "Select filter 3 condition:", choices = choicevec3(), multiple = TRUE)
    )
        
    ##### end dynamic filter ####
    
    output$filtered_table <- DT::renderDataTable(
      DT::datatable(data_active(), filter = c('top'),
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
      
      if(!is.null(data_internal$raw)) {
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
      req(input$sample_or_real != "shapefile") #does not work for shapefiles currently
      
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
          selected = colnames(data_active())[1],
          choices = colnames(data_active()),
          multiple = T
        )
      )
    })

    output$cluster_columns <- renderUI({
      req(data_internal$raw)
      req(input$sample_or_real != "shapefile") #does not work for shapefiles currently
      
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
      req(input$sample_or_real != "shapefile") #does not work for shapefiles currently
      
      div(
        title = "Adjust cluster sensitivity. Higher numbers correspond to smaller distances",
          shinyWidgets::noUiSliderInput(
            inputId = "cluster_size_select",
            label = "Cluster Sensitivity",
            value = 4,
            step = 1,
            min = 4,
            max = 16)
      )
    })
    
    output$atlas_color_by <- renderUI({
      req(data_internal$raw)
      req(input$sample_or_real != "shapefile") #does not work for shapefiles currently
      
      div(
        title="Select variable to color points by",
        selectInput(
          inputId = "atlas_color_by_select",
          label = "Color points by:",
          choices = c("", colnames(data_active())),
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
      req(data_internal$raw)
    
      selectInput(
        inputId = "select_timetrend_col",
        label = "Select variable 1",
        choices = c("", get_histogram_viable_columns(data_active())),
        selected = ""
      )
    })

    # Location Frequency Plot
    output$location_plot_selector <- renderUI({
      req(data_internal$raw)
      
      selectInput(
        inputId = "select_loc_col",
        label = "Select Variable 2",
        choices = c("", get_histogram_viable_columns(data_active())),
        selected = ""
      )
    })

    ## HEATMAP
    output$heatmap_selector <- renderUI({
      req(data_internal$raw)
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
    
    observeEvent(input$map_filter_select, {
      updateMaterialSwitch(session = session, inputId = "heatmap_filtered_select",
                         value = as.logical(input$map_filter_select))      
      updateMaterialSwitch(session = session, inputId = "barplots_filtered_select",
                         value = as.logical(input$map_filter_select))
      updateMaterialSwitch(session = session, inputId = "mapdatabase_filter_select",
                           value = as.logical(input$map_filter_select))
    })    
    
    observeEvent(input$heatmap_filter_select, {
      updateMaterialSwitch(session = session, inputId = "map_filtered_select",
                         value = as.logical(input$heatmap_filter_select))      
      updateMaterialSwitch(session = session, inputId = "barplots_filtered_select",
                         value = as.logical(input$heatmap_filter_select))
      updateMaterialSwitch(session = session, inputId = "mapdatabase_filter_select",
                           value = as.logical(input$heatmap_filter_select))
    })
    
    observeEvent(input$barplots_filter_select, {
      updateMaterialSwitch(session = session, inputId = "map_filtered_select",
                           value = as.logical(input$barplots_filter_select))      
      updateMaterialSwitch(session = session, inputId = "heatmap_filter_select",
                           value = as.logical(input$barplots_filter_select))      
      updateMaterialSwitch(session = session, inputId = "mapdatabase_filter_select",
                           value = as.logical(input$barplots_filter_select))
    })
    
    observeEvent(input$mapdatabase_filter_select, {
      updateMaterialSwitch(session = session, inputId = "map_filtered_select",
                           value = as.logical(input$mapdatabase_filter_select))      
      updateMaterialSwitch(session = session, inputId = "heatmap_filter_select",
                           value = as.logical(input$mapdatabase_filter_select))      
      updateMaterialSwitch(session = session, inputId = "barplots_filter_select",
                           value = as.logical(input$mapdatabase_filter_select))
    })
    
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
      # Generate basemap
      if (input$sample_or_real == "shapefile") {
        sys_map_shapefile(data_active(), popups = popup_string())
      } else {
        sys_map(data_active() )
      }
    })

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
    
    cluster_level <- reactive({input$cluster_size_select})
    
    popup_string <- reactive({
      popup_string <- ''

      for (popup in input$map_popup_select) {
        popup_string = paste0(popup_string, "<strong>", popup, '</strong>: ',
                              data_active()[[popup]], "<br/>")
      }
      popup_string
    })
    
    atlas_point_links <- reactive({
      if (input$map_link_select != "") {
        links_input <- sapply(data_active()[input$map_link_select], as.character)
        links = paste0("<strong><a target='_blank' rel='noopener noreferrer' href='", 
                       links_input, "'>Link to paper</a></strong>")
      } else {links <- ""}
      links
    })
    
    cluster_options <- reactive({
      if_else(input$map_cluster_select,
             parse(text=paste0('markerClusterOptions(freezeAtZoom = ', input$cluster_size_select, ')')),
             NULL)
    })
    
    observe({
      req(!is.null(input$atlas_color_by_select)) #could be anything in the evidence atlas pane
      req(input$sample_or_real != 'shapefile') #shapefiles are handled differently, so they have their own section

      radiusby <- input$atlas_radius_select

      lat_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(input$map_lat_select)))
      lng_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(input$map_lng_select)))

      if (input$atlas_color_by_select != "") {
        color_user <- input$atlas_color_by_select
        factpal <- colorFactor(RColorBrewer::brewer.pal(9, 'Set1'), data_active()$color_user)
        colorby <- ~factpal(data_active()[[color_user]])
      } else {colorby <- "blue"}
      
      leafletProxy("map", data = data_active()) %>%
          leaflet::clearMarkers() %>%
          leaflet::clearMarkerClusters() %>%
          leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                    popup = ~paste(popup_string(), atlas_point_links()),
                                    radius = ~as.numeric(radiusby * 3),
                                    color = colorby,
                                    stroke = FALSE, fillOpacity = 0.7,
                                    label = ~popup_string() %>% lapply(shiny::HTML),
                                    clusterOptions = eval(cluster_options())
                                    )
        
      })
    
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

    atlas_for_saving <- reactive({
      # This is redundant to everything in the app, but the is best solution I could find
      # for saving a map that's been heavily edited with leafletProxy
      if(input$sample_or_real == 'shapefile') {
        return(
          sys_map_shapefile(data_active(), 
                            popups = popup_string()) %>%
          setView(
            lng = input$map_center$lng,
            lat = input$map_center$lat,
            zoom = input$map_zoom
          ) %>%
          leaflet::addControl(
            input$map_title_select,
            position = "topright",
            className = "map-title",
            layerId = "atlas_title"
          ) %>%
          leaflet::addProviderTiles(input$map_basemap_select,
                                    layerId = "atlas_basemap")
        )
          
        }
      
      radiusby <- input$atlas_radius_select
      
      lat_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(input$map_lat_select)))
      lng_plotted <- as.numeric(unlist(data_active() %>% dplyr::select(input$map_lng_select)))
      
      if (input$atlas_color_by_select != "") {
        color_user <- input$atlas_color_by_select
        factpal <- colorFactor(RColorBrewer::brewer.pal(9, 'Set1'), data_active()$color_user)
        colorby <- ~factpal(data_active()[[color_user]])
      } else {colorby <- "blue"}
      
      # call the foundational Leaflet map
      generate_systematic_map() %>%
        # store the view based on UI
        setView(
          lng = input$map_center$lng,
          lat = input$map_center$lat,
          zoom = input$map_zoom
        ) %>%
        leaflet::addControl(input$map_title_select, 
                            position = "topright", 
                            className="map-title",
                            layerId = "atlas_title") %>%
        leaflet::addProviderTiles(input$map_basemap_select, 
                                  layerId = "atlas_basemap") %>%
        leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                  popup = ~paste(popup_string(), atlas_point_links()),
                                  radius = ~as.numeric(radiusby * 3),
                                  color = colorby,
                                  stroke = FALSE, fillOpacity = 0.7,
                                  label = ~popup_string() %>% lapply(shiny::HTML),
                                  clusterOptions = eval(cluster_options())
        )
      
    })
    
    output$savemap_interactive <- downloadHandler(
      filename = paste0('eviAtlasMap', Sys.Date(), '.html'),
      content = function(file){
        saveWidget(
          widget = atlas_for_saving(),
          file = file)
      }
    )
    
    output$savemap_pdf <- downloadHandler(
      filename = paste0('eviAtlasMap', Sys.Date(), '.pdf'),
      content = function(file) {
        mapview::mapshot(x = atlas_for_saving(), 
                         file = file,
                         cliprect = 'viewport',
                         selfcontained = FALSE)
      }
    )
    
    output$savemap_png <- downloadHandler(
      filename = paste0('eviAtlasMap', Sys.Date(), '.png'),
      content = function(file) {
        mapview::mapshot(x = atlas_for_saving(), 
                         file = file,
                         cliprect = 'viewport',
                         selfcontained = FALSE)
      }
    )
    
      
    outputOptions(output, "cluster_columns", suspendWhenHidden = FALSE)  

  })
