## ui.R ##

easyprint_js_file <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 

sidebar <- dashboardSidebar(

  # sidebarUserPanel("EviAtlas Nav"),
  sidebarMenu(
      menuItem("About EviAtlas", tabName = "about", 
               icon = icon("question")),
      menuItem("Evidence Atlas", tabName = "home", 
               icon = icon("map")),
      menuItem("Map Database", tabName = "data", 
               icon = icon("database")),
      menuItem("Descriptive Plots", tabName = "insightplots", 
               icon = icon("home")),
      menuItem("Heatmap", tabName = "heatmap", 
               icon = icon("fire"))
      )
)


home <- tags$html(
  tags$head(
    tags$title('EviAtlas'),
    tags$script(src=easyprint_js_file)
    ),
  tags$style(type="text/css", 
             "#map {height: calc(100vh - 240px) !important;}"),
  tags$body(
    leafletOutput("map")
  )
)

body <- dashboardBody(
  tag("style", HTML("
    .right-side {
      background-color: #dbf0ee;
    }
    .skin-blue .main-header .logo {
      background-color: #4FB3A9;
      color: #ffffff;
    }
    .skin-blue .main-header .logo:hover {
      background-color: #2d6c66;
    }
    .skin-blue .main-header .navbar {
      background-color: #4FB3A9;
    }
    .skin-blue .main-header .sidebar-toggle {
      background-color: #2d6c66;
    }
  ")),
  tabItems(
    tabItem(tabName = "about",
            fluidRow(
              mainPanel(wellPanel(
                tabsetPanel(
                  tabPanel(title = 'About EviAtlas', htmlOutput("start_text")),
                  tabPanel(title = 'About Systematic Maps', htmlOutput("about_sysmap_text")),
                  tabPanel(title = 'How to Use EviAtlas', htmlOutput("how_works_text")),
                  tabPanel(title = 'Data Attributes', htmlOutput("uploaded_attributes"), 
                           tableOutput("data_summary"))
                ))
              ),

      #Sidebar panel for inputs
      sidebarPanel(
        tabsetPanel(
          tabPanel(title = "Upload Data",
            radioButtons(
              "sample_or_real",
              label = h4("Which Data to Use?"),
              choices = list(
                "Sample Data" = 'sample',
                "Upload User Data" = 'user'
              ),
              selected = "user"
            ),

            conditionalPanel(
              condition = "input.sample_or_real == 'user'",

              # Input: Select a file ----
              fluidRow(
                fileInput(
                  "sysmapdata_upload",
                  label = "Choose CSV File",
                  multiple = FALSE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"),
                  placeholder = "Systematic Map Data (100 MB Limit)"
                )),
              fluidRow(
                h5(strong("CSV Properties")),
                column(
                  6,
                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header row?", TRUE),
                  radioButtons("upload_encoding",
                               label = "Select File Encoding",
                               choices = list("Default" = "", "UTF-8", "latin1"),
                           selected = ""
                         )
                       ),
                       column(6,
                          # Input: Select separator ----
                          radioButtons("sep",
                                       "Separator",
                                       choices = c(
                                         Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"
                                         ),
                                       selected = ","
                          ), 
                         # Input: Select quotes ----
                         radioButtons("quote",
                                      "Quote Delimiter",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = '"'
                         ), 
                         # Input: Select decimal separator ----
                         radioButtons("dec",
                                      "Decimal Indicator",
                                      choices = c(".", ","),
                                      selected = '.')
                ))
            ))
        )))
    ),
    
    tabItem(tabName = "home",
      fluidRow(
        tabsetPanel(
          tabPanel("Configure Map",
                   wellPanel(fluidRow(
                       column(2, 
                              uiOutput("map_columns")
                              ),
                       column(4, 
                              uiOutput("atlas_popups"),
                              uiOutput("atlas_link_popup")
                              ),
                       column(2, 
                              uiOutput("atlas_filter"),
                              uiOutput("atlas_color_by")
                              ),
                       column(2, 
                              uiOutput("cluster_columns"),
                              conditionalPanel(condition = "document.getElementById('map_cluster_select').checked == true",
                                               uiOutput("cluster_size"))
                              ),
                       column(2,
                              textInput("map_title_select", "Atlas Title"))
                  ))
          ),
          tabPanel('Save Map',
                   wellPanel(
                     downloadButton(outputId = "savemap_interactive", 
                                    label = "Save Map (Interactive)"),
                     downloadButton(outputId = "savemap_png", 
                                    label = "Save Map (png)"),
                     downloadButton(outputId = "savemap_pdf", 
                                    label = "Save Map (PDF)"),
                     bsTooltip("savemap_interactive", title = "Save an interactive HTML version of the map using the current display settings. This HTML map can then be easily hosted on your own website", 
                               placement = "bottom", trigger = "hover"),
                     bsTooltip("savemap_png", title = "Save a static version of the map using the current display settings. Currently only saves global (zoomed-out) view. To download a smaller view, use the save button in the upper left-hand corner of the map.", 
                               placement = "bottom", trigger = "hover"),
                     bsTooltip("savemap_pdf", title = "Save a static version of the map using the current display settings.  Currently only saves global (zoomed-out) view. To download a smaller view, use the save button in the upper left-hand corner of the map.", 
                               placement = "bottom", trigger = "hover")
                     
                   )))
      ),
      fluidRow(
        wellPanel(
          box(width = 15, home)
        )
      )
    ),
    
    tabItem(
      tabName = "data",
      fluidRow(
          column(width = 3,
                 wellPanel(
                   uiOutput("filter_selector"),
                   uiOutput("go_button")
                 )), 
          column(width=1,
                 downloadButton('download_filtered', 'Download Filtered Data')
      )),
      fluidRow(
        column(12,
               DT::dataTableOutput("filtered_table")
        )
      )
    ),
    tabItem(tabName = "insightplots",
            tabsetPanel(
              tabPanel('Plot Inputs',
                       fluidRow(
                         column(3, uiOutput("barplot_selector")),
                         column(4, uiOutput("location_plot_selector"))
                       ))
            ),
            wellPanel(
              plotOutput("plot1"),
              downloadButton("save_plot_1")
            ),
            wellPanel(
              plotOutput("plot2"),
              downloadButton("save_plot_2")
            )
    ),
    tabItem(tabName = "heatmap",
      fluidRow(
        uiOutput("heatmap_selector")
      ),
      fluidRow(
        wellPanel(
          plotOutput("heatmap", width = "100%", height = "75vh"),
          downloadButton("save_heatmap")
        )
      )
    )
  )
)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "EviAtlas"),
    sidebar,
    body
  ))
