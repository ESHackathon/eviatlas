

#' The Shiny App UI.
#' @export
shiny_ui <- function() {
  ## ui.R ##

  easyprint_js_file <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"


  sidebar <- shinydashboard::dashboardSidebar(

    # sidebarUserPanel("EviAtlas Nav"),
    shinydashboard::sidebarMenu(
      id = "main_sidebar",
      shinydashboard::menuItem("About EviAtlas",
                               tabName = "about",
                               icon = shiny::icon("question")
      ),
      shinydashboard::menuItem("Evidence Atlas",
                               tabName = "home",
                               icon = shiny::icon("map")
      ),
      shinydashboard::menuItem("Map Database",
                               tabName = "data",
                               icon = shiny::icon("database")
      ),
      shinydashboard::menuItem("Descriptive Plots",
                               tabName = "insightplots",
                               icon = shiny::icon("home")
      ),
      shinydashboard::menuItem("Heatmap",
                               tabName = "heatmap",
                               icon = shiny::icon("fire")
      ),
      shinydashboard::menuItem("View Code",
                               href = "https://github.com/ESHackathon/eviatlas",
                               icon = shiny::icon("github")
      )
    )
  )


  home <- tags$html(
    tags$head(
      tags$title("EviAtlas"),
      tags$script(src = easyprint_js_file)
    ),
    tags$style(
      type = "text/css",
      "#map {height: calc(100vh - 240px) !important;}"
    ),
    tags$body(
      leaflet::leafletOutput("map")
    )
  )

  body <- shinydashboard::dashboardBody(
    shiny::tag("style", shiny::HTML("
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
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "about",
        fluidRow(
          mainPanel(
            wellPanel(
              tabsetPanel(
                tabPanel(title = "About EviAtlas", htmlOutput("start_text")),
                tabPanel(title = "About Systematic Maps", htmlOutput("about_sysmap_text")),
                tabPanel(title = "How to Use EviAtlas", htmlOutput("how_works_text")),
                tabPanel(title = "How to Cite EviAtlas", htmlOutput("how_cite_text"))
              )
            ),
            wellPanel(tabsetPanel(
              tabPanel(
                title = "Data Attributes", htmlOutput("uploaded_attributes"),
                tableOutput("data_summary")
              )
            ))
          ),
          # Sidebar panel for inputs
          sidebarPanel(
            tabsetPanel(
              tabPanel(
                title = "Upload Data",
                radioButtons(
                  "sample_or_real",
                  label = h4("Which Data to Use?"),
                  choices = list(
                    "Sample Data" = "sample",
                    "Upload from .csv format (spreadsheet)" = "user",
                    "Upload from .shp format (shapefile)" = "shapefile"
                  ),
                  selected = "user"
                ),
                shinyBS::bsTooltip("sample_or_real",
                                   title = "Select whether you want to try EviAtlas using the sample data from a recent systematic map, or whether you wish to upload your own data in the correct format",
                                   placement = "left",
                                   trigger = "hover"
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
                        ".csv"
                      ),
                      placeholder = "Systematic Map Data (100 MB Limit)"
                    )
                  ),

                  fluidRow(
                    column(
                      12,
                      wellPanel(
                        h5(strong("CSV Properties")),
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header row?", TRUE),

                        selectInput("upload_encoding",
                                    label = "Select File Encoding",
                                    choices = list(
                                      "Default" = "",
                                      "UTF-8",
                                      "latin1",
                                      "mac"
                                    ),
                                    selected = ""
                        ),
                        # Input: Select separator ----
                        selectInput("sep",
                                    "Field Separator",
                                    choices = c(
                                      ",",
                                      ";",
                                      Tab = "\t",
                                      "|"
                                    ),
                                    selected = ","
                        ),
                        # Input: Select quotes ----
                        selectInput(
                          "quote",
                          "Quote Delimiter",
                          choices = c(
                            None = "",
                            '"',
                            "'"
                          ),
                          selected = '"'
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.sample_or_real == 'shapefile'",
                fluidRow(column(
                  12,
                  fileInput(
                    "shape",
                    "Select all files associated with the shapefile (.shp, .dbf, .sbn, .sbx, .shx, .cpg, .prj)",
                    multiple = TRUE,
                    accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj", ".cpg"),
                    placeholder = "Select All Data Files At Once"
                  )
                ))
              )
            )
          )
        )
      ),

      shinydashboard::tabItem(
        tabName = "home",
        fluidRow(
          tabsetPanel(
            tabPanel(
              "Configure Map",
              wellPanel(fluidRow(
                column(
                  2,
                  uiOutput("map_columns")
                ),
                column(
                  4,
                  uiOutput("atlas_popups"),
                  uiOutput("atlas_link_popup")
                ),
                column(
                  2,
                  uiOutput("atlas_filter"),
                  uiOutput("atlas_color_by"),
                  uiOutput("atlas_selectmap")
                ),
                column(
                  2,
                  uiOutput("cluster_columns"),
                  conditionalPanel(
                    condition = "input.map_cluster_select",
                    uiOutput("cluster_size")
                  )
                ),
                column(
                  2,
                  textInput("map_title_select", "Atlas Title"),
                  conditionalPanel(
                    condition = "input.sample_or_real != 'shapefile'", # doesn't work for shapefiles currently
                    sliderInput("atlas_radius_select", "Point size",
                                min = 1, max = 8, value = 3,
                                ticks = F
                    )
                  )
                )
              ))
            ),
            # tabPanel('Advanced Options',
            #          column(2,
            #                 textInput('atlas_opacity_select', 'Placeholder for Opacity'))),
            tabPanel(
              "Save Map",
              wellPanel(
                downloadButton(
                  outputId = "savemap_interactive",
                  label = "Save Map (Interactive)"
                ),
                downloadButton(
                  outputId = "savemap_png",
                  label = "Save Map (png)"
                ),
                downloadButton(
                  outputId = "savemap_pdf",
                  label = "Save Map (PDF)"
                ),
                shinyBS::bsTooltip("savemap_interactive",
                                   title = "Save an interactive HTML version of the map using the current display settings. This HTML map can then be easily hosted on your own website",
                                   placement = "bottom", trigger = "hover"
                ),
                shinyBS::bsTooltip("savemap_png",
                                   title = "Save a static version of the map using the current display settings.",
                                   placement = "bottom", trigger = "hover"
                ),
                shinyBS::bsTooltip("savemap_pdf",
                                   title = "Save a static version of the map using the current display settings.",
                                   placement = "bottom", trigger = "hover"
                )
              )
            )
          )
        ),
        fluidRow(
          wellPanel(
            shinydashboard::box(width = 15, home)
          )
        )
      ),

      shinydashboard::tabItem(
        tabName = "data",
        fluidRow(
          # dynamic filter
          column(
            width = 12,
            wellPanel(
              # select first filter column from fields vector
              shiny::uiOutput("filter1eval"),
              # reference a uiOutput that will offer values for first column
              shiny::uiOutput("filter1choice"),
              # offer a checkbox to allow user to select a second filter
              shiny::checkboxInput("filter2req", "Add second filter?"),
              # set further conditional panels to appear in the same fashion
              shiny::conditionalPanel(
                condition = "input.filter2req",
                shiny::uiOutput("filter2eval"),
                shiny::uiOutput("filter2choice"),
                shiny::checkboxInput(
                  "filter3req",
                  "Add third filter?"
                )
              ),
              shiny::conditionalPanel(
                condition = "input.filter3req &
                                       input.filter2req",
                shiny::uiOutput("filter3eval"),
                shiny::uiOutput("filter3choice")
              ),
              uiOutput("go_button"),
              shinyWidgets::materialSwitch(
                inputId = "mapdatabase_filter_select",
                label = "Use filtered data:",
                value = FALSE,
                status = "primary"
              ),
              downloadButton("download_filtered", "Download Filtered Data")
            )
          )
        ),
        fluidRow(
          column(
            12,
            DT::dataTableOutput("filtered_table")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "insightplots",
        tabsetPanel(
          tabPanel(
            "Plot Inputs",
            fluidRow(
              column(3, uiOutput("barplot_selector")),
              column(4, uiOutput("location_plot_selector"))
            ),
            fluidRow(
              shinyWidgets::materialSwitch(
                inputId = "barplots_filter_select",
                label = "Use filtered data:",
                value = FALSE,
                status = "primary"
              )
            )
          )
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
      shinydashboard::tabItem(
        tabName = "heatmap",
        fluidRow(
          uiOutput("heatmap_selector")
        ),
        fluidRow(
          shinyWidgets::materialSwitch(
            inputId = "heatmap_filter_select",
            label = "Use filtered data:",
            value = FALSE,
            status = "primary"
          )
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

    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "EviAtlas"),
      sidebar,
      body
    )
}
