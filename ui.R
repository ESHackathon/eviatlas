## ui.R ##

easyprint_js_file <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 


sidebar <- dashboardSidebar(
  
  # sidebarUserPanel("EviAtlas Nav"),
  sidebarMenu(id = "main_sidebar",
    menuItem("About EviAtlas", tabName = "about", 
             icon = icon("question")),
    menuItem("Evidence Atlas", tabName = "home", 
             icon = icon("map")),
    menuItem("Map Database", tabName = "data", 
             icon = icon("database")),
    menuItem("Descriptive Plots", tabName = "insightplots", 
             icon = icon("home")),
    menuItem("Heatmap", tabName = "heatmap", 
             icon = icon("fire")),
    menuItem("Resources", tabName = "resources", 
             icon = icon("list")),
    menuItem("View Code",  
             href = "https://github.com/ESHackathon/eviatlas",
             icon = icon("github"))
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
                  tabPanel(title = 'How to Cite EviAtlas', htmlOutput("how_cite_text"))
                )),
                wellPanel(tabsetPanel(
                  tabPanel(title = 'Data Attributes', htmlOutput("uploaded_attributes"), 
                           tableOutput("data_summary"))
                ))
              ),
              #Sidebar panel for inputs
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
                    bsTooltip("sample_or_real",
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
                            ".csv"),
                          placeholder = "Systematic Map Data (100 MB Limit)"
                        )),
                      
                      fluidRow(
                       column(12,
                              wellPanel(
                              h5(strong("CSV Properties")),
                              # Input: Checkbox if file has header ----
                              checkboxInput("header", "Header row?", TRUE),
                              
                              selectInput("upload_encoding",
                                          label = "Select File Encoding",
                                          choices = list("UTF-8", 
                                                         "latin1",
                                                         "mac"),
                                          selected = "UTF-8"
                              ),
                              # Input: Select separator ----
                              selectInput("sep",
                                           "Field Separator",
                                           choices = c(
                                             ",",
                                             ";",
                                             Tab = "\t",
                                             '|'
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
                              ),
                              selectInput(
                                "dec",
                                "Decimal marker",
                                choices = c(".", ","),
                                selected = "."
                              ))))
                   )),

                  conditionalPanel(condition = "input.sample_or_real == 'shapefile'",
                                   fluidRow(column(
                                     12,
                                     fileInput(
                                       'shape',
                                       'Select all files associated with the shapefile (.shp, .dbf, .sbn, .sbx, .shx, .cpg, .prj)',
                                       multiple = TRUE,
                                       accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', '.cpg'),
                                       placeholder = "Select All Data Files At Once"
                                     )
                                   ))
                  )
                )
              ))
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
                                  uiOutput("atlas_color_by"),
                                  uiOutput("atlas_selectmap")
                           ),
                           column(2, 
                                  uiOutput("cluster_columns"),
                                  conditionalPanel(condition = "input.map_cluster_select",
                                                   uiOutput("cluster_size"))
                           ),
                           column(2,
                                  textInput("map_title_select", "Atlas Title"),
                                  conditionalPanel(condition = "input.sample_or_real != 'shapefile'", #doesn't work for shapefiles currently
                                                   sliderInput("atlas_radius_select", "Point size", 
                                                               min = 1, max = 8, value = 3,
                                                               ticks = F)))
                         ))
                ),
                # tabPanel('Advanced Options',
                #          column(2,
                #                 textInput('atlas_opacity_select', 'Placeholder for Opacity'))),
                tabPanel('Save Map',
                         wellPanel(
                           downloadButton(outputId = "savemap_interactive", 
                                          label = "Save Map (Interactive)"),
                           downloadButton(outputId = "savemap_png", 
                                          label = "Save Map (png)"),
                           downloadButton(outputId = "savemap_pdf", 
                                          label = "Save Map (PDF)"),
                           bsTooltip("savemap_interactive", 
                                     title = "Save an interactive HTML version of the map using the current display settings. This HTML map can then be easily hosted on your own website", 
                                     placement = "bottom", trigger = "hover"),
                           bsTooltip("savemap_png", 
                                     title = "Save a static version of the map using the current display settings.", 
                                     placement = "bottom", trigger = "hover"),
                           bsTooltip("savemap_pdf", 
                                     title = "Save a static version of the map using the current display settings.", 
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
        #dynamic filter
        column(width = 12,
               wellPanel(
                 # select first filter column from fields vector 
                 shiny::uiOutput("filter1eval"),
                 # reference a uiOutput that will offer values for first column
                 shiny::uiOutput("filter1choice"),
                 # offer a checkbox to allow user to select a second filter
                 shiny::checkboxInput("filter2req", "Add second filter?"),
                 # set further conditional panels to appear in the same fashion
                 shiny::conditionalPanel(condition = 'input.filter2req', 
                                         shiny::uiOutput("filter2eval"),
                                         shiny::uiOutput("filter2choice"),
                                         shiny::checkboxInput("filter3req", 
                                                              "Add third filter?")),
                 shiny::conditionalPanel(condition = 'input.filter3req & 
                                       input.filter2req', 
                                         shiny::uiOutput("filter3eval"),
                                         shiny::uiOutput("filter3choice")),
                 uiOutput("go_button"),
                 materialSwitch(
                   inputId = "mapdatabase_filter_select", 
                   label = "Use filtered data:",
                   value = FALSE,
                   status = "primary"
                 ),
                 downloadButton('download_filtered', 'Download Filtered Data')
               ))
        ),
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
                       ),
                       fluidRow(
                         materialSwitch(
                           inputId = "barplots_filter_select", 
                           label = "Use filtered data:",
                           value = FALSE,
                           status = "primary"
                         )
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
              uiOutput("heatmap_selector")),
              fluidRow(
                materialSwitch(
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
    ),
    tabItem(tabName = "resources",
            fluidRow(
              column(12,
                     wellPanel(
                       tabsetPanel(
                         tabPanel(title = 'Hosting maps', 
                                  h2("Hosting interactive maps online"),
                                  br(),
                                  "You can easily and quickly set up your interactive HTML file for your map as a standalone web page that you can publish alongside your evidence map by making use of GitHub's built in webpage hosting via github.io. The following video shows how you can do this for free in just a few minutes:",
                                  br(),
                                  br(),
                                  tags$iframe(src="https://www.youtube.com/embed/Igsxx_5RBB4", style="display:block; width:80%; height:80vh;", frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture")),
                       tabPanel(title = 'Case studies', 
                                h2('Case studies of EviAtlas visualisations'),
                                br(),
                                h4('1. Review of the evidence for oceans and human health relationships in Europe: a systematic map'),
                                'In their systematic map of evidence on the impacts of oceans on human health, ', tags$a(href="https://www.sciencedirect.com/science/article/pii/S0160412020322303#f0015", "Short et al."), ' produced an interactive map of the evidence. You can see their interactive map by following ', tags$a(href="https://sophieatlas2020.github.io/", "this link"), ' and looking at their map below:',
                                br(),
                                br(),
                                tags$iframe(src="https://sophieatlas2020.github.io/map3.html", style="width:80%; height:80vh;", frameborder="0")),
                       tabPanel(title = 'Reviews using EviAtlas', 
                                h2('Reviews using EviAtlas'),
                                br(),
                                'The following collection of reviews made use of EviAtlas to present their findings:',
                                br(),
                                br(),
                                tags$a(href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-019-0183-1", tags$li('Macura, B., Piniewski, M., Księżniak, M., Osuch, P., Haddaway, N. R., Ek, F., ... & Tattari, S. (2019). Effectiveness of ecotechnologies in agriculture for the recovery and reuse of carbon and nutrients in the Baltic and boreo-temperate regions: a systematic map. Environmental Evidence, 8(1), 1-18.')),
                                tags$a(href="https://iopscience.iop.org/article/10.1088/1748-9326/aba4c7/meta", tags$li('Badullovich, N., Grant, W. J., & Colvin, R. M. (2020). Framing climate change for effective communication: a systematic map. Environmental Research Letters, 15(12), 123002.')),
                                tags$a(href="https://iopscience.iop.org/article/10.1088/1748-9326/ab9d00/meta", tags$li('Hunter, N. B., North, M. A., Roberts, D. C., & Slotow, R. (2020). A systematic map of responses to climate impacts in urban Africa. Environmental Research Letters, 15(10), 103005.')),
                                tags$a(href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-020-00207-7", tags$li('Johannesdottir, S. L., Macura, B., McConville, J., Lorick, D., Haddaway, N. R., Karczmarczyk, A., ... & Osuch, P. (2020). What evidence exists on ecotechnologies for recycling carbon and nutrients from domestic wastewater? A systematic map. Environmental Evidence, 9(1), 1-14.')),
                                tags$a(href="https://www.sciencedirect.com/science/article/pii/S0160412021000210", tags$li('Eales, J., Bethel, A., Fullam, J., Olmesdahl, S., Wulandari, P., & Garside, R. (2021). What is the evidence documenting the effects of marine or coastal nature conservation or natural resource management activities on human well-being in South East Asia? A systematic map. Environment International, 151, 106397.')),
                                tags$a(href="https://pure.ulster.ac.uk/ws/files/91770171/Final_published_version.pdf", tags$li('Keenan, C., Noone, C., Mc Connell, K., & Cheng, S. (2021). A rapid response to the COVID-19 outbreak: the meta-evidence project. European Association for Health Information and Libraries, 17(2), 16-20.')),
                                tags$a(href="https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13554", tags$li('Kim, B., Moran, N. P., Reinhold, K., & Sánchez‐Tójar, A. (2021). Male size and reproductive performance in three species of livebearing fishes (Gambusia spp.): A systematic review and meta‐analysis. Journal of Animal Ecology, 90(10), 2431-2445.')),
                                tags$a(href="https://www.sciencedirect.com/science/article/pii/S0959652620337628", tags$li('Reisch, L. A., Sunstein, C. R., Andor, M. A., Doebbe, F. C., Meier, J., & Haddaway, N. R. (2021). Mitigating climate change via food consumption and food waste: A systematic map of behavioral interventions. Journal of Cleaner Production, 279, 123717.')),
                                tags$a(href="https://www.sciencedirect.com/science/article/pii/S0160412020322303", tags$li('Short, R. E., Cox, D. T., Tan, Y. L., Bethel, A., Eales, J. F., & Garside, R. (2021). Review of the evidence for oceans and human health relationships in Europe: a systematic map. Environment International, 146, 106275.')),
                                tags$a(href="https://link.springer.com/article/10.1007/s43621-021-00059-2", tags$li('Vanhuyse, F., Haddaway, N. R., & Henrysson, M. (2021). Circular cities: an evidence map of research between 2010 and 2020. Discover Sustainability, 2(1), 1-17.')),
                                tags$a(href="https://besjournals.onlinelibrary.wiley.com/doi/full/10.1002/pan3.10317", tags$li('Hinson, C., O’Keeffe, J., Mijic, A., Bryden, J., Van Grootveld, J., & Collins, A. M. (2022). Using natural capital and ecosystem services to facilitate participatory environmental decision making: Results from a systematic map. People and Nature.')),
                                tags$a(href="https://www.sciencedirect.com/science/article/pii/S019745722200057X", tags$li('Rommerskirch-Manietta, M., Purwins, D., Van Haitsma, K., Abbott, K. M., & Roes, M. (2022). Instruments for assessing the preferences for everyday living of older people with various care needs across different care settings: an evidence map. Geriatric Nursing, 45, 18-28.')),
                                tags$a(href="https://link.springer.com/article/10.1186/s13750-022-00262-2", tags$li('Stanton, I. C., Bethel, A., Leonard, A. F. C., Gaze, W. H., & Garside, R. (2022). Existing evidence on antibiotic resistance exposure and transmission to humans from the environment: a systematic map. Environmental evidence, 11(1), 1-24.')))
                       )))
              ))
  )
  )

shinyUI(
  dashboardPage(
    dashboardHeader(title = "EviAtlas"),
    sidebar,
    body
  ))
