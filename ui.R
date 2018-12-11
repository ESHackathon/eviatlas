## ui.R ##

sidebar <- dashboardSidebar(

  # sidebarUserPanel("EviAtlas Nav"),
  sidebarMenu(
      menuItem("About EviAtlas", tabName = "about", icon = icon("question")),
      menuItem("View Data", tabName = "data", icon = icon("database")),
      menuItem("Bar Plots", tabName = "insightplots", icon = icon("home")),
      menuItem("Study Map", tabName = "home", icon = icon("map")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("fire"))
      )
)

home <- tags$html(
  tags$head(
    tags$title('EviAtlas')
  ),
  tags$style(type = "text/css", "#map {height: calc(100vh - 180px) !important;}"),
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
      mainPanel(
        tableOutput("start_text"),
        tableOutput("data_summary")
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

              fileInput(
                "sysmapdata_upload",
                label = "Choose CSV File",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"),
                placeholder = "Systematic Map Data"
              ),

              radioButtons(
                "upload_encoding",
                label = "Select File Encoding",
                choices = list("Default"="", "UTF-8", "latin1"),
                selected = ""
              ),

              # Input: Checkbox if file has header ----
              checkboxInput("header", "Does file have header row?", TRUE),

              # Input: Select separator ----
              radioButtons(
                "sep",
                "Separator",
                choices = c(
                  Comma = ",",
                  Semicolon = ";",
                  Tab = "\t"
                ),
                selected = ","
              ),

              # Input: Select quotes ----
              radioButtons(
                "quote",
                "Quote",
                choices = c(
                  None = "",
                  "Double Quote" = '"',
                  "Single Quote" = "'"
                ),
                selected = '"'
              )
          ))
      ))
    ),

    tabItem(tabName = "home",
      fluidRow(
        uiOutput("map_columns")
      ),
      fluidRow(
        box(width = 15, home)
      )
    ),

    tabItem(
      tabName = "data",
      fluidRow(
        column(width = 3,
          wellPanel(uiOutput("filter_selector"), 
          uiOutput("go_button"))
        )
      ),
      fluidRow(
      ),

      fluidRow(
        column(12,
               DT::dataTableOutput("filtered_table", height="80%")
               )
        )
      ),
    tabItem(tabName = "insightplots",
            tabsetPanel(
              tabPanel('Plot Inputs',
                       fluidRow(
                         column(3, uiOutput("barplot_selector")),
                         column(3, uiOutput("location_plot_selector"))
                       )),
              tabPanel('Save Plots',
                       fluidRow(
                         column(2, downloadButton("save_plot_1_button")),
                         column(2, downloadButton("save_plot_2_button"))
                       ))
            ),
            wellPanel(
              plotOutput("plot1")
            ),
            wellPanel(
              plotOutput("plot2")
            )
    ),
    tabItem(tabName = "heatmap",
      fluidRow(
        uiOutput("heatmap_selector")
      ),
      fluidRow(
        wellPanel(
          plotOutput("heatmap")
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
