## ui.R ##
library(shiny)
library(shinydashboard)
library(ggvis)
library(DT)
library(leaflet)


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
                  label = h4("Select File Encoding"),
                  choices = list("utf-8", "latin1"),
                  selected = "utf-8"
                ),

                # Input: Checkbox if file has header ----
                checkboxInput("header", "Check if file has header", TRUE),

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
              )
            )
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
        column(
          width = 4,
          uiOutput("filter_selector"),
          uiOutput("go_button")
        ),
        column(
          width = 8,
          uiOutput("value_selector"),
          tableOutput("test_values")
        )
      ),
      fluidRow(
        wellPanel(
          dataTableOutput("filtered_table")
        )
      )
    ),
    tabItem(tabName = "insightplots",
            fluidRow(box(selectInput("select_x1", label = h3("Select variable"),
                                     choices = colnames(pilotdata), # these bits do not work for external uploaded files
                                     selected = colnames(pilotdata)[8]))), # these bits do not work for external uploaded files
            wellPanel(plotOutput("plot1"))),

    tabItem(tabName = "heatmap",
            fluidRow(box(selectInput("heat_select_x", label = h3("Select variable"),
                                     choices = colnames(pilotdata),
                                     selected = colnames(pilotdata)[3]),
                         fluidRow(column(3, verbatimTextOutput("heat_x_axis")))),
                     fluidRow(box(selectInput("heat_select_y", label = h3("Select variable"),
                                              choices = colnames(pilotdata), # these bits do not work for external uploaded files
                                              selected = colnames(pilotdata)[8]), # these bits do not work for external uploaded files
                                  fluidRow(column(3, verbatimTextOutput("heat_y_axis")))))),
            wellPanel(plotOutput("heatmap")))
  ))

shinyUI(
  dashboardPage(
    dashboardHeader(title = "EviAtlas"),
    sidebar,
    body
  ))
