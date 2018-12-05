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
    tags$body(
      leafletOutput("map")
    )
  )

  body <- dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                mainPanel(
                  h4('Preview of uploaded data:'),
                  dataTableOutput("user_data")),

                #Sidebar panel for inputs
                sidebarPanel(
                  radioButtons(
                    "sample_or_real",
                    label = h4("Which Data to Use?"),
                    choices = list("Sample Data" = 'sample',
                                   "Upload User Data" = 'user'),
                    selected = "user"
                  ),

                  conditionalPanel(
                    condition = "input.sample_or_real == 'user'",

                    # Input: Select a file ----

                    fileInput(
                      "sysmapdata_upload",
                      label = "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
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
                ),
                hr(),
                fluidRow(height = '100%',
                         h2('About Systematic Maps'),
                         p('Systematic Maps are overviews of the quantity and quality of evidence in relation to a broad (open) question of policy or management relevance. The process and rigour of the mapping exercise is the same as for systematic review except that no evidence synthesis is attempted to seek an answer to the question. A critical appraisal of the quality of the evidence is strongly encouraged but may be limited to a subset or sample of papers when the quantity of articles is very large (and even be absent in exceptional circumstances). Authors should note that all systematic maps published in Environmental Evidence will have been conducted according to the CEE process. Please contact the Editors at an early stage of planning your review. More guidance can be found here.'),
                         br(),
                         p('For systematic maps to be relevant to policy and practice they need to be as up-to-date as possible. Consequently, at the time of acceptance for publication, the search must be less than two years old. We therefore recommend that systematic maps should be submitted no later than 18 months after the search was conducted.')
                         )),

        tabItem(tabName = "home",
                fluidRow(box(selectInput("map_popup_select", label = h3("Select Popup"),
                                         choices = colnames(pilotdata),
                                         selected = colnames(pilotdata)[4]))),
                fluidRow(box(width = 15, home))
                ),

        tabItem(tabName = "data",
                fluidRow(
                    column(width = 12,
                           tabBox(width = NULL,
                                  tabPanel(
                                    h5("Filter"),
                                    selectInput(
                                      "selected_variable",
                                      label = "Filter by:",
                                      choices = c("none", colnames(pilotdata)),
                                      selected = "none"
                                    ),
                                    uiOutput("value_selector"),
                                    uiOutput("go_button")
                                   # checkboxGroupInput('filter_table_countries', 'Countries to Display:',
                                                      # levels(pilotdata$Country), selected = levels(pilotdata$Country))
                                   ))
                        ),
                    column(width = 12,
                      wellPanel(
                        dataTableOutput("filtered_table")
                      )))),
       # tabItem(tabName = "insightplots",
        #        fluidRow(height = "50%",
         #                box(width = 12,
          #                   plotOutput("plot1"))),
           #     fluidRow(height = "50%",
            #             box(width = 12,
             #                plotOutput("plot2")))
              #  ),
       tabItem(tabName = "insightplots",
               fluidRow(box(selectInput("select_x1", label = h3("Select variable"),
                                        choices = colnames(pilotdata), # how do we change this to a generic dataset?
                                        selected = colnames(pilotdata)[8]))),
               wellPanel(plotOutput("plot1"))),
  tabItem(tabName = "heatmap",
                fluidRow(box(selectInput("heat_select_x", label = h3("Select variable"),
                                         choices = colnames(pilotdata),
                                         selected = colnames(pilotdata)[3]),
                             fluidRow(column(3, verbatimTextOutput("heat_x_axis")))),
                         fluidRow(box(selectInput("heat_select_y", label = h3("Select variable"),
                                                  choices = colnames(pilotdata),
                                                  selected = colnames(pilotdata)[8]),
                                      fluidRow(column(3, verbatimTextOutput("heat_y_axis")))))),
                wellPanel(plotOutput("heatmap")))
        ))

  shinyUI(
    dashboardPage(
      dashboardHeader(title = "EviAtlas"),
      sidebar,
      body
    ))
