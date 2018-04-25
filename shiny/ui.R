## ui.R ##
library(shiny)
library(shinydashboard)
library(ggvis)


  sidebar <- dashboardSidebar(

      sidebarUserPanel("", image = "http://www.pd4pic.com/images/building-flat-cartoon-trees-windows-doors-tall.png"),
      sidebarMenu(
        menuItem("About EviAtlas", tabName = "about", icon = icon("question")),
        menuItem("Bar Plots", tabName = "city", icon = icon("home")),
        menuItem("Study Map", tabName = "home", icon = icon("map")),
        menuItem("Heatmap", tabName = "heatmap", icon = icon("fire")),
        menuItem("View Data", tabName = "data", icon = icon("database")),
        menuItem("View Uploaded Data", tabName = "updata", icon = icon("database"))
        )
    )

  home <- tags$html(
    tags$head(
      tags$title('EviAtlas')
    ),
    tags$body(
      leafletOutput("map", width = "100%", height = "750"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 100, right = 25, bottom = "auto", left = "auto",
                    width = 250, height = "auto", cursor = "auto",
                    h3(strong('EviAtlas')),
                    p('Welcome to EviAtlas! See where studies have taken place.')
                    )
      )
      )

  body <- dashboardBody(
      tabItems(
        tabItem(tabName = "home",
                fluidRow(box(width = 15,
                             home))),
        tabItem(tabName = "data",
                # fluidRow(box(p(uiOutput("CLCLCL")))),
                fluidRow(
                    column(width = 2,
                           tabBox(width = NULL,
                                  tabPanel(h5("Filter"),
                                   checkboxGroupInput('filter_table_countries', 'Countries to Display:',
                                                      levels(pilotdata$Country), selected = levels(pilotdata$Country)))),
                          tabBox(width=8)),
                    column(width = 10,
                      wellPanel(dataTableOutput("table"))))),
        tabItem(tabName = "updata",
                fluidRow(
                    column(width = 2,
                           tabBox(width = NULL,
                                  tabPanel(h5("Filter"),
                                   checkboxGroupInput('filter_table_countries', 'Countries to Display:',
                                                      levels(pilotdata$Country), selected = levels(pilotdata$Country)))),
                          tabBox(width=8)),
                    column(width = 10,
                      wellPanel(dataTableOutput("table"))))),

        tabItem(tabName = "heatmap",
                fluidRow(box(selectInput("heat_select_x", label = h3("Select box"),
                                          choices = colnames(pilotdata),
                                          selected = colnames(pilotdata)[1]),
                              fluidRow(column(3, verbatimTextOutput("heat_x_axis")))),
                fluidRow(box(selectInput("heat_select_y", label = h3("Select box"),
                                         choices = colnames(pilotdata),
                                         selected = colnames(pilotdata)[2]),
                             fluidRow(column(3, verbatimTextOutput("heat_y_axis")))))),
                wellPanel(plotOutput("heatmap"))),

        tabItem(tabName = "about",
                fluidRow(fileInput("file", label = h3("File input: Upload CSV File"))),
                hr(),
                fluidRow(column(4, verbatimTextOutput("filevalue"))),
                fluidRow(height = '100%',
                         h1('Systematic Map Criteria'),
                         h4('Systematic Maps are overviews of the quantity and quality of evidence in relation to a broad (open) question of policy or management relevance. The process and rigour of the mapping exercise is the same as for systematic review except that no evidence synthesis is attempted to seek an answer to the question. A critical appraisal of the quality of the evidence is strongly encouraged but may be limited to a subset or sample of papers when the quantity of articles is very large (and even be absent in exceptional circumstances). Authors should note that all systematic maps published in Environmental Evidence will have been conducted according to the CEE process. Please contact the Editors at an early stage of planning your review. More guidance can be found here.'),
                         br(),
                         h4('For systematic maps to be relevant to policy and practice they need to be as up-to-date as possible. Consequently, at the time of acceptance for publication, the search must be less than two years old. We therefore recommend that systematic maps should be submitted no later than 18 months after the search was conducted.'),
                         br(),
                         h4('We will consider publication of updates of existing systematic maps, typically from three years since the original search, but earlier if the development of the evidence base justifies the update.'),
                         br(),
                         h4('Length up to 20000 words.'))),

        tabItem(tabName = "city",
                fluidRow(height = "20%",
                  box(width = 9)),
                fluidRow(height = "40%",
                         box(width = 6,
                             plotOutput("plot1")),
                         box(width = 6,
                             plotOutput("plot2"))),
                fluidRow(height = "40%",
                         box(width = 6,
                             plotOutput("plot3")),
                         box(width = 6,
                             plotOutput("plot4")))
                )
        ))

  shinyUI(
    dashboardPage(
      dashboardHeader(title = "EviAtlas"),
      sidebar,
      body
    ))
