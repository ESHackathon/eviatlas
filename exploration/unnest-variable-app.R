

library(tidyverse)
library(shiny)



trees <- tibble(
  species = c("birch; poplar",
              "oak; birch; pine",
              "",
              "poplar",
              "oak"),
  habitats = c(
    "wetlands",
    "forest",
    "forest; wetlands",
    "grasslands",
    "grasslands; forest"
  ),
  id = letters[1:5]
)

user_data <- trees # placeholder

long_table <- trees

ui <- fluidPage(
  selectInput(
    inputId = "variable",
    label = "select a nested variable",
    choices = objects(user_data)
  ),
  textInput(inputId = "longform_var", "Name the longform variable", "newvar"),
  textInput(inputId = "sep_symb", "How are the values separated?", ";"),
  tableOutput("long_data"),
  actionButton(inputId = "tidy", label = "tidy!")


)

server <- function(input, output) {
  long_data <- eventReactive(input$tidy, {
    get_long(user_data,
             input$variable,
             input$longform_var,
             input$sep_symb)
  })

  output$long_data <- renderTable({
    long_data()
  })

}
shinyApp(ui = ui, server = server)
