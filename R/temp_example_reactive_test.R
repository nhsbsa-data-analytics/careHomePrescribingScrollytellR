library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Reactivity Test"),

  # Sidebar with two input widgets
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Input #1 - Dataset",
        choices = c("mtcars", "iris")
      ),
      selectInput(
        inputId = "variable",
        label = "Input #2 - Variable",
        choices = NULL
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  input_dataset <- reactive({
    if (input$dataset == "mtcars") {
      return(mtcars)
    } else {
      return(iris)
    }
  })

  observeEvent(input$dataset, {
    freezeReactiveValue(input, "variable")
    updateSelectInput(session = session, inputId = "variable", choices = names(input_dataset()))
  })

  output$distPlot <- renderPlot({
    ggplot(input_dataset(), aes(x = .data[[input$variable]])) +
      geom_histogram()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
