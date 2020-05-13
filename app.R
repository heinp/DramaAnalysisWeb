library(shiny)
library(DramaAnalysis)
library(hash)

ui <- fluidPage(
  
  # App title ----
  titlePanel("DramaAnalysisWeb"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId="text", "Text auswählen:", choices=list("Emilia Galotti" = "rksp.0", "Miß Sara Sampson" = "rjmw.0"))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "plot")
    )
  )
)


server <- function(input, output) {
  # loads a specific, pre-packaged play
  data(rksp.0)
  data(rjmw.0)
  charStatsEmil <- characterStatistics(rksp.0)
  charStatsSara <- characterStatistics(rjmw.0)
  charStatsEmil <- characterNames(charStatsEmil, rksp.0)
  charStatsSara <- characterNames(charStatsSara, rjmw.0)
  dataDict = hash("rksp.0"=charStatsEmil, "rjmw.0"=charStatsSara)
  
  # plot them as a bar plot
  output$plot <- renderPlot(barplot(dataDict[[input$text]], main=input$text))
}

shinyApp(ui, server)

