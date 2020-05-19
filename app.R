library(shiny)
library(DramaAnalysis)

avaliableDramas <- list("Emilia Galotti" = "rksp.0", "Miß Sara Sampson" = "rjmw.0")

ui <- fluidPage(
  
  # App title ----
  titlePanel("DramaAnalysisWeb"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId="dramaID", "Text auswählen:", choices=avaliableDramas)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Utterance Quantity", plotOutput("quant"), sliderInput("maxNCh", "Only show top n% of characters (min. 2):",
                                                                        min = 0, max = 100,
                                                                        value = 100, step=1),),
        tabPanel("Utterance Distribution", plotOutput("dist")),
        tabPanel("Character Presence", plotOutput("presence")),
        tabPanel("Copresence", plotOutput("copresence"))
      )
    )
  )
)


server <- function(input, output) {
  dramas <- loadDrama(avaliableDramas, defaultCollection ="test")
  
  # get the selected Drama
  thisDrama <- reactive({
    drama <- list("text" = dramas$text[dramas$text$drama == input$dramaID],
                  "meta" = dramas$meta[dramas$meta$drama == input$dramaID],
                  "segments" = dramas$segments[dramas$segments$drama == input$dramaID],
                  "mentions" = dramas$mentions[dramas$mentions$drama == input$dramaID],
                  "characters" = dramas$characters[dramas$characters$drama == input$dramaID],
                  "stageDirections" = dramas$stageDirections[dramas$stageDirections$drama == input$dramaID])
    class(drama) <- c("QDDrama", "list")
    drama
  }) 
  
  # create utterance quantity stats
  charStats <- reactive({
    stats <- characterNames(characterStatistics(thisDrama()), thisDrama())
    stats <- stats[order(-stats$tokens),]
  })
  
  topNStats <- reactive({
    percentage <- input$maxNCh / 100
    firstN <- ceiling(length(charStats()) * percentage)
    if (firstN < 2){firstN <- 2}
    head(charStats(), firstN)
  })

  # plot utterance quantity as a bar plot
  output$quant <- renderPlot(barplot(topNStats(), main=dramaNames(thisDrama())), width=500)
  
  #create utterance distribution stats
  
  uttStats <- reactive({
    stats <- utteranceStatistics(thisDrama())
    stats <- characterNames(stats, thisDrama())
  })
  
  # plot utterance distribution
  output$dist <- renderPlot(plot(uttStats(), thisDrama(), main=dramaNames(thisDrama())), width=500)
  
  
}

shinyApp(ui, server)

