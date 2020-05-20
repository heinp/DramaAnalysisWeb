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
        tabPanel("Utterance Quantity", plotOutput("quant"), uiOutput("intSlider")),
        tabPanel("Utterance Distribution", plotOutput("dist", width=500)),
        tabPanel("Character Presence", plotOutput("presence", width=500)),
        tabPanel("Copresence", plotOutput("copresence", width=500, height=500))
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
    head(charStats(), as.integer(input$maxNAbs))
  })

  # plot utterance quantity as a bar plot
  output$quant <- renderPlot(barplot(topNStats(), main=dramaNames(thisDrama())), width=500)
  output$intSlider <- renderUI(sliderInput("maxNAbs", "Only show top N characters:", min=2, max=length(charStats()), value=length(charStats()), step=1))
  
  #create utterance distribution stats
  
  uttStats <- reactive({
    stats <- utteranceStatistics(thisDrama())
    stats <- characterNames(stats, thisDrama())
  })
  
  # plot utterance distribution
  uttDistPlot <- reactive({
    par(mar=c(2,9,2,2))
    plot(uttStats(), thisDrama(), main=dramaNames(thisDrama()))
  })
  
  # output$dist <- renderPlot(plot(uttStats(), thisDrama(), main=dramaNames(thisDrama())), width=500)
  output$dist <- renderCachedPlot(uttDistPlot(), {input$dramaID})
  
  
  # create copresence stats
  co <- reactive({
    co <- configuration(thisDrama(), onlyPresence = TRUE, segment="Scene")
    co <- filterCharacters(co, thisDrama(), n = 7)
    characterNames(co, thisDrama())
  })
  
  copresence <- reactive({
    # extract a matrix
    mat <- as.matrix(co())

    # multiply the matrix with its inverse
    # this creates the copresence matrix
    cop <- mat %*% t(mat)

    # add character names
    rownames(cop) <- co()$character
    colnames(cop) <- co()$character

    # since it's a square matrix, we don't need the bottom right triangle
    # and diagonales.
    cop[lower.tri(cop,diag=TRUE)] <- NA
    cop
  })

  # plot copresence
  heat <- reactive({
    par(mar=c(10,10,1,1)) # plot margins
    image(copresence(),
          col = rgb(256,111,184, alpha=(seq(0,255)),
                    maxColorValue = 256),
          xaxt= "n",  # no x axis
          yaxt= "n",  # no y axis
          frame=TRUE  # print a frame around the heatmap
    )

    # include values as labels
    text(y=(rep(1:ncol(copresence()), each=nrow(copresence()))-1)/(nrow(copresence())-1),
         x=(1:nrow(copresence())-1)/(nrow(copresence())-1),
         labels=as.vector(copresence()))

    # add the x axis
    axis(1, at = seq(0,1,length.out = length(co()$character)), labels = co()$character, las=3)
    # add the y axis
    axis(2, at = seq(0,1,length.out = length(co()$character)), labels = co()$character, las=1)
  })
  output$copresence <- renderCachedPlot(heat(), {input$dramaID})
  
  
  # create character presence stats
  pres <- reactive({
    characterNames(presence(thisDrama()), thisDrama())
  })
  
  # plot character presence stats
  presplot <- reactive({
    plot(x=pres()$active/pres()$scenes, 
         y=pres()$passive/pres()$scenes, 
         xlim=c(0,1), 
         ylim=c(0,1), 
         xlab="Active", 
         ylab="Passive",
         sub=dramaNames(thisDrama()),
         main="Character Presence")
    text(x=pres()$actives/pres()$scenes, 
         y=pres()$passives/pres()$scenes, 
         labels=substr(pres()$character,0,20), 
         pos=3,
         cex=0.8)
    lines(x=seq(0,0.5,0.1),seq(0,0.5,0.1), lty=3)
    lines(x=1:0,y=0:1, lty=2)
  })
  
  output$presence <- renderCachedPlot(presplot(), {input$dramaID})
  
}

shinyApp(ui, server)

