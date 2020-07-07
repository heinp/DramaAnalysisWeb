library(shiny)
library(DramaAnalysis)

tryCatch(installData(dataSource="qd"), error = function(e) {})

ids <- loadAllInstalledIds()
ids <- ids[which(grepl("qd:", ids))]
metaData <- loadMeta(ids)
#avaliableDramas <- as.character(metaData$drama)
#names(avaliableDramas) <- lapply(avaliableDramas, function(x) dramaNames(x))

avaliableDramas <- setNames(as.character(metaData$drama), paste(metaData$Name, ": ", metaData$documentTitle, " (", metaData$Date.Written, ")", sep=""))



ui <- fluidPage(
  
  # App title ----
  titlePanel("DramaAnalysisWeb"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectizeInput(inputId="dramaID", "Select a drama:", choices=avaliableDramas, selected=NULL, multiple=FALSE, options=list(create=FALSE, placeholder="Select a drama", onInitialize = I('function() { this.setValue(""); }'))),
      hr(),
      uiOutput("intSlider"),
      hr(),
      checkboxInput(inputId="expertSettings", label="show export settings", value=FALSE),
      conditionalPanel(condition="input.expertSettings",
                       numericInput(inputId="sizeX", label="size (px)", value= 500, min = 50, max = 1000, step = 50,
                                    width = NULL),
                       numericInput(inputId="resolution", label="resolution (dpi)", value=150, min=50, max=500, step=10),
                       selectInput(inputId="filetype", label="filetype", choices = c("png", "pdf")),
                       selectInput(inputId="title", label="title", choices=list("drama title","metric", "nothing", "custom"), selected="drama title"),
                       conditionalPanel(condition="input.title=='custom'",
                                        textInput("customTitle", label=NULL, placeholder="your custom title")),
                       downloadButton('downloadPlot', 'Download Plot'))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel( id="tabs",
        tabPanel("Utterance Quantity", plotOutput("quant")),
        tabPanel("Utterance Distribution", plotOutput("dist", width=500)),
        tabPanel("Character Presence", plotOutput("presence", width=500)),
        tabPanel("Copresence", plotOutput("copresence", width=500, height=500))
      )
    )
  )
)



server <- function(input, output) {
  # get the selected Drama
  thisDrama <- reactive({
    validate(
      need(input$dramaID != "", "Please select drama")
    )

    loadDrama(input$dramaID)
  })
  
  # create slider for choice of top N
  startVal <- reactive({
    if (nrow(charStats()) > 10) val <- 10
    else val <- nrow(charStats())
    val
  })
  output$intSlider <- renderUI(sliderInput("topN", "Only show top N characters:", min=2, max=nrow(charStats()), value=startVal(), step=1))
  
  # get different versions of titles
  title <- reactive({
    if (input$title == "drama title") t <- dramaNames(thisDrama())
    else if (input$title == "metric") t <- input$tabs
    else if (input$title == "custom") t <- input$customTitle
    else t <- ""
  })
  
  # download button
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(thisDrama(), '_', tolower(chartr(old=" ", new="_", input$tabs)), input$filetype, sep='') },
    content = function(file) {
      ggsave(file, plot = plots[[input$tabs]], device = input$filetype)
    }
  )
  
  # define cacheKey
  cacheKey <- reactive({
    c(input$dramaID, input$topN, input$title) 
  })
  
  # create sorted charecter stats (for utterance quantity and for topN)
  charStats <- reactive({
    stats <- characterNames(characterStatistics(thisDrama()), thisDrama())
    stats <- stats[order(-stats$tokens),]
    stats
  })
  
  # get topN figures
  topNFigs <- reactive({
    tryCatch(head(charStats()$character, input$topN), error=function(e) charStats()$character)
  })
  
  
  topNCharStats <- reactive({
    charStats()[charStats()$character %in% topNFigs(),]
  })
  
  size <- reactive(input$sizeX)

  # plot utterance quantity as a bar plot
  output$quant <- renderPlot(barplot(topNCharStats(), main=title(), xaxt="n"), width=500)
  
  #create utterance distribution stats
  
  uttStats <- reactive({
    stats <- utteranceStatistics(thisDrama())
    stats <- characterNames(stats, thisDrama())
    stats
  })
  
  topNUttStats <- reactive({
    uttStats <- uttStats()[uttStats()$character %in% topNFigs(), ]
    uttStats$character <- factor(uttStats$character, levels=unique(uttStats$character))
    uttStats
  })
  
  # plot utterance distribution
  uttDistPlot <- reactive({
    par(mar=c(2,9,2,2))
    plot(topNUttStats(), thisDrama(), main=title())
  })
  
  output$dist <- renderCachedPlot(uttDistPlot(),  cacheKey())
  
  
  # create copresence stats
  co <- reactive({
    co <- configuration(thisDrama(), onlyPresence = TRUE, segment="Scene")
    co <- filterCharacters(co, thisDrama(), n=input$topN)
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
    par(mar=c(10,10,3,3)) # plot margins
    image(copresence(),
          col = rgb(256,111,184, alpha=(seq(0,255)),
                    maxColorValue = 256),
          xaxt= "n",  # no x axis
          yaxt= "n",  # no y axis
          frame=TRUE,  # print a frame around the heatmap
          main=title())

    # include values as labels
    text(y=(rep(1:ncol(copresence()), each=nrow(copresence()))-1)/(nrow(copresence())-1),
         x=(1:nrow(copresence())-1)/(nrow(copresence())-1),
         labels=as.vector(copresence()))

    # add the x axis
    axis(1, at = seq(0,1,length.out = length(co()$character)), labels = co()$character, las=3)
    # add the y axis
    axis(2, at = seq(0,1,length.out = length(co()$character)), labels = co()$character, las=1)
  })
  output$copresence <- renderCachedPlot(heat(),  cacheKey())
  
  
  # create character presence stats
  pres <- reactive({
    characterNames(presence(thisDrama()), thisDrama())
  })
  
  # plot character presence stats
  presplot <- reactive({
    pres <- pres()[pres()$character %in% topNFigs(), ]
    plot(x=pres$active/pres$scenes, 
         y=pres$passive/pres$scenes, 
         xlim=c(0,1), 
         ylim=c(0,1), 
         xlab="Active", 
         ylab="Passive",
         main=title())
    text(x=pres$actives/pres$scenes, 
         y=pres$passives/pres$scenes, 
         labels=substr(pres$character,0,20), 
         pos=3,
         cex=0.8)
    lines(x=seq(0,0.5,0.1),seq(0,0.5,0.1), lty=3)
    lines(x=1:0,y=0:1, lty=2)
  })
  
  output$presence <- renderCachedPlot(presplot(), cacheKey())
  
  # lookup for plotfunction by tab name
  plots <- reactive({
    list("Utterance Quantity"=function(){barplot(topNCharStats(), main=title(), xaxt="n")},
         "Utterance Distribution"=uttDistPlot(),
         "Character Presence"=presplot(),
         "Copresence"=heat())
  })
  
}

shinyApp(ui, server)

