library(shiny)
library(ggplot2)

globalData <- read.csv("GLB.Ts+dSST.csv", skip = 1)
northData <- read.csv("NH.Ts+dSST.csv", skip = 1)
southData <- read.csv("SH.Ts+dSST.csv", skip = 1)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(title = h3("GISS Surface Temperature Analysis", align="center")),
   br(), br(),
   sidebarLayout(
      sidebarPanel(
            radioButtons("poleInput",
                         label = "Select Data: ",
                         choices = list("North Pole Data" = 'NHem',
                                        "South Pole Data" = 'SHem',
                                        "Global Data" = 'Glob'),
                         selected = 'Glob'),
            br(), br(),
            sliderInput("YearRange",
                        label = "Select Year Range: ",
                        min = 1880, max = 2018, value = c(1880, 1900),
                        step = 1),
            br(),
            selectInput("var", "Select Variable from dataset: ",
                        choices = c("Jan"=2, "Feb"=3, "Mar"=4, "Apr"=5, "May"=6,
                                    "Jun"=7, "Jul"=8, "Aug"=9, "Sep"=10, 
                                    "Oct"=11,"Nov"=12, "Dec"=13),
                        multiple = TRUE, selected = 2),
            br(), br()
      ),
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type = "tab",
                     tabPanel("Summary", verbatimTextOutput("summary")),
                     tabPanel("Data", tableOutput("data")),
                     tabPanel("Plot", plotOutput("plot"))
                     )
      )
   )
)

server <- function(input, output) {
    cols <- reactive({
        as.numeric(c(input$var))
    })
    mylabel <- reactive({
        if(input$poleInput == 'Glob') {
            label <- "Plot for Global Data"
        }
        if(input$poleInput == 'NHem') {
            label <- "Plot for North Pole Data"
        }
        if(input$poleInput == 'SHem') {
            label <- "Plot for South Pole Data"
        }
        label
    })
    myFinalData <- reactive({
        if(input$poleInput == 'Glob') {
            mydata <- globalData
        }
        if(input$poleInput == 'NHem') {
            mydata <- northData
        }
        if(input$poleInput == 'SHem') {
            mydata <- southData
        }
        mydata1 <- mydata[mydata$Year >= input$YearRange[1], ] 
        mydata1 <- mydata1[mydata1$Year <= input$YearRange[2], ]
        
        mydata2<- mydata1[, c(1, sort(cols()))]

        data.frame(mydata2)
        
    })
    
    output$data <- renderTable({
        myFinalData()
    })
    
    rendersmry <- reactive({summary(myFinalData())})
    
    output$summary <- renderPrint({
        rendersmry()
    })
    
    output$plot <- renderPlot({
        plotdata <- myFinalData()
        plot(plotdata, col=c(1,2,3,4,5,6,7,8,9), main=mylabel())
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

