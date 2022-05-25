library(shiny)
source("model.R")

# Be sure to change the working directory if need be:
setwd("C:/Users/Kevin/Desktop/Projects/R/TeamAthena2022/analysis/")
source('model.R')

choices <- c('yes', 'no')

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Quick Check", br(),
             sidebarLayout(
               sidebarPanel(
                 numericInput('sc', 'Serm Creatinine Level (mgs / dl)', 0, min = 0),
                 numericInput('sg', 'Specific Gravity', 0, min = 0),
                 numericInput('al', 'Albumin Level', 0, min = 0),
                 selectInput('htn', 'Has Hypertension?', choices),
                 numericInput('sod', 'Sodium Level (mEg / L)', 0, min = 0),
                 numericInput('bu', 'Blood Urea Level (mgs / dl)', 0, min = 0),
                 actionButton('submit', 'Check Status')
               ),
               mainPanel(
                 h3("Concensus"),
                 textOutput('status')
               )
             )),
    tabPanel("Multiple Data", br(),
             fileInput("file","Upload", multiple = TRUE, accept=".csv")
    )
  )
)

server <- function(input, output, session) {
  # == Helper function to make a data frame ==
  makeDF <- reactive({
    if (input$submit > 0) {
      df <- data.frame(
        sc = input$sc, sg = input$sg, al = input$al, htn = input$htn,
        sod = input$sod, bu = input$bu
      )
      df$htn <- as.factor(df$htn)
    }
    return(list(df = df))
  })
  
  dataToReturn <- reactiveValues(data = NULL)
  
  observeEvent(input$submit, {
    dataToReturn$df <- makeDF()$df
  })
  
  output$status <- renderText({
    if (is.null(dataToReturn$df)) {return()}
    predict(rfWithVip, dataToReturn$df) %>% as.character()
  })
  output$contents <- renderTable({
    file <- input$filecsv
    ext <- tools::file_ext(file$datapath)
    data2 <- read.csv(file$datapath, header = TRUE,sep=c(",",""))
    })
}
shinyApp(ui, server)
