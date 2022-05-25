library(shiny) 

# Be sure to change the working directory if need be:
#setwd("C:/Users/Kevin/Desktop/Projects/R/TeamAthena2022/analysis/")
#source('model.R')

choices <- c('yes', 'no')

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Quick Check", br(),
             sidebarLayout(
               sidebarPanel(
                 numericInput('sc', 'Serum Creatinine (sc) Level (mgs / dl)', 0, min = 0),
                 numericInput('sg', 'Specific Gravity (sg)', 0, min = 0),
                 numericInput('al', 'Albumin Level (al)', 0, min = 0),
                 selectInput('htn', 'Has Hypertension (htn)?', choices),
                 numericInput('sod', 'Sodium (sod) Level (mEg / L)', 0, min = 0),
                 numericInput('bu', 'Blood Urea (bu) Level (mgs / dl)', 0, min = 0),
                 actionButton('submit', 'Predict Status')
               ),
               mainPanel(
                 h3("Data Entered"),
                 tableOutput('dataE1'),
                 h3("Concensus"),
                 htmlOutput('status')
               )
             )),
    tabPanel("Multiple Data", br(),
             fileInput('fileUpload', "Upload a .csv file!", multiple = T, accept = '.csv'),
             actionButton('mdSubmit', "Predict Status"), 
             br(), br(), br(), 
             dataTableOutput('testme')
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
  
  output$dataE1 <- renderTable({
    if (is.null(dataToReturn$df)) {return(data.frame(sc = NA, sg = NA, 
                                                     al = NA, htn = NA,
                                                     sod = NA, bu = NA))}
    dataToReturn$df
  })
  
  output$status <- renderText({
    if (is.null(dataToReturn$df)) {return("Enter some data to get started!")}
    pStatus <- predict(rfWithVip, dataToReturn$df) %>% as.character()
    if (pStatus == 'ckd') {
      return("<b><font color = \"red\">Patient has kidney disease</font></b>")
    } else {
      return("<b><font color = \"green\">Patient does not have kidney disease</font></b>")
    }
  })
  
  # == .csv file input == 
  
  patientData <- reactive({
    req(input$fileUpload)
    
    pData <- tools::file_ext(input$fileUpload$name)
    switch(pData, 
           csv = vroom::vroom(input$fileUpload$datapath, delim = ','),
           validate("Please only upload .csv files!"))
  })
  
  dataToCompute <- reactiveValues(data = NULL)
  observeEvent(input$mdSubmit, {
    dataToCompute$df <- patientData()
    dataToCompute$allPred <- predict(rfWithVip, dataToCompute$df)
  })
  
  output$testme <- renderDataTable({
    if (is.null(dataToCompute$df)) {return(data.frame(`Subject Number` = NA, Predictions = NA))}
    data.frame(`Subject Number` = 1:length(dataToCompute$allPred), 
                    Predictions = ifelse(dataToCompute$allPred == 'ckd', "Has Kidney Disease",
                                         "No Kidney Disease"))
  }, options = list(pagelength = 5))
}

shinyApp(ui, server)