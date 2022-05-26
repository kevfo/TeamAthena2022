library(shiny) 

# Be sure to change the working directory if need be:
#setwd("C:/Users/Kevin/Desktop/Projects/R/TeamAthena2022/analysis/")
#source('model.R')

choices <- c('yes', 'no')
alLevels <- 0:5

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Quick Check", br(),
             sidebarLayout(
               sidebarPanel(
                 numericInput('sc', 'Serum Creatinine (sc) Level (mgs / dl)', 0, min = 0),
                 numericInput('sg', 'Specific Gravity (sg)', 0, min = 0),
                 numericInput('pot', 'Potassium (al) Level (mEg / L)', 0, min = 0),
                 numericInput('bgr', 'Blood Glucose Ramdom (bgr - mgs / dl)', 0, min = 0),
                 numericInput('sod', 'Sodium (sod) Level (mEg / L)', 0, min = 0),
                 numericInput('bu', 'Blood Urea (bu) Level (mgs / dl)', 0, min = 0),
                 selectInput('dm', 'Has Diabetes Mellitus (dm)?', choices),
                 selectInput('pe', 'Has Pedal Edema (pe)?', choices),
                 numericInput("bp", "Blood Pressure (mm / Hg)", 0, min = 0),
                 selectInput('htn', 'Has Hypertension (htn)?', choices),
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
        sg = input$sg, sc = input$sc, sod = input$sod, bgr = input$bgr,
        bu = input$bu, pot = input$pot, htn = input$htn, dm = input$dm,
        bp = input$bp, pe = input$pe
      )
      df$htn <- as.factor(df$htn) ; df$dm <- as.factor(df$dm)
      df$pe <- as.factor(df$pe)
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
                                                     sod = NA, bu = NA, bgr = NA))}
    dataToReturn$df
  })
  
  output$status <- renderText({
    if (is.null(dataToReturn$df)) {return("Enter some data to get started!")}
    pStatus <- predict(rfWithVip, dataToReturn$df) %>% as.character()
    if (pStatus == 'Diseased') {
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
    dataToCompute$df$dm <- as.factor(dataToCompute$df$dm)
    dataToCompute$df$htn <- as.factor(dataToCompute$df$htn)
    dataToCompute$df$pe <- as.factor(dataToCompute$df$pe)
    dataToCompute$allPred <- predict(rfWithVip, dataToCompute$df)
  })
  
  output$testme <- renderDataTable({
    if (is.null(dataToCompute$df)) {return(data.frame(`Subject Number` = NA, Predictions = NA))}
    data.frame(`Subject Number` = 1:length(dataToCompute$allPred), 
                    Predictions = dataToCompute$allPred)
  }, options = list(pagelength = 5))
}

# shinyApp(ui, server)