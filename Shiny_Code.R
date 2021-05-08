  #####################################################################################################
  # Rshiny App
  
  library(shiny)
  library(fmsb)
  library(shinydashboard)
  library(shinythemes)
  
  
  ui <- fluidPage(
    theme = shinytheme("journal"),
    titlePanel("Credit Card Fraud Detection"),
    windowTitle = ("Credit Card Fraud Detection"),
  
    sidebarLayout(
      sidebarPanel(h2("Enter inputs below"),
                   
                   numericInput(inputId = "Time", 
                                label = "Time", 
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V1", 
                                label = "V1", 
                                value = 0, min = -10, max = 10), 
                   numericInput(inputId = "V2", 
                                label = "V2", 
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V3", 
                                label = "V3",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V4", 
                                label = "V4",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V5", 
                                label = "V5",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V6", 
                                label = "V6",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V7", 
                                label = "V7",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V8", 
                                label = "V8",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V9", 
                                label = "V9",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V10", 
                                label = "V10",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V11", 
                                label = "V11",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V12", 
                                label = "V12",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V13", 
                                label = "V13",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V14", 
                                label = "V14",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V15", 
                                label = "V15",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V15", 
                                label = "V15",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V16", 
                                label = "V16",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V17", 
                                label = "V17",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V18", 
                                label = "V18",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V19", 
                                label = "V19",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V20", 
                                label = "V20",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V21", 
                                label = "V21",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V22", 
                                label = "V22",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V23", 
                                label = "V23",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V24", 
                                label = "V24",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V25", 
                                label = "V25",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V26", 
                                label = "V26",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V27", 
                                label = "V27",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "V28", 
                                label = "V28",
                                value = 0, min = -10, max = 10),
                   numericInput(inputId = "Amount", 
                                label = "Amount",
                                value = 0, min = -10, max = 10),
                   actionButton("update", "Get Predictions")
      ),
      mainPanel(tags$img(src = "https://img.etimg.com/thumb/msid-76173969,width-650,imgsize-1099506,,resizemode-4,quality-100/twitterati-began-following-longstanding-anonymous-posters-and-retweeting-them-.jpg", height = 240, width = 400),
        tabsetPanel(
          tabPanel("Prediction", 
                   fluidRow(
                     h4("The probability that the given transaction is fraudulent is:"),
                     tableOutput("Pred")),
          )
        )
      )
    ),
  )
  
  server <- function(input, output) {
  
    values <- reactiveValues()
    values$df <- cc_bal[,1:30]
    observe({
      if(input$update > 0) {
        newLine <- isolate(c(input$Time,input$V1,input$V2,input$V3,input$V4,input$V5,
                             input$V6,input$V7,input$V8,input$V9,input$V10,
                             input$V11,input$V12,input$V13,input$V14,input$V15,
                             input$V16,input$V17,input$V18,input$V19,input$V20,
                             input$V21,input$V22,input$V23,
                             input$V24,input$V25,input$V26,input$V27, 
                             input$V28, input$Amount))
        
        isolate(values$df <- rbind(as.matrix(values$df), unlist(newLine)))
        
        d <- as.data.frame({values$df})
        str(d)
     
        source("predictorf.R")
        
        p <- predictorf(d)
        
        output$Pred <- renderTable(tail(p,1), include.rownames=F)
      }
    })
    
  }
  
  shinyApp(ui = ui, server = server)
  
  
  
