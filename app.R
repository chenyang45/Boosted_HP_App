#=================================================================
# build App for Boosted HP
#-----------------------------------------------------------------
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#-----------------------------------------------------------------
# By: Chen Yang (chen_yang@link.cuhk.edu.hk)
# Date: 2019-05-20
# Update: 2019-07-01
#=================================================================

library(shiny)
library(tseries)
library(expm)
library(xts)

source("BoostedHP.R")
source("plot_all.R")

load("data/IRE.rda")

# User interface ----

ui <- fluidPage(
  titlePanel("Boosted HP App"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # --------------- Define UI for data upload app ------------------
      
      h2("Uploading Files"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # --------------- Define UI for data download app ------------------
      # Download Button
      
      h2("Downloading Files "),
      downloadButton("downloadData", "Download"),
      helpText("Download Files After the Boosted HP Filter in the default 'Download Document' in your computer."),
      
      # Horizontal line ----
      tags$hr(),
      img(src = "shiny from rstudio.png", height = 70, width = 100),
      br(),
      span("Shiny", style = "color:blue"),
      " is a product of RStudio", 
      helpText("Create user friendly app through 'Shiny'."),
      p("For more information of the APP, visit the Boosted_HP_App",
        a(" Github Repository.", 
          href = "https://github.com/chenyang45/Boosted_HP_App"))
      
      
      
      
      
      ),
    
     
  
    
    # Main panel for displaying outputs ----
    mainPanel(
      
     # tags$hr(),
      
      
      textOutput("currentTime"),
      
      # --------------- Define UI for argument in BoostedHP.R ------------------
      #column(5,
      h2("Argument Options"),
      
      #h3("Argument in the Boosted Function"),
      #checkboxInput("checkbox", "Choice A", value = TRUE),
      
      numericInput("lambda", label = "Set the lambda for HP filter",value = 1600),
      radioButtons("iter", label = "Iterated",
                   choices = list("Ture" = "TURE", "False" = "FALSE"),selected = "TURE"),
      selectInput("testtype", 
                  label = "Choose the test type to display",
                  choices = list("HP once", 
                                 "ADF",
                                 "BIC"),
                  selected = "none"),
      selectInput("pvalue", 
                  label = "Choose the p-value of significance",
                  choices = list("0.010", 
                                 "0.050",
                                 "0.100"),
                  selected = "0.050"),
      numericInput("maxnum", label = "Maximum Iterated Number",value = 100),
      
      #),
      
      #column(5,
      #h3("Single checkbox")    
      #),
      
      # Output: Data file ----
      #column(3,
      #h2("View the Input Data"),
      #tableOutput("contents"), 
      #h2("Summary"),
      #verbatimTextOutput("summary"),
      #),
      
      # Horizontal line ----
      tags$hr(),
      h2("Plot"),
      selectInput("Type", 
                  label = "Choose the Type of Plot",
                  choices = list("Time Series", 
                                 "Plain",
                                 "SVG",
                                 "GGPlot",
                                 "Dynamic"),
                  selected = "Time Series"),
      dateInput("date", 
                h3("End Date of Input Data (valid only for ts plot)"), 
                value = "2014-01-01"),
      
      #plotOutput("Boostedplot"),
      tags$hr(),
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Input Data", tableOutput("contents")),
                  tabPanel("Plot", plotOutput("Boostedplot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Results Table", tableOutput("table")))
    
      
    )
      
    )
)









# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  #req(input$file1)
  
  
  
  output$Boostedplot <- renderPlot({
    
    req(input$file1)
    rawdata <- read.csv(input$file1$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
    
    arg <- rawdata
    
    arg$lambda <- input$lambda
    
    arg$test_type <- switch(input$testtype, 
                            "HP once" = "none",
                            "ADF" = "adf",
                            "BIC" = "BIC")
    
    arg$sig_p <- input$p_value
    
    arg$Max_Iter <- input$maxnum
    
    bt_results <- do.call(BoostedHP, args)
    #function(rawdata, trend, p_history, plot_type){
    Boostedplot(rawdata, bt_results[[2]], bt_results[[4]], input$Type )
    
    
    
    
    
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    req(input$file1)
    
    #dataset <- datasetInput()
    summary(input$file1)
  })
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  output$table <- renderTable({
    bt_results <- do.call(BoostedHP, args)
    summary(bt_results[[2]])
    
  }
    
  )
    
    
  
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)


