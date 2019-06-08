#=================================================================
# build App for Boosted HP
#-----------------------------------------------------------------
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
#-----------------------------------------------------------------
# Date: 2019-05-20
#=================================================================

library(shiny)

library(tseries)
library(expm)
source("BoostedHP.R")

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
      img(src = "shiny.png", height = 70, width = 70),
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
                  choices = list("none", 
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
             h2("View the Input Data"),
             tableOutput("contents"), 
             #),
      
      # Horizontal line ----
      tags$hr(),
      h2("Plot"),
      plotOutput("Boosted"),
      
      
      
      
      
      
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
      
      
    )
  )
)

    
      
  


    
  


# Define server logic to read selected file ----
server <- function(input, output) {
  
  
  output$Boosted <- renderPlot({
    args <- switch(input$var,
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
  
  
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


