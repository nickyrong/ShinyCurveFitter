rm(list=ls())

library(shiny)
library(shinythemes)
library(plotly)


# a more elegant solution than multiple br()
linebreaks <- function(n){HTML(strrep(br(), n))}

shinyalert::useShinyalert() # for the pop up confirmation window

# Lists
Dist_Options <- c("Exponential", "Gamma", "GEV", "Gen. Logistic", "Gen. Normal", "Gen. Pareto",
                  "Gumbel", "Kappa", "Normal", "Pearson III", "Wakeby", "Weibull", "Log-Normal", "LP3")
Tr_Options <- c(2, 5, 10, 20, 25, 30, 40, 50, 75, 100, 200, 250, 300,
                400, 500, 1000, 2500, 10000)


shinyUI(fluidPage(
  
  theme = shinytheme("simplex"),
  
  tags$head(HTML("<title>Shiny App for Fitting Frequency Curve</title>")),
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight: bold;
      }
    "))
  ),
  
  
  pageWithSidebar(
    
    headerPanel(title=div("Frequency Curve Fitting", 
                          img(src='https://company_logo', 
                              style = "float:right;"
                          )
    )
    ),
    
    
    
    # Sidebar Panel
    sidebarPanel(
      width = 2,
      br(),
      
      # Upload format choice
      radioButtons(
        "FileType",
        label = h4("Choose File Type"),
        choices = list(".csv/txt" = "csv", ".xlsx" = "xlsx"),
        selected = "csv",
        inline = TRUE
      ),
      
      # File upload button
      fileInput(
        'uploadedfile',
        h4('Upload String File (No header!)'),
        accept = c(
          'text/csv',
          'text/comma-separated-values,text/plain',
          '.csv',
          '.xlsx'
        )
      ),
      
      linebreaks(2)

      
      
    ), # end of side bar panel
    
    
    
    # Main Panel
    mainPanel(
      
      # tabset panel
      tabsetPanel(
        
        #---------ReadMe tab
        tabPanel("Read Me",
                 htmlOutput("README") #it is technically a markdown render but HTML works
                 
        ), # End of Read Me tab
        
        #---------Main tab
        tabPanel("Curve Fitter",
                 h5("Review Data Entered:"),
                 verbatimTextOutput("previewfile", placeholder = TRUE),
                 
                 h5("Descriptive Statistics (pastecs::stat.desc):"),
                 verbatimTextOutput("simplestats", placeholder = FALSE),
                 
                 h5("Linear Moments (lmom::samlmu):"),
                 verbatimTextOutput("lmoments", placeholder = FALSE),
                 
                 
                 fluidRow(
                   column(6,
                          selectizeInput('selector_dist', 'Select Distributions',
                                         choices = Dist_Options, multiple = TRUE)),
                   column(6,
                          selectizeInput('selector_Tr', 'Select Return Periods (Years)',
                                         choices = Tr_Options, multiple = TRUE,
                                         options = list(maxItems = 10), selected = 2))),
                 br(),
                 DT::dataTableOutput("ffa.table"),
                 br(),
                 plotlyOutput("ffa.figure"),
                 br()
                 
        ) # End of Main tab
        
 
        
      ) # End of tab setting
      
    ) # End of main panel
    
  ) # End of Page with Panel
  
)) # End of Script