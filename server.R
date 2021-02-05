rm(list=ls())

library(tidyverse)
library(readxl) # read Excel file
library(tools) # for file_ext() for getting file extension
library(pastecs) # quick descriptive stats stat.desc()
library(lmom) # L-moments method
library(plotly) 
library(DT)


# Distributions List for Flood Frequency Analyis (FFA)
Dist_Options <- c("Exponential", "Gamma", "GEV", "Gen. Logistic", "Gen. Normal", "Gen. Pareto",
                  "Gumbel", "Kappa", "Normal", "Pearson III", "Wakeby", "Weibull", "Log-Normal", "LP3")

Dist_Key <- Dist_Options

# ---------------- FFA function from ShinyCFA ------------------
lmom_Q <- function(Qp, empirical.Tr = NA, evaluation = FALSE) {
  
  
  # Custom output (let's keep the custom of having largest on top)
  if(evaluation == TRUE) (
    ReturnPeriods <- empirical.Tr %>% sort(decreasing = FALSE)
  ) else if(evaluation == "Plot") (
    ReturnPeriods  <- c(seq(1.01, 1.99, by = 0.01), seq(2, 9.9, by = 0.1), 10:1000) %>% sort(decreasing = FALSE)
  ) else (
    ReturnPeriods <- empirical.Tr %>% sort(decreasing = FALSE)
  )
  
  Pnonexc = 1 - (1/ReturnPeriods)
  
  # samlmu() gets the sample L-moments, pelxxx() estimates the distribution's parameters from L-moments
  # Quaxxx generates quantile given probability and distribution parameters
  # xxx = "exp" "gam" "gev" "glo" "gno" "gpa" "gum" "kap" "ln3" "nor" "pe3" "wak" "wei"
  
  lmoms <- samlmu(Qp, nmom = 5)
  log.lmoms <- samlmu(log10(Qp),nmom = 5)
  
  error_value <- as.numeric(rep(NA, length(Pnonexc)))
  # using tryCatch to allow the app to continue running if a particular distibution can't be fit to the data
  extremes <- tibble(ReturnPeriods = ReturnPeriods,
                     Pnonexc = Pnonexc,
                     Exponential = tryCatch(error = function(err) {return(error_value)}, quaexp(f = Pnonexc, para = pelexp(lmoms))), # exponential
                     Gamma = tryCatch(error = function(err) {return(error_value)}, quagam(f = Pnonexc, para = pelgam(lmoms))), # gamma
                     GEV = tryCatch(error = function(err) {return(error_value)}, quagev(f = Pnonexc, para = pelgev(lmoms))), # generalized extreme-value
                     `Gen. Logistic` = tryCatch(error = function(err) {return(error_value)}, quaglo(f = Pnonexc, para = pelglo(lmoms))), # generalized logistic
                     `Gen. Normal` = tryCatch(error = function(err) {return(error_value)}, quagno(f = Pnonexc, para = pelgno(lmoms))), # generalized normal
                     `Gen. Pareto` = tryCatch(error = function(err) {return(error_value)}, quagpa(f = Pnonexc, para = pelgpa(lmoms))), # generalized pareto
                     Gumbel = tryCatch(error = function(err) {return(error_value)}, quagum(f = Pnonexc, para = pelgum(lmoms))), # gumbel (extreme value type I)
                     Kappa = tryCatch(error = function(err) {return(error_value)}, quakap(f = Pnonexc, para = pelkap(lmoms))), # kappa
                     Normal = tryCatch(error = function(err) {return(error_value)}, quanor(f = Pnonexc, para = pelnor(lmoms))), # normal
                     `Pearson III` = tryCatch(error = function(err) {return(error_value)}, quape3(f = Pnonexc, para = pelpe3(lmoms))), # pearson type III
                     Wakeby = tryCatch(error = function(err) {return(error_value)}, quawak(f = Pnonexc, para = pelwak(lmoms))), # wakeby
                     Weibull = tryCatch(error = function(err) {return(error_value)}, quawei(f = Pnonexc, para = pelwei(lmoms))), # weibull
                     
                     # Logged distribution from the package
                     `Log-Normal` = tryCatch(error = function(err) {return(error_value)}, qualn3(f = Pnonexc, para = pelln3(lmoms))), # lognormal
                     
                     # Manually created log distribution
                     LP3 = tryCatch(error = function(err) {return(error_value)}, 10^quape3(f = Pnonexc, para = pelpe3(log.lmoms))) # log pearson type III
  )
  
  if (evaluation == TRUE) {
    extremes <- extremes %>% mutate(Qp.obs = Qp) # observed Qp
  }
  
  return(extremes)
} # End of Flood Frequency Function





#------------------ Shiny app functions
# Define server functionality
function(input, output, session) {
 
  
  #---------- README Tab
  # using HTML will mess up CSS style/theme format for some reasons, use Markdown
  
  output$README <- renderUI({

      includeMarkdown("./README.md")

  })
  
  #----------- Data Upload
  
  # Get the upload file
  get_file <- reactive({
    
    inFile <- input$uploadedfile
    
    if (is.null(inFile))
      return(NULL)
    
    if (input$FileType == "csv") {
      
      validate(
        need(file_ext(inFile$name) %in% c(
          'text/csv',
          'text/comma-separated-values',
          'csv'
        ), "Wrong file format, please try again."))
      
      # only read data from the first column
      uploadedfile <- read.csv(inFile$datapath,
                          header = FALSE,
                          stringsAsFactors = FALSE)[,1]
    } else {
      
      validate(
        need(file_ext(inFile$name) %in% c(
          'xlsx'
        ), "Wrong file format, please try again."))
      
      uploadedfile <- readxl::read_excel(inFile$datapath, 
                                         col_names = FALSE, 
                                         # get rid of the "New names:..." warning message
                                         .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
                                         ) %>% 
                          # only read data from the first column
                          pull(1)
    }
    
    uploadedfile
    
  })
  
  
  
  #------------------ Main tab
  
  output$previewfile <- renderText({get_file()})
  
  output$simplestats <- renderPrint({
    
    validate(
      need(!is.null(get_file()), "No file uploaded"))
    
    get_file() %>% 
      stat.desc(basic = TRUE, desc = TRUE, norm = TRUE) %>% 
      round(digits = 3)
    
    })
  
  output$lmoments <- renderPrint({
    
    validate(
      need(!is.null(get_file()), "No file uploaded"))
    
    get_file() %>% samlmu(nmom=5)
    
  })
  

  # FFA Plot
  output$ffa.figure <- renderPlotly({
    
    validate(
      need(!is.null(get_file()), "No file uploaded"))
    
    empirical.ffa <- tibble(AMS = get_file()) %>% 
                        mutate(Rank = base::rank(-AMS, ties.method = "random"),
                                                 Tr = ((length(Rank)+1) / Rank)
    ) 
    
    desired_columns <- Dist_Key[match(input$selector_dist, Dist_Options)]
    
    ffa_results <- lmom_Q(Qp = get_file(), evaluation = "Plot") %>%
      select(-Pnonexc) %>%
      select(ReturnPeriods, !!desired_columns)
    
    ffa_results <- gather(ffa_results, "Distribution", "Q", -1)
    
    ffa_reduced_variate <- -log(-log(1-1/ffa_results$ReturnPeriods))
    empirical_reduced_variate <- -log(-log(1-1/empirical.ffa$Tr))
    
    if (length(desired_columns) > 0) (
      
      ffa_plot <- ggplot(data = ffa_results) +
        suppressWarnings(geom_line(aes(x = ffa_reduced_variate, y = Q, color = Distribution, `Return Periods`=ReturnPeriods))) + theme_bw() +
        suppressWarnings(geom_point(data = empirical.ffa, aes(x = empirical_reduced_variate, y = AMS, colour = "Observed", `Return Periods`=Tr))) +
        scale_x_continuous(name = "Return Periods", breaks =-log(-log(1-1/c(2,5,10,20,50,100,200,500,1000))), 
                          labels = c(2,5,10,20,50,100,200,500,1000)) +
        scale_y_continuous(name = 'Quantile', limits=c(0, NA)) + 
        ggtitle("L-moments Fitted Frequency Distributions") + theme_bw() 
      
    )
    
    if (length(desired_columns) > 0) (ggplotly(ffa_plot, height = 800, width = 1000))
  })
  
  # FFA DataTable
  output$ffa.table <- DT::renderDataTable({
    
    validate(
      need(!is.null(get_file()), "No file uploaded"))
    
    empirical.ffa <- tibble(AMS = get_file()) %>% 
      mutate(Rank = base::rank(-AMS, ties.method = "random"),
             Tr = ((length(Rank)+1) / Rank)
      ) 
    
    desired_columns <- Dist_Key[match(input$selector_dist, Dist_Options)]
    
    if (length(input$selector_Tr) < 1) (
      ffa_results <- lmom_Q(Qp = empirical.ffa$AMS) %>%
        mutate_at(vars(-Pnonexc), round(., 2)) %>%
        mutate_at(vars(Pnonexc), round(., 6)) %>%
        select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
        rename("Return Periods" = ReturnPeriods, "Probability Non-Exc" = Pnonexc)
      
    ) else (
      ffa_results <- lmom_Q(Qp = empirical.ffa$AMS, empirical.Tr = as.integer(input$selector_Tr)) %>%
        mutate_at(vars(-Pnonexc), list(~round(., 2))) %>%
        mutate_at(vars(Pnonexc), list(~round(., 6))) %>%
        select(ReturnPeriods, Pnonexc, !!desired_columns) %>%
        rename("Return Periods" = ReturnPeriods, "Probability Non-Exc" = Pnonexc)
    )
    
    ffa_results %>% DT::datatable(
      # Options for data table formatting
      extensions = c('Buttons', 'FixedColumns'),
      options = list(
        
        # Options for extension "Buttons"
        dom = 'Bfrtip',
        
        buttons = 
          list(I('colvis'), list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', 
                   filename = "fitted_values.csv", title = "L-moment Fitted Distributions"),
              list(extend = 'excel', 
                   filename = "fitted_values.csv", title = "L-moment Fitted Distributions")
            ),
            text = 'Download Results'
          )),
        
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        
        # Options for extension "FixedColumns"
        scrollX = TRUE,
        
        # Options for extension "Scroller"
        deferRender = TRUE,
        scroller = TRUE
        
      )
    )
  })
  
  
}