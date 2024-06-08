
# Call required packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyvalidate)
library(DT)
library(dplyr)
library(gt)

# Read in TRM table for simplified model without age
trmData <- read.csv(
  "trm-data-ageAgnostic.csv", 
  header = TRUE,
  check.names = FALSE
)

# Read in TRM table for simplified model with age for 60+
trmDataSixtyPlus <- read.csv(
  "trm-data-sixtyplus.csv", 
  header = TRUE,
  check.names = FALSE
)

# Read in TRM table for simplified model with age for 60 and under
trmDataUnderSixty <- read.csv(
  "trm-data-undersixty.csv", 
  header = TRUE,
  check.names = FALSE
)

# Define the possible trm Intervals
trmIntervals_list <- trmData$`TRM Score Interval`

# Set up User Interface
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$a(
      href='https://hutchdatascience.org',
      tags$img(
        src='fhLogo.png',
        height='35px',
        width='150px'
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "TRM Calculator", 
        tabName = "trm", 
        icon = icon("calculator"), 
        badgeLabel = "calculate", 
        badgeColor = "red"
      ),
      menuItem(
        "Other Handy Tool", 
        tabName = "futuretool", 
        icon = icon("vial"),
        badgeLabel = "future", 
        badgeColor = "orange"
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "trm",
        fluidRow(
          box(
            selectInput("performance", "Performance Status (0 - 4)", choices = c("0", "1", "2", "3", "4")),
            numericInput("platelets", "Platelet Count (x10^3/uL)", min = 0, value = NULL),
            numericInput("albumin", "Albumin (g/dL)", min = 0, value = NULL),
            numericInput("age", "Age (Years)", min = 0, max = 125, value = NULL),
            checkboxInput("secondaryAML", "Secondary AML? (Check if yes)", value = FALSE)
          ),
          box(
            numericInput("wbc", "White Blood Cell Count (x10^3/uL)", min = 0, value = NULL),
            numericInput("blast", "Blast Percentage in Peripheral Blood (%)", min = 0, max = 100, value = NULL),
            numericInput("creatinine", "Creatinine (mg/dL)", min = 0, value = NULL),
            actionButton(inputId = "calculateNow", label = "Calculate"),
            textOutput(outputId = "trmScore"),
            actionButton(inputId = "reset", label = "Reset")
          )
        ),
        
        fluidRow(
          column(12, gt_output(outputId = "trmTable")),
          column(12, gt_output(outputId = "trmTableSixtyPlus")),
          column(12, gt_output(outputId = "trmTableUnderSixty"))
        ),
        
        fluidRow(
          column(
            12, 
            box(
              width = 12, 
              "Publication URL: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3221524/. Prediction of Treatment-Related Mortality after Induction Therapy for Newly Diagnosed Acute Myeloid Leukemia - It is well known that the risk treatment-related mortality (TRM) varies considerably between individual patients with acute myeloid leukemia (AML). Numerous factors have been identified that are individually associated with this risk, including age and covariates that may serve as surrogates for the biological (rather than chronological) age, such as performance status, organ function parameters (e.g. bilirubin, fibrinogen, albumin, creatinine), degree of cytopenias, and disease characteristics. Using data from 3,365 adults of all ages administered intensive chemotherapy for newly diagnosed AML on SWOG trials or at M.D. Anderson Cancer Center between 1986 and 2009, we defined TRM as death within 28 days from initiation of chemotherapy based on the observation that the risk of death declined once 4 weeks had elapsed from treatment start. We then used the area under the receiver operator characteristic curve (AUC) to quantify the relative effects of individual covariates on TRM in a subset of 2,238 patients treated between 1986 and 2009 at M.D. Anderson Cancer Center. We found that multicomponent models were significantly more accurate in predicting TRM than individual covariates alone. A maximal model comprised of 17 covariates yielded an AUC of 0.83. Omission of covariates of lesser importance led to a “simplified” model that included performance status, age, platelet count, serum albumin, type of AML (secondary vs. primary), white blood cell count, percentage of blasts in the peripheral blood, and serum creatinine, yielding an AUC of 0.82."
            )
          )
        ),
        
        fluidRow(
          column(
            12, 
            box(
              width = 12, 
              uiOutput(outputId = "contactInfo")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "futuretool",
        box(
          align = "center",
          h3("What else would be handy to have right next to this calculator? Could we put it here?")
        ),
      )
    )
  )
)

# Define server logic required 
server <- function(input, output, session) {  
  iv <- InputValidator$new()
  iv$add_rule("age", sv_required())
  iv$add_rule("age", function(value) {
    if (value < 0) {
      "Age must be greater than 0"
    }
  })
  iv$add_rule("platelets", sv_required())
  iv$add_rule("platelets", function(value) {
    if (value < 0) {
      "Platelet count must be greater than 0"
    }
  })
  iv$add_rule("albumin", sv_required())
  iv$add_rule("albumin", function(value) {
    if (value < 0) {
      "Albumin must be greater than 0"
    }
  })
  iv$add_rule("wbc", sv_required())
  iv$add_rule("wbc", function(value) {
    if (value < 0) {
      "WBC must be greater than 0"
    }
  })
  iv$add_rule("blast", sv_required())
  iv$add_rule("blast", function(value) {
    if (value < 0 || value > 100) {
      "Blast Percentage must be between 0 and 100"
    }
  })
  iv$add_rule("creatinine", sv_required())
  iv$add_rule("creatinine", function(value) {
    if (value < 0) {
      "Creatinine must be greater than 0"
    }
  })
  iv$enable()

  vals <- reactiveValues(
    row_priority = trmIntervals_list,
    row_color = rep('white', 7)
  )
  
  calculation <- eventReactive(
    input$calculateNow, 
    {
      req(iv$is_valid())
      
      PS = as.numeric(input$performance)
      age = input$age
      platelets = input$platelets
      albumin = input$albumin
      hasSecondaryAML = ifelse(input$secondaryAML==T,1,0)
      WBC = input$wbc # white blood count
      PBBP = input$blast # peripheral blood blast percentage
      creatinine = input$creatinine
      
      x = -4.08 + 0.89*PS + 0.03*age - 0.008*platelets - 0.48*albumin +
        0.47*hasSecondaryAML  + 0.007*WBC - 0.007*PBBP + 0.34*creatinine
      
      output = round(
        100/(1 + exp(-x)),
        digits = 4
      )
      
      output
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
  
  
  output$trmScore <- renderText({
    paste0("The TRM Score is: ", calculation())
  })
  
  
  # Based on calculated TRM score, figure out which row to highlight in table
  highlightedRow <- eventReactive(input$calculateNow, {
    chosenTrmInterval = "0 - 1.9"
    
    # Iterate over options in the TRM intervals and match the score
    for(elem in trmIntervals_list){
      minValue = as.numeric(strsplit(elem, " - ")[[1]][1])
      maxValue = as.numeric(strsplit(elem, " - ")[[1]][2])
      # If we find the right row, we can break out of our loop
      if(calculation() >= minValue && calculation() <= maxValue){
        chosenTrmInterval = elem
        break
      }
    }
    
    # Match the identified row with the right highlight color
    vals$row_priority <- c(
      chosenTrmInterval, 
      vals$row_priority[vals$row_priority != chosenTrmInterval]
    )
    vals$row_color <- c(
      'lightgreen', #TODO: maybe change colors to Hutch themes?
      'white', 'white', 'white', 'white', 'white', 'white'
    )
    
    vals
  })

  # Show the data table for the simplified model + relevant age
  observeEvent(input$calculateNow, {
    # Show the data table for the simplified model
    # "Simplified Model without Age"
    output$trmTable <- render_gt(
      trmData
    )
    
    # If age > 60, show only the older age table
    # "Simplified Model with Age (Over 60)"
    if(input$age > 60){
      output$trmTableSixtyPlus <- render_gt(
          trmDataSixtyPlus
        )
      
      output$trmTableUnderSixty <- render_gt({})
    }
    
    # If age <= 60, show only the younger age table
    # caption = "Simplifed Model with Age (60 and under)"
    if(input$age <= 60){
      output$trmTableUnderSixty <- render_gt(
          trmDataUnderSixty
        )
      
      output$trmTableSixtyPlus <- render_gt({})
    }
  })
  
  # Hitting the reset button will clear all values
  observeEvent(input$reset, {
    updateSelectInput(session,"performance", selected = 0)
    updateNumericInput(session, "platelets", value = NA)
    updateNumericInput(session, "albumin", value = NA)
    updateNumericInput(session, "age", value = NA)
    updateCheckboxInput(session, "secondaryAML", value = FALSE)
    updateNumericInput(session, "wbc", value = NA)
    updateNumericInput(session, "blast", value = NA)
    updateNumericInput(session, "creatinine", value = NA)
    output$trmScore <- renderText({""})
    output$trmTableSixtyPlus <- render_gt({})
    output$trmTableUnderSixty <- render_gt({})
    output$trmTable <- render_gt({})
  })
  
  daslWebsite <- a("Data Science Lab (DaSL)", href="https://hutchdatascience.org")
  daslTA <- a("Translational Analytics", href="https://hutchdatascience.org/tr-analytics/")
  daslEmail <- a("analytics@fredhutch.org", href="mailto:analytics@fredhutch.org")
  
  output$contactInfo <- renderUI({
    tagList(
      "This application was developed by the Fred Hutch ",
      daslWebsite,
      ". For questions or feedback regarding this application, email DaSL ",
      daslTA,
      " at ",
      daslEmail,
      "."
    )
  })
}


# Run the application 
options <- list()

if (!interactive()) {
  options$port = 3838
  options$launch.browser = FALSE
  options$host = "0.0.0.0"
}

shinyApp(
  ui, 
  server, 
  options = options
)