
# Call required packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyvalidate)
library(dplyr)
library(gt)
library(gtExtras)

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
  dashboardHeader(
    title = tags$a(
      href='https://hutchdatascience.org',
      tags$img(
        src='fhLogo.png',
        height='35px',
        width='155px'
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "TRM Calculator", 
        tabName = "trm", 
        icon = icon("calculator")
      ),
      menuItem(
        "Background", 
        tabName = "background", 
        icon = icon("book")
      )
    )
  ),
  
  dashboardBody(
    includeCSS("www/hutch_theme.css"),
    
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Arial",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Treatment-Related Mortality (TRM) Calculator </span>\');
      })
     ')),
    
    tabItems(
      tabItem(
        tabName = "trm",
        fluidRow(
          box(
            selectInput("performance", "Performance Status (0 to 4)", choices = c("0", "1", "2", "3", "4")),
            numericInput("age", "Age (Years)", min = 0, max = 125, value = NULL),
            numericInput("platelets", HTML(paste0("Platelet Count (x10",tags$sup("3"), '/uL)')), min = 0, value = NULL),
            numericInput("albumin", "Albumin (g/dL)", min = 0, value = NULL),
            checkboxInput("secondaryAML", strong("Secondary AML? (Check if yes)"), value = FALSE)
          ),
          box(
            numericInput("wbc", HTML(paste0("White Blood Cell Count (x10",tags$sup("3"), '/uL)')), min = 0, value = NULL),
            numericInput("blast", "Blast Percentage in Peripheral Blood (%)", min = 0, max = 100, value = NULL),
            numericInput("creatinine", "Creatinine (mg/dL)", min = 0, value = NULL),
            actionButton(inputId = "calculateNow", label = strong("Calculate")),
            actionButton(inputId = "reset", label = strong("Reset")),
            htmlOutput(outputId = "trmScore")
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
              uiOutput(outputId = "contactInfo")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "background",
        box(
          width = 12, 
          uiOutput(outputId = "background")
        )
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
    paste("The TRM Score is: ", "<b>",calculation(),"</b>")
  })
  
  
  # Based on calculated TRM score, figure out which row to highlight in table
  highlightedRow <- eventReactive(input$calculateNow, {
    rowIndex = 0
    
    # Iterate over options in the TRM intervals and match the score
    for(elem in trmIntervals_list){
      rowIndex = rowIndex + 1
      minValue = as.numeric(strsplit(elem, " - ")[[1]][1])
      maxValue = as.numeric(strsplit(elem, " - ")[[1]][2])
      # If we find the right row, we can break out of our loop
      if(calculation() >= minValue && calculation() <= maxValue){
        break
      }
    }
    
    rowIndex
  })

  # Show the data table for the simplified model + relevant age
  observeEvent(input$calculateNow, {
    req(iv$is_valid())

    # Show the data table for the simplified model
    output$trmTable <- render_gt(
      expr = gt(trmData) |>
        gt_highlight_rows(
          rows = highlightedRow(),
          fill = "gold",
          bold_target_only = TRUE,
          target_col = `TRM Score Interval`
        ) |> tab_header(
          title = md("Simplified Model without Age")
        ) |>   cols_align(
          align = "left",
          columns = everything()
        )
    )
    
    # If age > 60, show only the older age table
    if(input$age > 60){
      output$trmTableSixtyPlus <- render_gt(
        expr = gt(trmDataSixtyPlus) |>
          gt_highlight_rows(
            rows = highlightedRow(),
            fill = "gold",
            bold_target_only = TRUE,
            target_col = `TRM Score Interval`
          ) |> tab_header(
            title = md("Simplified Model with Age (Over 60)")
          ) |>   cols_align(
            align = "left",
            columns = everything()
          )
        )
      
      output$trmTableUnderSixty <- render_gt({})
    }
    
    # If age <= 60, show only the younger age table
    if(input$age <= 60){
      output$trmTableUnderSixty <- render_gt(
        expr = gt(trmDataUnderSixty) |>
          gt_highlight_rows(
            rows = highlightedRow(),
            fill = "gold",
            bold_target_only = TRUE,
            target_col = `TRM Score Interval`
          ) |> tab_header(
            title = md("Simplifed Model with Age (60 and under)")
          ) |>   cols_align(
            align = "left",
            columns = everything()
          )
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
    HTML(
      paste(
        "This application was developed by the Fred Hutch ",
        daslWebsite,
        ". For questions or feedback regarding this application, email DaSL ",
        daslTA,
        " at ",
        daslEmail,
        "."
      )
    )
  })

  trmManuscript <- a("Prediction of Treatment-Related Mortality after Induction Therapy for Newly Diagnosed Acute Myeloid Leukemia",
                 href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3221524/")

  output$background <- renderText({
    HTML(
      paste(
        "<b>",
        trmManuscript,
        "</b>",
        "<br>",
        "<br>",
        "<i>",
        "Roland B. Walter, Megan Othus, Gautam Borthakur, Farhad Ravandi, Jorge E. Cortes, Sherry A. Pierce, Frederick R. Appelbaum, Hagop A. Kantarjian, and Elihu H. Estey",
        "</i>",
        "<br>",
        "<br>",
        "It is well known that the risk treatment-related mortality (TRM) varies considerably between individual patients with acute myeloid leukemia (AML). Numerous factors have been identified that are individually associated with this risk, including age and covariates that may serve as surrogates for the biological (rather than chronological) age, such as performance status, organ function parameters (e.g. bilirubin, fibrinogen, albumin, creatinine), degree of cytopenias, and disease characteristics. Using data from 3,365 adults of all ages administered intensive chemotherapy for newly diagnosed AML on SWOG trials or at M.D. Anderson Cancer Center between 1986 and 2009, we defined TRM as death within 28 days from initiation of chemotherapy based on the observation that the risk of death declined once 4 weeks had elapsed from treatment start. We then used the area under the receiver operator characteristic curve (AUC) to quantify the relative effects of individual covariates on TRM in a subset of 2,238 patients treated between 1986 and 2009 at M.D. Anderson Cancer Center. We found that multicomponent models were significantly more accurate in predicting TRM than individual covariates alone. A maximal model comprised of 17 covariates yielded an AUC of 0.83. Omission of covariates of lesser importance led to a “simplified” model that included performance status, age, platelet count, serum albumin, type of AML (secondary vs. primary), white blood cell count, percentage of blasts in the peripheral blood, and serum creatinine, yielding an AUC of 0.82.",
        "<br>",
        "<br>",
        "<b>",
        "Reference:",
        "</b>",
        "<br>",
        "1. Walter RB, Othus M, Borthakur G, Ravandi F, Cortes JE, Pierce SA, Appelbaum FR, Kantarjian HM, Estey EH. Prediction of early death following induction therapy for newly diagnosed acute myeloid leukemia with pretreatment risk scores: a novel paradigm for treatment assignment.",
        "<i>",
        "J Clin Oncol.",
        "</i>",
        "2011;29(33):4417-4424. PMID: 21969499."
      )
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