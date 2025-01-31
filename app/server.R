
# Call required packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyvalidate)
library(dplyr)
library(gt)
library(gtExtras)

addResourcePath('assets', 'www')

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
  
  score <- reactiveVal(NULL)
  highlightedRow <- reactiveVal(NULL)
  trm_table_data <- reactiveVal(NULL)
  trm_under_table_data <- reactiveVal(NULL)
  trm_over_table_data <- reactiveVal(NULL)
  
  # Calculation Button
  observeEvent(input$calculateNow, {
    req(iv$is_valid())
    
    PS = as.numeric(input$performance)
    age = input$age
    platelets = input$platelets
    albumin = input$albumin
    hasSecondaryAML = ifelse(input$secondaryAML == T,1,0)
    WBC = input$wbc # white blood count
    PBBP = input$blast # peripheral blood blast percentage
    creatinine = input$creatinine
    
    x = -4.08 + 0.89*PS + 0.03*age - 0.008*platelets - 0.48*albumin +
      0.47*hasSecondaryAML  + 0.007*WBC - 0.007*PBBP + 0.34*creatinine
    x_t = round(100/(1 + exp(-x)), digits = 4)
    score(x_t)
    
    rowIndex = 0
    # Iterate over options in the TRM intervals and match the score
    for(elem in trmIntervals_list){
      rowIndex = rowIndex + 1
      minValue = as.numeric(strsplit(elem, " - ")[[1]][1])
      maxValue = as.numeric(strsplit(elem, " - ")[[1]][2])
      # If we find the right row, we can break out of our loop
      if(score() >= minValue && score() <= maxValue){
        break
      }
    }
    highlightedRow(rowIndex)
    
    trm_table_data(
      gt(trmData) |>
        gt_highlight_rows(
          rows = highlightedRow(),
          fill = "gold",
          bold_target_only = TRUE,
          target_col = `TRM Score Interval`
        ) |>
        tab_header(
          title = md("Simplified Model without Age")
        ) |>
        cols_align(
          align = "left",
          columns = everything()
        ) |>
        opt_table_font(
          font = list(
            google_font(name = "Arial"),
            "serif"
          )
        ) |> 
        tab_style(
          style = cell_borders(
            sides = c("left", "right"),
            weight = px(0.5)),
          locations = cells_body(
            columns = everything()
          )
        )
    )
    
    if(!is.na(age) && age > 60 && !is.null(score())){
      trm_over_table_data(
        gt(trmDataSixtyPlus) |>
          gt_highlight_rows(
            rows = highlightedRow(),
            fill = "gold",
            bold_target_only = TRUE,
            target_col = `TRM Score Interval`
          ) |> 
          tab_header(
            title = md("Simplified Model with Age (Over 60)")
          ) |>
          cols_align(
            align = "left",
            columns = everything()
          ) |>
          opt_table_font(
            font = list(
              google_font(name = "Arial"),
              "serif"
            )
          ) |> 
          tab_style(
            style = cell_borders(
              sides = c("left", "right"),
              weight = px(0.5)),
            locations = cells_body(
              columns = everything()
            )
          )
      )
      
      trm_under_table_data(NULL)
    }
    
    if(!is.na(age) && age <= 60 && !is.null(score())){
      trm_under_table_data(
        gt(trmDataUnderSixty) |>
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
          ) |> opt_table_font(
            font = list(
              google_font(name = "Arial"),
              "serif"
            )
          ) |> 
          tab_style(
            style = cell_borders(
              sides = c("left", "right"),
              weight = px(0.5)),
            locations = cells_body(
              columns = everything()
            )
          ) 
      )
      
      trm_over_table_data(NULL)
    }
    
  })
  
  output$trmScore <- renderText({
    if (is.null(score())) return("No score calculated yet.")
    paste("The TRM Score is: ", "<b>",score(),"</b>")
  })
  
  output$trmTable <- render_gt({
    req(trm_table_data())
    trm_table_data()
  })
  
  output$trmTableSixtyPlus <- render_gt({
    req(trm_over_table_data())
    trm_over_table_data()
  })
  
  output$trmTableUnderSixty <- render_gt({
    req(trm_under_table_data())
    trm_under_table_data()
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
    
    score(NULL)
    trm_table_data(NULL)
    trm_over_table_data(NULL)
    trm_under_table_data(NULL)
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
