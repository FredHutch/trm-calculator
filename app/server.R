# Call required packages
library(shiny)
library(gt)

# Bring in helper functions
source("helper.R")

# Read in TRM table for simplified model without age
trmData <- get_table("trm-data-ageAgnostic.csv")
# Get list of score intervels
trmIntervals_list <- trmData$`TRM Score Interval`

# Read in TRM table for simplified model with age for 60+ and <=60
trmDataSixtyPlus <- get_table("trm-data-sixtyplus.csv")
trmDataUnderSixty <- get_table("trm-data-undersixty.csv")

# Define server logic 
server <- function(input, output, session) {
  
  # Set up input validation
  iv <- set_up_input_validator()
  
  # Initialize reactive values for output score and tables
  score <- reactiveVal(NULL)
  highlightedRow <- reactiveVal(NULL)
  trm_under_table_data <- reactiveVal(NULL)
  trm_over_table_data <- reactiveVal(NULL)
  trm_table_data <- reactiveVal(NULL)
  
  # Set the baseline TRM table to be the simple model w/out age and no highlight
  trm_table_data(
    make_gt_from_table_no_highlight(
      trmData, 
      "Simplified Model without Age"
    )
  )
  
  # Click the 'calculate' button to trigger output
  observeEvent(input$calculateNow, {
    # Check that inputs are valid
    req(iv$is_valid())
    
    # Get the TRM score from inputs and save it out
    x_t = get_trm_score(input)
    score(x_t)
    
    # Identify / save the appropriate row to highlight based on the TRM score
    rowIndex <- pick_row_to_highlight(
      score(), 
      trmIntervals_list
    )
    highlightedRow(rowIndex)
    
    # Get the TRM table with the right higlighted row
    trm_table_data(
      make_gt_from_table(
        trmData, 
        highlightedRow(), 
        "Simplified Model without Age"
      )
    )
    
    # Get the TRM table for age 60+ with highlighted row if appropriate
    if(!is.na(input$age) && input$age > 60 && !is.null(score())){
      trm_over_table_data(
        make_gt_from_table(
          trmDataSixtyPlus, 
          highlightedRow(), 
          "Simplifed Model with Age (Over 60)"
        )
      )
      
      trm_under_table_data(NULL)
    }
    
    # Get the TRM table for age <60 with highlighted row if appropriate
    if(!is.na(input$age) && input$age <= 60 && !is.null(score())){
      trm_under_table_data(
        make_gt_from_table(
          trmDataUnderSixty, 
          highlightedRow(), 
          "Simplifed Model with Age (60 and under)"
        )
      )
      
      trm_over_table_data(NULL)
    }
    
  })
  
  # Save output score to TRM text output
  output$trmScore <- renderText({
    if (is.null(score())){
      return("No score calculated yet.")
    } 
    
    paste("The TRM Score is: ", "<b>",score(),"</b>")
  })
  
  # Save TRM table to TRM table output
  output$trmTable <- render_gt({
    req(trm_table_data())
    trm_table_data()
  })
  
  # Save TRM 60+ table to TRM 60+ table output
  output$trmTableSixtyPlus <- render_gt({
    req(trm_over_table_data())
    trm_over_table_data()
  })
  
  # Save TRM <60 table to TRM <60 table output
  output$trmTableUnderSixty <- render_gt({
    req(trm_under_table_data())
    trm_under_table_data()
  })
  
  # Clicking "reset" will reset all inputs and outputs
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
    trm_table_data(
      make_gt_from_table_no_highlight(
        trmData, 
        "Simplified Model without Age"
      )
    )
    trm_over_table_data(NULL)
    trm_under_table_data(NULL)
  })
  
  # Save contact info to contact information output
  output$contactInfo <- renderUI({
    print_contact_info()
  })
  
  # Save background info to background information output
  output$background <- renderText({
    print_jco_manuscript()
  })
  
}
