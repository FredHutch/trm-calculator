# Call required packages
library(shiny)
library(shinyvalidate)
library(gt)
library(gtExtras)

# Read in data for gt table
get_table <- function(path_to_table) {
  read.csv(
    path_to_table, 
    header = TRUE,
    check.names = FALSE
  )
}

#Initialize the input validator
set_up_input_validator <- function() {
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
  
  iv
}

# Determine the appropriate row to highlight in the TRM table based on score
pick_row_to_highlight <- function(current_score, trm_intervals) {
  rowIndex = 0
  # Iterate over options in the TRM intervals and match the score
  for(elem in trm_intervals){
    rowIndex = rowIndex + 1
    minValue = as.numeric(strsplit(elem, " - ")[[1]][1])
    maxValue = as.numeric(strsplit(elem, " - ")[[1]][2])
    # If we find the right row, we can break out of our loop
    if(current_score >= minValue && current_score <= maxValue){
      break
    }
  }
  
  rowIndex
}


# Create a gt table based on the input TRM table
make_gt_from_table <- function(current_table, row_to_highlight, table_title) {
  gt(current_table) |>
    gt_highlight_rows(
      rows = row_to_highlight,
      fill = "gold",
      bold_target_only = TRUE,
      target_col = `TRM Score Interval`
    ) |> tab_header(
      title = md(table_title)
    ) |> cols_align(
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
}


# Print out contact information for the bottom of the app
print_contact_info <- function() {
  issue_tracker <- a("a new issue", href="https://github.com/FredHutch/trm-calculator/issues/new/")
  email <- a("katrus@fredhutch.org", href="mailto:katrus@fredhutch.org")
  
  HTML(
    paste0(
      "This application was developed in partnership with the Fred Hutch Data Science Lab (DaSL). To request new features or report any concerns, please file ",
      issue_tracker,
      " on GitHub. For direct inquiries or clinical questions related to the model, please send an email to Katie Russell at ",
      email,
      "."
    )
  )
}


# Print out background information for the JCO manuscript
print_jco_manuscript <- function() {
  trmManuscript <- a("Prediction of Treatment-Related Mortality after Induction Therapy for Newly Diagnosed Acute Myeloid Leukemia",
                     href="https://doi.org/10.1200/JCO.2011.35.7525")
  
  manuscript_one <- a("Second Allogeneic Hematopoietic Cell Transplantation for Relapsed Adult Acute Myeloid Leukemia: Outcomes and Prognostic Factors",
                     href="https://doi.org/10.1016/j.jtct.2024.06.019")
  
  manuscript_two <- a("Utility of the Treatment-Related Mortality (TRM) score to predict outcomes of adults with acute myeloid leukemia undergoing allogeneic hematopoietic cell transplantation",
                     href="https://doi.org/10.1038/s41375-022-01574-5")
  
  manuscript_three <- a("Accuracy of SIE/SIES/GITMO Consensus Criteria for Unfitness to Predict Early Mortality After Intensive Chemotherapy in Adults With AML or Other High-Grade Myeloid Neoplasm",
                     href="https://doi.org/10.1200/JCO.20.01392")
  
  manuscript_four <- a("Prediction of early death in adults with relapsed or refractory acute myeloid leukemia",
                     href="https://doi.org/10.3109/10428194.2015.1135436")
  
  manuscript_five <- a("The treatment-related mortality score is associated with non-fatal adverse events following intensive AML induction chemotherapy",
                     href="https://doi.org/10.1038/bcj.2014.97")
  HTML(
    paste0(
      "The development of the treatment-related mortality (TRM) risk model is outlined in the manuscript ",
      "<b>", trmManuscript, "</b> (Walter RB, et al.). ",
      "<br>",
      "<br>",
      "<b>Abstract</b><br>",
      "It is well known that the risk treatment-related mortality (TRM) varies considerably between individual patients with acute myeloid leukemia (AML). Numerous factors have been identified that are individually associated with this risk, including age and covariates that may serve as surrogates for the biological (rather than chronological) age, such as performance status, organ function parameters (e.g. bilirubin, fibrinogen, albumin, creatinine), degree of cytopenias, and disease characteristics. Using data from 3,365 adults of all ages administered intensive chemotherapy for newly diagnosed AML on SWOG trials or at M.D. Anderson Cancer Center between 1986 and 2009, we defined TRM as death within 28 days from initiation of chemotherapy based on the observation that the risk of death declined once 4 weeks had elapsed from treatment start. We then used the area under the receiver operator characteristic curve (AUC) to quantify the relative effects of individual covariates on TRM in a subset of 2,238 patients treated between 1986 and 2009 at M.D. Anderson Cancer Center. We found that multicomponent models were significantly more accurate in predicting TRM than individual covariates alone. A maximal model comprised of 17 covariates yielded an AUC of 0.83. Omission of covariates of lesser importance led to a “simplified” model that included performance status, age, platelet count, serum albumin, type of AML (secondary vs. primary), white blood cell count, percentage of blasts in the peripheral blood, and serum creatinine, yielding an AUC of 0.82.",
      "<br>",
      "<br>",
      "<b>Additional articles that have used or evaluated the TRM score include: </b>",
      "<br>",
      "1. ", manuscript_one,
      "<br>",
      "2. ", manuscript_two,
      "<br>",
      "3. ", manuscript_three,
      "<br>",
      "4. ", manuscript_four,
      "<br>",
      "5. ", manuscript_five
    )
  )
}

# Calculate the TRM score based on user input
get_trm_score <- function(input) {
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
  
  x_t
}

# Print out introductory information for the top of the main calculator
intro <- function() {
  HTML(
    paste0(
      "This calculator is useful for evaluating mortality risk of high intensity therapy for Acute Myeloid Leukemia (AML) patients. The statistical model underlying this calculator was trained on data from 3,365 adults of all ages administered intensive chemotherapy for newly diagnosed AML on SWOG trials or at M.D. Anderson Cancer Center between 1986 and 2009. For more information, click on the \"<b>Background</b>\" tab to the left."
    )
  )
}
  
