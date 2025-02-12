# Call required packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(gt)
library(bslib)

# Bring in images/CSS
addResourcePath('assets', 'www')

# Set up User Interface
ui <- dashboardPage(
  # Title
  title="Fred Hutch TRM Calculator",
  
  # Header (FH icon with link to OCDO)
  dashboardHeader(
    title = tags$a(
      href='https://ocdo.fredhutch.org',
      tags$img(
        src='/assets/fhLogo.png',
        height='35px',
        width='155px'
      )
    )
  ),
  
  # Sidebar with tabs
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "TRM Score Calculator", 
        tabName = "trm", 
        icon = icon("calculator")
      ),
      menuItem(
        "TRM Probability Tables", 
        tabName = "tables", 
        icon = icon("table")
      ),
      menuItem(
        "Related Literature", 
        tabName = "background", 
        icon = icon("book")
      )
    )
  ),
  
  # Dashboard body - themes, aesthetics, inputs, and outputs
  dashboardBody(
    includeCSS("www/hutch_theme.css"),
    tags$head(tags$title("Fred Hutch TRM Calculator")),
    
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Arial",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }'
    ))),
    
    tags$head(tags$style(HTML('.bslib-value-box .value-box-title {font-size:18px;}'))),
    
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
            selectInput(
              "performance", "Performance Status (0 to 4)", 
              choices = c("0", "1", "2", "3", "4")
            ),
            numericInput(
              "age", "Age (Years)", 
              min = 0, max = 125, value = NULL
            ),
            numericInput(
              "platelets", 
              HTML(paste0("Platelet Count (x10",tags$sup("3"), '/uL)')), 
              min = 0, value = NULL
            ),
            numericInput(
              "albumin", "Albumin (g/dL)", 
              min = 0, value = NULL
            ),
            checkboxInput(
              "secondaryAML", strong("Secondary AML? (Check if yes)"), 
              value = FALSE
            )
          ),
          
          box(
            numericInput(
              "wbc", 
              HTML(paste0("White Blood Cell Count (x10",tags$sup("3"), '/uL)')),
              min = 0, value = NULL
            ),
            numericInput(
              "blast", "Blast Percentage in Peripheral Blood (%)", 
              min = 0, max = 100, value = NULL
            ),
            numericInput(
              "creatinine", "Creatinine (mg/dL)", 
              min = 0, value = NULL
            ),
            actionButton(
              inputId = "calculateNow", 
              label = strong("Calculate")
            ),
            actionButton(
              inputId = "reset", 
              label = strong("Reset")
            )
          )
        ),
        
        fluidRow(
          column(
            12, 
            box(
              width = 12, 
              
              value_box(
                title = "The TRM Score is: ",
                value = htmlOutput(outputId = "trmScore"),
                theme = value_box_theme(bg = "#1B365D")
              )
              
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
        tabName = "tables",
        
        fluidRow(
          column(
            12, 
            box(
              width = 12, 
              
              value_box(
                title = "The TRM Score is: ",
                value = htmlOutput(outputId = "trmScoreForTables"),
                theme = value_box_theme(bg = "#1B365D")
              )
              
            )
          ),
          
          column(
            12, 
            gt_output(outputId = "trmTable")
          ),
          column(
            12, 
            gt_output(outputId = "trmTableSixtyPlus")
          ),
          column(
            12, 
            gt_output(outputId = "trmTableUnderSixty")
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
