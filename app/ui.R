# Call required packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(gt)
library(bslib)
library(shinyBS)

# Bring in images/CSS
addResourcePath('assets', 'www')

# Set up User Interface
ui <- dashboardPage(
  # Title
  title="Fred Hutch TRM Calculator",
  
  # Header (FH icon with link to OCDO)
  dashboardHeader(
    title = tags$a(
      href='https://www.fredhutch.org/en.html',
      tags$img(
        src='/assets/fhLogo.png',
        height='35px',
        width='155px',
        alt='Fred Hutch logo'
      )
    )
  ),
  
  # Sidebar with tabs
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
        icon = icon("book-open")
      )
    )
  ),
  
  # Dashboard body - themes, aesthetics, inputs, and outputs
  dashboardBody(
    tags$html(lang="en"),
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
      }
      a {
        color: #346F93
      }
      .checkbox { /* checkbox is a div class*/
        line-height: 5px;
        margin-bottom: 40px; /*set the margin, so boxes dont overlap*/
      }
      input[type=\'radio\']{
        width: 20px; /*Desired width*/
        height: 20px; /*Desired height*/
        line-height: 20px; 
      }
      span { 
          margin-left: 0px;  /*set the margin, so boxes dont overlap labels*/
          line-height: 30px; 
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
            width = 12, 
            uiOutput(outputId = "intro")
          )
        ),
        
        
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
            radioButtons(
              "secondaryAML",
              "Type of Acute Myeloid Leukemia (AML) ",
              choices = list("No AML or de novo AML" = 0, "Secondary AML" = 1),
              selected = 0
            ),
            bsPopover(
              "secondaryAML", 
              "<b><i>Secondary AML</i></b> is defined as having a documented blood count abnormality for at least a month before the diagnosis of <b>(a)</b> AML after an antecedent hematologic dorder (AHD) or <b>(b)</b> AML after cytotoxic therapy", 
              placement = "top", 
              trigger = "hover"
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
            ),
            htmlOutput(
              outputId = "trmScore"
            )
          )
        ),
        
        fluidRow(
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
        ),
        
        fluidRow(
          box(
            width = 12, 
            uiOutput(outputId = "contactInfo")
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
