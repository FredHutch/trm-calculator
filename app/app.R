
library(shiny); library(shinythemes); library(shinydashboard);

trmData <- read.csv("trm-data.csv", header = TRUE)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Handy Toolset"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("TRM Calculator", tabName = "trm", icon = icon("calculator"), 
                                     badgeLabel = "calculate", badgeColor = "red"),
                            menuItem("Other Handy Tool", tabName = "futuretool", icon = icon("vial"),
                                     badgeLabel = "future", badgeColor = "orange")
                        )
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "trm",
                                    fluidRow(
                                             box(
                                              selectInput("performance", "Performance Status", choices = c("0", "1", "2", "3", "4")),
                                               numericInput("platelets","Platelet Count",min = 0, value=NULL),
                                               numericInput("albumin","Albumin",min = 0, value=NULL),
                                               numericInput("age","Age",min = 0,max = 125, value=NULL),
                                               checkboxInput("secondaryAML", "Secondary AML?", value = FALSE)
                                             ),
                                             box(
                                               numericInput("wbc","WBC",min = 0, value=NULL),
                                               numericInput("blast","% Blast in Peripheral Blood",min = 0,max = 100, value=NULL),
                                               numericInput("creatinine","Creatinine",min = 0, value=NULL),
                                               actionButton(inputId = "calculateNow", 
                                                            label = "Calculate"),
                                               actionButton(inputId = "reset", 
                                                            label = "Reset Button doesn't work yet"),
                                               textOutput(outputId = "trmScore")
                                             )
                                    ),
                                    fluidRow(
                                      column(12, tableOutput(outputId = "trmTable"))
                                    ),
                                    fluidRow(
                                      column(12, 
        
                                    box(width = 12, "Publication URL: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3221524/
                                        
                                        Prediction of Treatment-Related Mortality after Induction Therapy for Newly Diagnosed Acute Myeloid Leukemia

It is well known that the risk treatment-related mortality (TRM) varies considerably between individual patients with acute myeloid leukemia (AML). Numerous factors have been identified that are individually associated with this risk, including age and covariates that may serve as surrogates for the biological (rather than chronological) age, such as performance status, organ function parameters (e.g. bilirubin, fibrinogen, albumin, creatinine), degree of cytopenias, and disease characteristics.

Using data from 3,365 adults of all ages administered intensive chemotherapy for newly diagnosed AML on SWOG trials or at M.D. Anderson Cancer Center between 1986 and 2009, we defined TRM as death within 28 days from initiation of chemotherapy based on the observation that the risk of death declined once 4 weeks had elapsed from treatment start.1

We then used the area under the receiver operator characteristic curve (AUC) to quantify the relative effects of individual covariates on TRM in a subset of 2,238 patients treated between 1986 and 2009 at M.D. Anderson Cancer Center. We found that multicomponent models were significantly more accurate in predicting TRM than individual covariates alone. A maximal model comprised of 17 covariates yielded an AUC of 0.83. Omission of covariates of lesser importance led to a “simplified” model that included performance status, age, platelet count, serum albumin, type of AML (secondary vs. primary), white blood cell count, percentage of blasts in the peripheral blood, and serum creatinine, yielding an AUC of 0.82.")
                        ))),
                            tabItem(tabName = "futuretool",
                                    box(align = "center",
                                             h3("What else would be handy to have right next to this calculator? Could we put it here?")),
                                    )
                    )
)
)

# Define server logic required 
server <- function(input, output, session) {

  calculation <- eventReactive(input$calculateNow, {
        #x = −4.08 + 0.89 × PS + 0.03 × age − 0.008 × platelets − 0.48 × albumin + 0.47 × (have secondary AML) + 0.007 × WBC − 0.007 × (peripheral blood blast percentage) + 0.34 × creatinine.
    round(100/(1+ exp(-(-4.08 + 0.89*as.numeric(input$performance) + 0.03*input$age - 0.008*input$platelets - 0.48*input$albumin + 0.47*ifelse(input$secondaryAML==T,1,0)  + 0.007*input$wbc - 0.007*input$blast + 0.34*input$creatinine))), digits = 4)
       }, ignoreInit = TRUE, ignoreNULL = TRUE)
  output$trmTable <- renderTable(trmData)
  output$trmScore <- renderText({ paste0("The TRM Score is: ", calculation() )})
}

# Run the application 
shinyApp(ui = ui, server = server)
