
# Call required packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyvalidate)
library(dplyr)
library(gt)
library(gtExtras)

addResourcePath('assets', 'www')

source("ui.R")
source("server.R")

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
