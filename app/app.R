# Call required packages
library(shiny)

# Source relevant scripts
source("ui.R")
source("server.R")

# Set options for the Shiny application
options <- list()
if (!interactive()) {
  options$port = 3838
  options$launch.browser = FALSE
  options$host = "0.0.0.0"
}

# Run the application 
shinyApp(
  ui, 
  server, 
  options = options
)
