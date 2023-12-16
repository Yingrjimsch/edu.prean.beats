library("shiny")

# Source UI and Server scripts
source("ui.R")
source("server.R")

print(shinyApp(ui = ui, server = server))




