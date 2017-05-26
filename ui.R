library(leaflet)
library(shiny)

shinyUI(fluidPage(
  # Application title
  #titlePanel(h1(em("Dasypus pilosus's"), "potential distribution in Peru")),
  uiOutput("mytitle"),
  tags$head(includeScript("www/google-analytics.js")),
  
  radioButtons(inputId = "language", label = "",
               choices = c("English" = "en", "Spanish" = "sp"),
               selected = "en"),
  
  uiOutput("wholeUI")
  
  )
)
