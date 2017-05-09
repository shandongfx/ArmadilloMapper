
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(leaflet)

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(h1(em("Dasypus pilosus's"), "potential distribution in Peru")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(position = "left",
    sidebarPanel(
      #textOutput("ttt"),
      #textOutput("sss"),
      #actionButton("runrunrun", label = "runrunrun"),br(),
      img(src="dp_photo.jpg", height = 100, width = 200),
      actionButton("runrunrun", label = "Update map"),
      #helpText("Be patient after click this button."),
      
      h2("Please provide your coordinates:"),
      h2(textOutput("error")),
      
      tabsetPanel(id = "tabs",
        tabPanel("Manual input",id="man",
                 #textInput("lon",label="Longitude",value = ""),
                 #textInput("lat",label="Latitude",value = ""),
                 numericInput("lon", "Longitude:", -999, min = -180, max = 180),
                 numericInput("lat", "Latitude:", -999, min = -90, max = 90),
                 
                 br(),
                 br()
                 ), 
        tabPanel("Batch input",id="bat",
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 tags$hr(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"'),
                 br(),
                 #actionButton("runModel2", label = "Update map"),br(),
                 tableOutput('contents'),
                 br()
                 )
      )
      ),
    # Show a plot of the generated distribution
    mainPanel(#width=6,
               tabsetPanel(
                 tabPanel("Normal map",
                          column(6,
                                 h4("The potential distribution from Feng et al. 2017:"),
                                 plotOutput("originalMap")
                          ),
                          column(6,
                                 h4("Your updated potential distribution:"),
                                 plotOutput("newMap0")
                          )
                 ),
                 tabPanel("Interactive map",
                          column(6,
                                 h4("The potential distribution from Feng et al. 2017:"),
                                 leafletOutput("originalMap_leaf")
                          ),
                          column(6,
                                 h4("Your updated potential distribution:"),
                                 leafletOutput("newMap_leaf")
                          )
                 )

               )
               
    )
  )
))
