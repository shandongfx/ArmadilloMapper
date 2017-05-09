
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(leaflet)

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(h1(em("Dasypus pilosus"), "niche mapper")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(position = "left",
    sidebarPanel(h4("Please provide your coordinates:"),
                 
                 
                 
                 img(src="dp_photo.jpg", height = 100, width = 200),
                 
                 
                 
                 
      #sliderInput(
      #            min = 5,
      #            max = 50,
      #            value = 30)
    
      tabsetPanel(
        tabPanel("Manual input",
                 #textInput("lon",label="Longitude",value = ""),
                 #textInput("lat",label="Latitude",value = ""),
                 numericInput("lon", "Longitude:", -999, min = -180, max = 180),
                 numericInput("lat", "Latitude:", -999, min = -90, max = 90),
                 #br(),
                 textOutput("results_check"),
                 
                 br(),
                 actionButton("runModel", label = "Update map"),br(),
                 #helpText("update the potential distribution map"),
                 br()
                 
                 ), 
        tabPanel("Batch input",
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
                 textOutput("results_check2"),
                 br(),
                 actionButton("runModel2", label = "Update map"),br(),
                 tableOutput('contents'),
                 #helpText("update the potential distribution map"),
                 br()
                 )
      )
      
      ),


    # Show a plot of the generated distribution
    mainPanel( 
      h4("The potential distribution from Feng et al. 2017:"),
               tabsetPanel(
                 
                 tabPanel("Interactive map",
                          leafletOutput("originalMap_leaf"),
                          h4("Your updated potential distribution:")
                          #leafletOutput("newlMap_leaf"),
 
                 ),
                 tabPanel("Normal map",
                          plotOutput("originalMap"),
                          h4("Your updated potential distribution:"),
                          plotOutput("newMap0")
                          #plotOutput("newMap1"),
                          #plotOutput("newMap2")
                 )
               )
               
    )
  )
))
