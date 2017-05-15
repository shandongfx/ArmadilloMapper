
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
  tags$head(includeScript("www/google-analytics.js")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(position = "left",
    sidebarPanel(
      #textOutput("ttt"),
      #textOutput("sss"),
      #actionButton("runrunrun", label = "runrunrun"),br(),
      img(src="dp_photo.jpg", height = 100, width = 200),
      actionButton("runrunrun", label = "Update map"),
      #actionButton("but_niche", label = "Update niche"),
      
      #helpText("Be patient after click this button."),
      
      h2("Please provide your coordinates:"),
      h2(textOutput("error")),
      
      tabsetPanel(id = "tabs",
        tabPanel("Manual input",id="man",
                 #textInput("lon",label="Longitude",value = ""),
                 #textInput("lat",label="Latitude",value = ""),
                 #numericInput("longitude", "Longitude:", -78.4492, min = -180, max = 180),
                 #numericInput("latitude", "Latitude:", -6.64722, min = -90, max = 90),
                 column(6,
                        numericInput("longitude", "Longitude:", -78.4492, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("latitude", "Latitude:", -6.64722, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("longitude2", "Longitude:", -999, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("latitude2", "Latitude:", -999, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("longitude3", "Longitude:", -999, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("latitude3", "Latitude:", -999, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("longitude4", "Longitude:", -999, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("latitude4", "Latitude:", -999, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("longitude5", "Longitude:", -999, min = -180, max = 180)
                 ),
                 column(6,
                        numericInput("latitude5", "Latitude:", -999, min = -180, max = 180)
                 ),
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
      ),
      #h3("Contact Xiao.Feng@okstate.edu for more information."),
      h4(em("Contact Xiao Feng",a("[web]", href="http://www.fengxiao.info"),a("[email]", href="mailto:xiao.feng.armadillo@gmail.com"), "for more information." ))
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
                 ),
                 tabPanel("Climatic niche",
                          #column(12,
                          #       )
                          h4("Red: climatic niche based on occurrence data in Feng et al. 2017"),
                          h4("Blue: climatic niche based on new occurrence data"),
                          column(6,
                                 #h4("climatic niche"),
                                 plotOutput("niche2d_old")
                          ),
                          column(6,
                                 #h4("climatic niche"),
                                 plotOutput("niche2d_new")
                          )

                 ),
                 column(12,
                        #br(),br(),
                        h4("References:"),
                        h5("Castro et al. (2015) Reassessment of the hairy long-nosed armadillo",em("\"Dasypus\" pilosus"),"(Xenarthra, Dasypodidae) and revalidation of the genus", em("Cryptophractus"),"Fitzinger, 1856. Zootaxa, 3947, 30."),
                        h5("Feng et al. (2017) Hiding in a Cool Climatic Niche in the Tropics? An Assessment of the Ecological Biogeography of Hairy Long-Nosed Armadillos (",em("Dasypus pilosus"),"). Tropical Conservation Science, 10, doi:10.1177/1940082917697249.")
                 )
                        
               )
               
    )
  )
))
