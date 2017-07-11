library(leaflet)
library(shiny)


shinyUI(fluidPage(
  # Application title
  titlePanel(HTML(as.character(languages$text_title[LL]))),

  div(style = "position:absolute;right:0.5em;top:0em",
      HTML(as.character(languages$text_homepage[1])),
      HTML(as.character(languages$text_homepage[2]))
  ),
  
  
  tags$head(includeScript("www/google-analytics.js")),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  img(src="dp_photo.jpg", height = 100, width = 200),
                  actionButton("runrunrun", label = languages$text_buttonupdate[LL] ),#"Update map"),
                  #helpText("Be patient after click this button."),
                  
                  
                  h2(textOutput("error")),
                  
                  tabsetPanel(id = "tabs",
                              
                              tabPanel(languages$text_man[LL],#"Manual input",
                                       id="man",
                                       h2(languages$text_please[LL]),#"Please provide your coordinates:"),
                                       column(6,
                                              numericInput("longitude", languages$text_lon[LL], -78.4492, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("latitude", languages$text_lat[LL], -6.64722, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("longitude2", languages$text_lon[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("latitude2", languages$text_lat[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("longitude3", languages$text_lon[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("latitude3", languages$text_lat[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("longitude4", languages$text_lon[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("latitude4", languages$text_lat[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("longitude5", languages$text_lon[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("latitude5", languages$text_lat[LL], -999, min = -180, max = 180)
                                       ),
                                       br(),
                                       br()
                              ), 
                              tabPanel(languages$text_batch[LL],#"Batch input",
                                       id="bat",
                                       h2(languages$text_please[LL]),#"Please provide your coordinates:"),
                                       fileInput('file1', 
                                                 languages$text_upload3[LL],#'Choose CSV File',
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
                                       tableOutput('contents'),
                                       br()
                              ),
                              tabPanel(languages$text_tabshare[LL],#
                                       #"Share data",
                                       id="share",
                                       h2(languages$text_pleaseshare[LL]),#"Please provide your coordinates:"),
                                       column(6,
                                              textInput("submit_name",
                                                        languages$text_share1[LL],#"Your name:",
                                                        "") 
                                       ),
                                       
                                       column(6,
                                              textInput("submit_email",
                                                        languages$text_share2[LL],#"Email (*required):",
                                                        "")
                                       ),
                                       column(12,
                                              textInput("submit_loca",
                                                        languages$text_share3[LL],#"Locality description:",
                                                        languages$text_share5[LL])#"I observed the species at ...")
                                       ),
                                       
                                       column(6,
                                              numericInput("submit_longitude", languages$text_lon[LL], -999, min = -180, max = 180)
                                       ),
                                       column(6,
                                              numericInput("submit_latitude", languages$text_lat[LL], -999, min = -180, max = 180)
                                       ),
                                       
                                       fileInput("myFile", 
                                                 languages$text_share4[LL],#"If you have more than one records, please upload any related files, e.g. occurrences data, photos, videos.", 
                                                 #accept = c('image/png', 'image/jpeg'),
                                                 multiple=TRUE),
                                       actionButton("subsubsub", 
                                                    label = languages$text_sharebut[LL])#"Submit my data" )
                                       
                              )
                  ),
                  HTML(as.character(languages$text_contact1_html[LL])),
                  HTML(as.character(languages$text_contact2_html[LL]))
                ),
                
                mainPanel(#width=6,
                  tabsetPanel(
                    tabPanel(languages$text_tab1[LL],#"Normal map",
                             column(6,
                                    h4(languages$text_tab1_leg1[LL]),
                                    plotOutput("originalMap")
                             ),
                             column(6,
                                    h4(languages$text_tab1_leg2[LL]),
                                    plotOutput("newMap0")
                             )
                    ),
                    tabPanel(languages$text_tab2[LL],#"Interactive map",
                             column(6,
                                    h4(languages$text_tab1_leg1[LL]),
                                    leafletOutput("originalMap_leaf",
                                                  height = 500)
                             ),
                             column(6,
                                    h4(languages$text_tab1_leg2[LL]),
                                    leafletOutput("newMap_leaf",
                                                  height = 500)
                             )
                    ),
                    tabPanel(languages$text_tab3[LL],#"Climatic niche",
                             column(6,
                                    h4(languages$text_tab3_leg1_2[LL]),
                                    plotOutput("niche2d_old")
                             ),
                             column(6,
                                    h4(languages$text_tab3_leg2_2[LL]),
                                    plotOutput("niche2d_new")
                             )
                    )
                  ),
                  column(12,
                         h4(languages$text_ref[LL]),
                         h5("Castro, M. C., Ciancio, M. R., Pacheco, V., Salas-Gismondi, R. M., Bostelmann, J. E., & Carlini, A. A. (2015). Reassessment of the hairy long-nosed armadillo",em("\"Dasypus\" pilosus"),"(Xenarthra, Dasypodidae) and revalidation of the genus Cryptophractus Fitzinger, 1856. Zootaxa, 3947, 30-48."),
                         h5("Feng, X., Castro, M. C., McBee, K., & Papes, M. (2017). Hiding in a cool climatic niche in the Tropics? An assessment of the ecological biogeography of hairy long-nosed armadillos (",em("Dasypus pilosus"),"). Tropical Conservation Science, 10, 1-13."),
                         h5(languages$text_note1[LL])
                  )
                )
  )
))
