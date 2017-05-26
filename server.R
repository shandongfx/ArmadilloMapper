
library(leaflet)
library(shiny)
library(dismo)
library(raster)
library(sp)
library(ggplot2)
library(plyr)
#library(rJava)

load("raw_data_clean/occ")
#load("raw_data_clean/env")
env <- stack(list.files("raw_data_clean/climate/",pattern = ".bil$",full.names=T))
crs(env) <- crs(occ)
#load("raw_data_clean/raw_prediction")
load("raw_data_clean/raw_prediction_peru") #  map_peru
load("raw_data_clean/shp_peru") # Peru
load("raw_data_clean/predicted_occ_train") 
original_MTP <- min(predicted_occ_train)

load("raw_data_clean/iucn")
load("raw_data_clean/env_peru")
flag_test=FALSE

load("raw_data_clean/env_p")
load("raw_data_clean/env_a")

languages <- read.csv("raw_data_clean/web translation_v1.csv", header = TRUE, as.is = TRUE) 


# function of checking data
checkData2 <- function(in1,in2){
  for(i in length(in1)){
    if(in1[i] > 90 | in1[i] < -90 | 
       in2[i] > 180 | in2[i] < -180 |
       is.na(in1[i]) | is.na(in2[i])){
      check_report <- "Please provide valid coordinates."
      break
    } else {
      check_report <- NULL
    }
  }
  return(check_report)
}

# function of selecting top contributing variables
varContribution <- function (mod,contribution.index=0){
  t <- mod@results
  var.con <- t[ grep(pattern=".contribution",row.names(t) ) , 1]
  var.con.sub <- sort( var.con[var.con>contribution.index] ,decreasing = T)
  pre.var <- gsub(".contribution","",names(var.con.sub))
  return (pre.var)
}

# function of selecting variable based on correlation matrix
filterVariables <- function(layers,pre.var= NULL,threshold ){
  #threshold=0.7
  #pre.var <- best_variables
  #layers <- bg_env
  if( typeof(layers)=="list" ){
    m <- abs(cor(layers))
  } else{
    cor.m <- layerStats(layers,stat="pearson",na.rm=T)  
    m <- abs( cor.m[[1]] )
    m <- as.matrix(m)}
  
  if (!is.null(pre.var) ){
    old.order <- colnames(m)
    new.order <- old.order %in% pre.var
    new.order <- c(pre.var,old.order[!new.order])
    m <- m[new.order,new.order]
  }
  
  if ( !identical(colnames(m) , rownames(m)) ) print("double check the correlation matrix")
  
  var <- colnames(m)
  
  for(i in 1:length(var)){
    if(i >= length(var)) break
    j <- i+1
    v1 <- m[j:length(var),i]
    rm.var <- names(v1[which(v1>threshold)])
    to.rm<- var %in% rm.var
    #var <- var[!to.rm]
    m<- m[!to.rm,!to.rm]
    var <- colnames(m)
  }
  
  #   if( typeof(layers)=="list" ){final <- layers[var] 
  #   } else{ final <-  layers[[colnames(m)]]  }
  
  return( var )
  
}



shinyServer(function(input, output) {
  
  # check manual input
  # testoutput <- eventReactive(input$runrunrun, {
  #   if(input$tabs=="Manual input"){
  #     paste0("You are viewing tab \"", input$tabs, "\"")
  #   } else
  #   if(input$tabs=="Batch input"){
  #     paste0("You are viewing tab \"", input$tabs, "\"")
  #   }
  # })
  # output$sss  <- renderText({
  #   testoutput()
  # })
  checkocc_click <- eventReactive(input$runrunrun, {
    #if(input$tabs=="Manual input"){
    #if(input$id=="man"){
    if(input$tabs==subset(languages,key=="text_man")$en |
       input$tabs==subset(languages,key=="text_man")$sp){
      man_lat_all <- c(input$latitude,
                       input$latitude2,
                       input$latitude3,
                       input$latitude4,
                       input$latitude5 )
      man_lon_all <- c(input$longitude,
                       input$longitude2,
                       input$longitude3,
                       input$longitude4,
                       input$longitude5 )
      man_coord <- as.data.frame( cbind(man_lon_all,man_lat_all) )
      man_coord <- man_coord[which(man_coord$man_lon_all!="-999" &
                                   man_coord$man_lat_all!="-999"),]
      validate(
        checkData2(man_coord$man_lat_all,man_coord$man_lon_all)
      )
    } else
      #if(input$tabs=="Batch input"){
      if(input$tabs==subset(languages,key=="text_batch")$en |
         input$tabs==subset(languages,key=="text_batch")$sp){
          inFile <- input$file1
          
          if (is.null(inFile))
            return(NULL)
          
          upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                                 quote=input$quote)
          validate(
            checkData2(upload_occ$latitude,upload_occ$longitude)
          )
        }
  })
  
  output$error  <- renderText({
    checkocc_click()
  })
  
  # show checked results
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  loadocc_click <- eventReactive(input$runrunrun, {
    #if(input$tabs=="Manual input"){
    #if(input$id=="man"){
    if(input$tabs==subset(languages,key=="text_man")$en |
       input$tabs==subset(languages,key=="text_man")$sp){
      man_lat_all <- c(input$latitude,
                       input$latitude2,
                       input$latitude3,
                       input$latitude4,
                       input$latitude5 )
      man_lon_all <- c(input$longitude,
                       input$longitude2,
                       input$longitude3,
                       input$longitude4,
                       input$longitude5 )
      man_coord <- as.data.frame( cbind(man_lon_all,man_lat_all) )
      man_coord <- man_coord[which(man_coord$man_lon_all!="-999" &
                                     man_coord$man_lat_all!="-999"),]
      
      if(is.null(checkData2(man_coord$man_lat_all,man_coord$man_lon_all)) ){
        
        newocc <- man_coord
        coordinates(newocc) <- ~ man_lon_all + man_lat_all
        crs(newocc) <- crs(env)
        newocc
      } #else("error")
    } else
      #if(input$tabs=="Batch input"){
      if(input$tabs==subset(languages,key=="text_batch")$en |
         input$tabs==subset(languages,key=="text_batch")$sp){
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                               quote=input$quote)
        if(is.null(checkData2(upload_occ$latitude,upload_occ$longitude)) ){
          #update occ
          #Latitude=as.numeric(upload_occ$lat)
          #Longitude=as.numeric(upload_occ$lon)
          #newocc <- data.frame(Longitude,Latitude)
          newocc <- upload_occ
          coordinates(newocc) <- ~ longitude + latitude
          crs(newocc) <- crs(env)
          newocc
        }
    }
    
  })
  
  updatemap_click <- eventReactive(input$runrunrun, {
    new_occ <- loadocc_click()
    occ_all <- occ + new_occ
    cell <- cellFromXY(env[[1]], occ_all)
    dup <- duplicated(cell)
    occ_unique <- occ_all[!dup,]
    training_shp <-  buffer(occ_unique,2)
    prjExtent<- extent(training_shp) 
    prjExtent[1] <- -85
    prjExtent[2] <- -68
    prjExtent[3] <- -18
    prjExtent[4] <- 0
    env_ext <- crop(x = env,y = prjExtent )
    env_train <- mask(env_ext,training_shp )
    #writeRaster(env_ext,filename=paste("climateData/2_5m_studyArea/",names(env_ext),".tif",sep=""),format="GTiff",bylayer=T,overwrite=T)
    #writeRaster(env_ext,filename=paste("climateData/2_5m_studyArea/",names(env_ext),".asc",sep=""),format="ascii",bylayer=T,overwrite=T)
    if (ncell(env_train)>=10000) {bg_cell_num <- 10000} else bg_cell_num<- ncell(env_train)
    set.seed(1) # this will guarantee the same random sample...
    bg <- sampleRandom(x=env_train,size=bg_cell_num,na.rm=T,sp=T)
    # run full Maxent model
    p <- extract(env_train,occ_unique)
    a <- extract(env_train,bg)
    pa <- c(rep(1,nrow(p)), rep(0,nrow(a)))
    pder <- as.data.frame(rbind(p,a))
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M_")
    maxent_folder1 <- paste0(timestamp,"full")
    maxent_folder2 <- paste0(timestamp,"2nd")
    fullM <- dismo::maxent(x=pder,p=pa,path=paste0("d:/projects/2016.4_dasypus_pilosus/cleaned_version_2017_5/maxent_outputs/",maxent_folder1),args=c("responsecurves"))
    # run 2nd model with fewer variables
    best_variables <- varContribution(fullM)
    selected <- filterVariables(layers=env_train,pre.var= best_variables,threshold=0.7 )
    updatedM <- dismo::maxent(x=pder[selected],p=pa,path=paste0("d:/projects/2016.4_dasypus_pilosus/cleaned_version_2017_5/maxent_outputs/",maxent_folder2),args=c("responsecurves"))
    updatedMap_clamp <- predict(env_peru,updatedM)
    new_map <- updatedMap_clamp
    predicted_occ_train <- extract(new_map,occ_unique)
    #new_map <- new_map>=min(predicted_occ_train,na.rm = T)
    MTP <- min(predicted_occ_train,na.rm = T)
    new_map[new_map<MTP] <- NA
    new_map
  })
  
  plot_click <- eventReactive(input$runrunrun, {
    new_ped <- updatemap_click()
    new_occ <- loadocc_click()
    new_map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data=Peru,color="black",weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      addPolygons(data=IUCN,color="green",weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      addCircleMarkers(as.numeric(as.character(occ$Longitude)), 
                       as.numeric(as.character(occ$Latitude)),
                       color="black",
                       radius=4,
                       opacity = 0.6) %>%
      addCircleMarkers(coordinates(new_occ)[,1], 
                       coordinates(new_occ)[,2],
                       color="red",
                       radius=4,
                       opacity = 0.6) %>%
      addRasterImage(new_ped, colors = "skyblue", opacity = 0.5)  %>%
      setView(lng=-77,lat=-9,zoom=6)
    new_map
  })
  
  plot_click_normal <- eventReactive(input$runrunrun, {
    new_ped <- updatemap_click()
    new_occ <- loadocc_click()
    plot(new_ped,col="skyblue" ,legend=FALSE,
         xlab="Longitude", ylab="Latitude")
    plot(Peru,add=T,col=NA,border="black")
    plot(IUCN,add=T,col=NA,border="green")
    plot(occ,add=T,col="black")
    plot(new_occ,add=T,col="red")
  })
  
  output$newMap0 <- renderPlot({
    plot_click_normal()
  })
  
  output$newMap_leaf <- renderLeaflet({
    plot_click()
  })
  
  output$originalMap <- renderPlot({ 
    mm <- map_peru
    mm[mm<original_MTP] <- NA
    plot(mm,col="skyblue" ,legend=FALSE,
         xlab="Longitude", ylab="Latitude")
    plot(Peru,add=T,col=NA,border="black")
    plot(IUCN,add=T,col=NA,border="green")
    plot(occ,add=T,col="black")
  })
  
  # default plot the old map
  output$originalMap_leaf <- renderLeaflet({
    mm <- map_peru
    mm[mm<original_MTP] <- NA
    leaflet() %>%
      #addProviderTiles(providers$Esri.WorldImagery)%>%
      addTiles() %>%
      addPolygons(data=Peru,color="black",weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      addPolygons(data=IUCN,color="green",weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      #addMarkers(as.numeric(as.character(occ$Longitude)), as.numeric(as.character(occ$Latitude))) %>%
      addCircleMarkers(as.numeric(as.character(occ$Longitude)), 
                       as.numeric(as.character(occ$Latitude)),
                       color="black",
                       radius=4,
                       opacity = 0.6) %>%
      addRasterImage(mm, colors = "skyblue", opacity = 0.5) %>%
      setView(lng=-77,lat=-9,zoom=6) 
      
  })
  
  updateniche_click <- eventReactive(input$runrunrun, {
    new_occ <- loadocc_click()
    p_new <- as.data.frame( extract(env,new_occ),na.rm=T )
    
    p_hull <- chull(p$bio1/10,p$bio12)
    p_all <- rbind(p_new,p)
    p_hull_new <- chull(p_all$bio1/10,p_all$bio12)
    nicheplot_new <- ggplot(aes(x=bio1/10,y=bio12),data=a)+
      geom_point(  colour = "gray",size=1    )  +
      geom_polygon(data = p[p_hull,], alpha = 0.3,fill="red" ) +
      geom_polygon(data = p_all[p_hull_new,], alpha = 0.3,fill="blue" ) +
      geom_point(aes(x=bio1/10,y=bio12),size=1,col="red",data=p)+
      geom_point(aes(x=bio1/10,y=bio12),size=1,col="blue",data=p_new)+
      xlab("Annual mean temperature (°C)")+
      ylab("Annual precipitation (mm)")+
      theme(legend.position="none")+
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            legend.text = element_text(size = 18))
      
    
    nicheplot_new  
   })
  
  output$niche2d_old <- renderPlot({ 
    p_hull <- chull(p$bio1/10,p$bio12)
    nicheplot <- ggplot(aes(x=bio1/10,y=bio12),data=a)+
      geom_point(  colour = "gray",size=1    )  +
      geom_polygon(data = p[p_hull,], alpha = 0.3,fill="red" ) +
      geom_point(aes(x=bio1/10,y=bio12),size=1,col="red",data=p)+
      xlab("Annual mean temperature (°C)")+
      ylab("Annual precipitation (mm)")+
      theme(legend.position="none")+
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            legend.text = element_text(size = 18))
    nicheplot  
  })
  
  output$niche2d_new <- renderPlot({ 
    updateniche_click()
  })
  #output$ttt <- renderText({paste0("You are viewing tab \"", input$tabs, "\"")})
  
  # UI
  # translates text into current language
  # tr <- function(text){ # translates text into current language
  #   sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
  # }
  # 
  # output$text_title <- renderUI({
  #   titlePanel( HTML(tr("text_title") ) )
  # })
  # 
  # output$text_buttonupdate <- renderUI({
  #   #actionButton("runrunrun", label = "Update map")
  #   actionButton("runrunrun", label = tr("text_buttonupdate"))
  # })
  # 
  # output$text_please <- renderUI({
  #   #h2("Please provide your coordinates:")
  #   h2(tr("text_please") ) 
  # })
  

  # output$text_title <- renderText({
  #   selectedLanguage <- as.data.frame(t(languages[,input$language]))
  #   colnames(selectedLanguage) <- languages[,"key"]
  #   paste0(selectedLanguage$text_title)
  # })
    
  output$mytitle <- renderUI({
    selectedLanguage <- as.data.frame(t(languages[,input$language]))
    colnames(selectedLanguage) <- languages[,"key"]
    titlePanel(HTML(as.character(selectedLanguage$text_title)))
    #titlePanel(h1(em("Dasypus pilosus's"), "potential distribution in Peru"))
  })
  
  output$wholeUI <- renderUI({
    
    selectedLanguage <- as.data.frame(t(languages[,input$language]))
    colnames(selectedLanguage) <- languages[,"key"]
  
    
    sidebarLayout(position = "left",
                  sidebarPanel(
                    img(src="dp_photo.jpg", height = 100, width = 200),
                    
                    #actionButton("runrunrun", label = "Update map"),
                    actionButton("runrunrun", label = selectedLanguage$text_buttonupdate),
                    
                    #h2("Please provide your coordinates:"),
                    h2(selectedLanguage$text_please),
                    
                    h2(textOutput("error")),
                    
                    tabsetPanel(id = "tabs",
                                tabPanel(selectedLanguage$text_man,#"Manual input",
                                         id="man",
                                         column(6,
                                                numericInput("longitude", 
                                                             selectedLanguage$text_lon,#"Longitude:",
                                                             -78.4492, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("latitude",
                                                             selectedLanguage$text_lat,#"Latitude:",
                                                             -6.64722, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("longitude2", 
                                                             selectedLanguage$text_lon,#"Longitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("latitude2", 
                                                             selectedLanguage$text_lat,#"Latitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("longitude3",
                                                             selectedLanguage$text_lon,#"Longitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("latitude3",
                                                             selectedLanguage$text_lat,#"Latitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("longitude4",
                                                             selectedLanguage$text_lon,#"Longitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("latitude4", 
                                                             selectedLanguage$text_lat,#"Latitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("longitude5", 
                                                             selectedLanguage$text_lon,#"Longitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         column(6,
                                                numericInput("latitude5", 
                                                             selectedLanguage$text_lat,#"Latitude:",
                                                             -999, min = -180, max = 180)
                                         ),
                                         br(),
                                         br()
                                ),
                                tabPanel(selectedLanguage$text_batch,#"Batch input",
                                         id="bat",
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
                                         tableOutput('contents'),
                                         br()
                                )
                    ),
                    #h4(em("Contact Xiao Feng",a("[web]", href="http://www.fengxiao.info"),a("[email]", href="mailto:xiao.feng.armadillo@gmail.com"), "for more information." ))
                    HTML(as.character(selectedLanguage$text_contact1_html)),
                    #h4(em("Not sure about the identification of the specimen? Please contact Mariela Castro ",a("[email]",herf="mailto:marielaccastro@yahoo.com.br"),"."))
                    HTML(as.character(selectedLanguage$text_contact2_html))
                  ),
                  
                  mainPanel(#width=6,
                    tabsetPanel(
                      tabPanel(selectedLanguage$text_tab1,#"Normal map",
                               column(6,
                                      h4(selectedLanguage$text_tab1_leg1),#h4("The potential distribution from Feng et al. 2017:"),
                                      plotOutput("originalMap")
                               ),
                               column(6,
                                      h4(selectedLanguage$text_tab1_leg2),#h4("Your updated potential distribution:"),
                                      plotOutput("newMap0")
                               )
                      ),
                      tabPanel(selectedLanguage$text_tab2,#"Interactive map",
                               column(6,
                                      h4(selectedLanguage$text_tab1_leg1),#h4("The potential distribution from Feng et al. 2017:"),
                                      leafletOutput("originalMap_leaf")
                               ),
                               column(6,
                                      h4(selectedLanguage$text_tab1_leg2),#h4("Your updated potential distribution:"),
                                      leafletOutput("newMap_leaf")
                               )
                      ),
                      tabPanel(selectedLanguage$text_tab3,#"Climatic niche",
                               h4(selectedLanguage$text_tab3_leg1),#h4("Red: climatic niche based on occurrence data in Feng et al. 2017"),
                               h4(selectedLanguage$text_tab3_leg2),#h4("Blue: climatic niche based on new occurrence data"),
                               column(6,
                                      plotOutput("niche2d_old")
                               ),
                               column(6,
                                      plotOutput("niche2d_new")
                               )
                      ),
                      column(12,
                             h4(selectedLanguage$text_ref),#h4("References:"),
                             h5("Castro et al. (2015) Reassessment of the hairy long-nosed armadillo",em("\"Dasypus\" pilosus"),"(Xenarthra, Dasypodidae) and revalidation of the genus", em("Cryptophractus"),"Fitzinger, 1856. Zootaxa, 3947, 30."),
                             h5("Feng et al. (2017) Hiding in a Cool Climatic Niche in the Tropics? An Assessment of the Ecological Biogeography of Hairy Long-Nosed Armadillos (",em("Dasypus pilosus"),"). Tropical Conservation Science, 10, doi:10.1177/1940082917697249."),
                             h5("Note: The potential distribution is made by", a("Maxent algorithm", href="https://www.cs.princeton.edu/~schapire/maxent/"),"(3.3.3k, MIT License).")
                      )
                    )
                  )
    )
  })

    

  
  
})
