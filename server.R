
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(leaflet)

library(shiny)
library(dismo)
library(raster)
library(sp)
load("raw_data_clean/occ")
load("raw_data_clean/env")
load("raw_data_clean/raw_prediction")
load("raw_data_clean/predicted_occ_train")
load("raw_data_clean/iucn")
flag_test=FALSE

checkData <- function(in1,in2){
  if(in1 > 90 | in1 < -90 | in2 > 180 | in2 < -180 | is.na(in1) | is.na(in2)){
    "Please provide valid coordinates."
  } else {
    NULL
  }
}

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

varContribution <- function (mod,contribution.index=0){
  t <- mod@results
  var.con <- t[ grep(pattern=".contribution",row.names(t) ) , 1]
  var.con.sub <- sort( var.con[var.con>contribution.index] ,decreasing = T)
  pre.var <- gsub(".contribution","",names(var.con.sub))
  return (pre.var)
}
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
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  
  check_results <- eventReactive(input$runModel, {
    validate(
      checkData(input$lat,input$lon)
      #need(input$lat>-90,"errorrrrr")
    )
  })
  
  check_results2 <- eventReactive(input$runModel2, {
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                           quote=input$quote)
    validate(
      checkData2(upload_occ$lat,upload_occ$lon)
      #need(input$lat>-90,"errorrrrr")
    )
  })
  
  loadNewPresence <- eventReactive(input$runModel, {
    if(is.null(  checkData(input$lat,input$lon)) ){
      #update occ
      Latitude=as.numeric(input$lat)
      Longitude=as.numeric(input$lon)
      newocc <- data.frame(Longitude,Latitude)
      coordinates(newocc) <- ~ Longitude + Latitude
      crs(newocc) <- crs(env)
      newocc
      #occ <- newocc+occ
    }
  })
  
  loadNewPresence2 <- eventReactive(input$runModel2, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                           quote=input$quote)
    if(is.null(checkData2(upload_occ$lat,upload_occ$lon)) ){
      #update occ
      #Latitude=as.numeric(upload_occ$lat)
      #Longitude=as.numeric(upload_occ$lon)
      #newocc <- data.frame(Longitude,Latitude)
      newocc <- upload_occ
      coordinates(newocc) <- ~ lon + lat
      crs(newocc) <- crs(env)
      newocc
      #occ <- newocc+occ
    }
  })
  
  updatemap <- eventReactive(input$runModel, {
    if(is.null(  checkData(input$lat,input$lon))){
      
    new_occ <- loadNewPresence()
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
    updatedMap_clamp <- predict(env_ext,updatedM)
    new_map <- updatedMap_clamp
    predicted_occ_train <- extract(new_map,occ_unique)
    new_map <- new_map>=min(predicted_occ_train,na.rm = T)
    }
  })
  updatemap2 <- eventReactive(input$runModel2, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                           quote=input$quote)
    if(is.null(checkData2(upload_occ$lat,upload_occ$lon)) ){
      new_occ <- loadNewPresence2()
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
      updatedMap_clamp <- predict(env_ext,updatedM)
      new_map <- updatedMap_clamp
      predicted_occ_train <- extract(new_map,occ_unique)
      new_map <- new_map>=min(predicted_occ_train,na.rm = T)
    }
  })
  
  output$results_check  <- renderText({
    check_results()
  })
    
  output$results_check2  <- renderText({
    check_results2()
  })
  # default plot the old map
  output$originalMap <- renderPlot({
      plot(map>=min(predicted_occ_train))
      plot(occ,add=T,col="black")
  })
  
  output$originalMap_leaf <- renderLeaflet({
    mm <- map
    mm[mm<predicted_occ_train] <- NA
    pal <- colorNumeric(c("red","green"), values(map),
                        na.color = "transparent")
    leaflet() %>%
    addTiles() %>%
    addPolygons(data=IUCN,weight = 1, smoothFactor = 0.5,
               opacity = 1, fillOpacity = 0) %>%
    addMarkers(as.numeric(as.character(occ$Longitude)), as.numeric(as.character(occ$Latitude))) %>%
    addRasterImage(mm, colors = pal, opacity = 0.5) %>%
    addLegend(pal = pal, values = values(mm),
             title = "Relative probability of presence")
  })
  
  
  output$newMap0 <- renderPlot({
    if(input$runModel != 0 ){
      new_ped <- updatemap()
      new_occ <- loadNewPresence()
      plot( new_ped )
      plot(occ,add=T,col="black")
      plot(new_occ,add=T,col="red")
    }
    if(input$runModel2 != 0){
      new_ped <- updatemap2()
      new_occ <- loadNewPresence2()
      plot( new_ped )
      plot(occ,add=T,col="black")
      plot(new_occ,add=T,col="red")
    }
    
  })
  
  # output$newMap_leaf <- renderLeaflet({
  #   if(input$runModel != 0 ){
  #     new_ped <- updatemap()
  #     new_occ <- loadNewPresence()
  #     
  #     mm <- new_ped
  #     mm[mm<predicted_occ_train] <- NA
  #     pal <- colorNumeric(c("red","green"), values(map),
  #                         na.color = "transparent")
  #     leaflet() %>%
  #       addTiles() %>%
  #       addPolygons(data=IUCN,weight = 1, smoothFactor = 0.5,
  #                   opacity = 1, fillOpacity = 0) %>%
  #       addMarkers(as.numeric(as.character(occ$Longitude)), as.numeric(as.character(occ$Latitude))) %>%
  #       addRasterImage(mm, colors = pal, opacity = 0.5) %>%
  #       addLegend(pal = pal, values = values(mm),
  #                 title = "Relative probability of presence")
  #     plot( new_ped )
  #     plot(occ,add=T,col="black")
  #     plot(new_occ,add=T,col="red")
  #   }
  #   if(input$runModel2 != 0){
  #     new_ped <- updatemap2()
  #     new_occ <- loadNewPresence2()
  #     plot( new_ped )
  #     plot(occ,add=T,col="black")
  #     plot(new_occ,add=T,col="red")
  #   }
  # 
  # })



})
