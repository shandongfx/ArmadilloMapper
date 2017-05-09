
library(leaflet)
library(shiny)
library(dismo)
library(raster)
library(sp)
load("raw_data_clean/occ")
load("raw_data_clean/env")
#load("raw_data_clean/raw_prediction")
load("raw_data_clean/raw_prediction_peru") #  map_peru
load("raw_data_clean/shp_peru") # Peru
load("raw_data_clean/predicted_occ_train") 
load("raw_data_clean/iucn")
load("raw_data_clean/env_peru")
flag_test=FALSE

# function of checking data
checkData <- function(in1,in2){
  if(in1 > 90 | in1 < -90 | in2 > 180 | in2 < -180 | is.na(in1) | is.na(in2)){
    "Please provide valid coordinates."
  } else {
    NULL
  }
}
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
  testoutput <- eventReactive(input$runrunrun, {
    if(input$tabs=="Manual input"){
      paste0("You are viewing tab \"", input$tabs, "\"")
    } else
    if(input$tabs=="Batch input"){
      paste0("You are viewing tab \"", input$tabs, "\"")
    }
  })
  output$sss  <- renderText({
    testoutput()
  })
  checkocc_click <- eventReactive(input$runrunrun, {
    if(input$tabs=="Manual input"){
      validate(
        checkData(input$lat,input$lon)
      )
    } else
      if(input$tabs=="Batch input"){
          inFile <- input$file1
          
          if (is.null(inFile))
            return(NULL)
          
          upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                                 quote=input$quote)
          validate(
            checkData2(upload_occ$lat,upload_occ$lon)
          )
        }
  })
  
  output$error  <- renderText({
    checkocc_click()
  })
  # show checked results
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
  
  loadocc_click <- eventReactive(input$runrunrun, {
    if(input$tabs=="Manual input"){
      if(is.null(  checkData(input$lat,input$lon)) ){
        #update occ
        lat=as.numeric(input$lat)
        lon=as.numeric(input$lon)
        newocc <- data.frame(lon,lat)
        coordinates(newocc) <- ~ lon + lat
        crs(newocc) <- crs(env)
        newocc
      }
    } else
      if(input$tabs=="Batch input"){
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
      addRasterImage(new_ped, colors = "skyblue", opacity = 0.9) 
    new_map
  })
  
  output$newMap_leaf <- renderLeaflet({
    plot_click()
  })
  
  output$newMap0 <- renderPlot({
    if(input$runrunrun != 0 ){
      new_ped <- updatemap_click()
      new_occ <- loadocc_click()
      plot(new_ped,col=c(NA,"skyblue") ,legend=FALSE)
      plot(Peru,add=T,col=NA,border="black")
      plot(IUCN,add=T,col=NA,border="green")
      plot(occ,add=T,col="black")
      plot(new_occ,add=T,col="red")
    }
  })

 
  output$originalMap <- renderPlot({ 
      plot(map_peru>=min(predicted_occ_train),col=c(NA,"skyblue") ,legend=FALSE)
      plot(Peru,add=T,col=NA,border="black")
      plot(IUCN,add=T,col=NA,border="green")
      plot(occ,add=T,col="black")
  })
  
  # default plot the old map
  output$originalMap_leaf <- renderLeaflet({
    mm <- map_peru
    mm[mm<predicted_occ_train] <- NA
    leaflet() %>%
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
      addRasterImage(mm, colors = "skyblue", opacity = 0.9) 
  })
  
  #output$ttt <- renderText({paste0("You are viewing tab \"", input$tabs, "\"")})
  
})
