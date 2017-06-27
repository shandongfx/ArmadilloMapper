
library(leaflet)
library(shiny)
library(dismo)
library(raster)
library(sp)
library(ggplot2)
library(rJava)

# settings for uploading data
options(shiny.maxRequestSize=30*1024^2) # upload size limited to 30MB
user_path <- paste("userUpload/",format(Sys.time(), "%Y%b%d%H%M"),"",sep="")
user_txt <- file(paste0(user_path,"/user_info.txt"))

# load the Feng et al. (2017) data
load("raw_data_clean/occ")
env <- stack(list.files("raw_data_clean/climate/",pattern = ".bil$",full.names=T))
crs(env) <- crs(occ)
load("raw_data_clean/paper_ped")
load("raw_data_clean/shp_peru") # Peru
load("raw_data_clean/predicted_occ_train") 
original_MTP <- min(predicted_occ_train)
load("raw_data_clean/iucn")
load("raw_data_clean/env_peru")
load("raw_data_clean/env_p")
load("raw_data_clean/env_a")

source("internalFunctions.R",chdir = T)

shinyServer(function(input, output) {
  
  checkocc_click <- eventReactive(input$runrunrun, {
    if(input$tabs==languages$text_man[1] |
       input$tabs==languages$text_man[2]   ){
      
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
      validate(checkData2(man_coord$man_lat_all,man_coord$man_lon_all))
    } else
      if(input$tabs==languages$text_batch[1] |
         input$tabs==languages$text_batch[2]   ){
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep,quote=input$quote)
        validate(checkData2(upload_occ$latitude,upload_occ$longitude))
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
    
    upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep,quote=input$quote)
  })
  
  loadocc_click <- eventReactive(input$runrunrun, {
    if(input$tabs==languages$text_man[1] |
       input$tabs==languages$text_man[2]   ){
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
      if(input$tabs==languages$text_batch[1] |
         input$tabs==languages$text_batch[2]   ){
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        upload_occ <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                               quote=input$quote)
        if(is.null(checkData2(upload_occ$latitude,upload_occ$longitude)) ){
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
    fullM <- dismo::maxent(x=pder,p=pa,path=paste0("maxent_outputs/",maxent_folder1),args=c("responsecurves"))
    # run 2nd model with fewer variables
    best_variables <- varContribution(fullM)
    selected <- filterVariables(layers=env_train,pre.var= best_variables,threshold=0.7 )
    updatedM <- dismo::maxent(x=pder[selected],p=pa,path=paste0("maxent_outputs/",maxent_folder2),args=c("responsecurves"))
    updatedMap_clamp <- predict(env_peru,updatedM)
    new_map <- updatedMap_clamp
    predicted_occ_train <- extract(new_map,occ_unique)
    MTP <- min(predicted_occ_train,na.rm = T)
    new_map[new_map<MTP] <- NA
    new_map
  })
  
  plot_click <- eventReactive(input$runrunrun, {
    mm <- paper_ped
    mm[mm<original_MTP] <- NA
    
    new_ped <- updatemap_click()
    new_occ <- loadocc_click()
    
    crs(new_ped) <- crs(mm)
    
    diff <- env_peru[[1]]
    values(diff) <- NA
    
    mm <- leaflet::projectRasterForLeaflet(mm)
    new_ped <- leaflet::projectRasterForLeaflet(new_ped)
    diff <- leaflet::projectRasterForLeaflet(diff)
    
    diff[(!is.na(values(mm)))  & (!is.na(values(new_ped)))   ] <- 0
    diff[(is.na(values(mm)))  & (!is.na(values(new_ped)))   ] <- 1
    diff[(!is.na(values(mm)))  & (is.na(values(new_ped)))   ] <- -1
    diff[(is.na(values(mm)))  & (is.na(values(new_ped)))   ] <- NA
    
    color3 <- c(c_oldmap,c_overmap,c_newmap)
    labels3 <- factor( c(as.character(languages$text_legend_1[LL]),
                        as.character(languages$text_legend_2[LL]),
                        as.character(languages$text_legend_3[LL]) ),
                      levels=c(as.character(languages$text_legend_1[LL]),
                               as.character(languages$text_legend_2[LL]),
                               as.character(languages$text_legend_3[LL])))
    layerName3 <- c(as.character(languages$text_layer_1[LL]),
                    as.character(languages$text_layer_2[LL]),
                    as.character(languages$text_layer_3[LL]) )
    
    c_diff <- colorFactor(palette= color3,
                          domain=factor(c(-1,0,1)),
                          na.color = "transparent")
    
    new_map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data=Peru,color="black",weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      addPolygons(data=IUCN,color="green",weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      addCircleMarkers(as.numeric(as.character(occ$Longitude)), 
                       as.numeric(as.character(occ$Latitude)),
                       color=c_oldocc,
                       radius=4,
                       opacity = 0.6) %>%
      addCircleMarkers(coordinates(new_occ)[,1], 
                       coordinates(new_occ)[,2],
                       color=c_newocc,
                       radius=4,
                       opacity = 0.6) %>%
      addRasterImage(diff, colors = c_diff, opacity = 1, group = layerName3[2],project=F) %>%
      addRasterImage(new_ped, colors = c_newmap, opacity = 0.7, group = layerName3[3],project=F)  %>%
      addRasterImage(mm, colors = c_oldmap, opacity = 0.7, group = layerName3[1],project=F) %>%
      addLegend(position="bottomleft",
                #pal = c_diff, 
                pal = colorFactor(palette= color3,
                                  domain=labels3,
                                  na.color = "transparent"),
                values = labels3,
                title = paste0(as.character(languages$text_legend_title[LL]),paste0(rep("&nbsp",49),collapse="")  )   )%>%
      setView(lng=-77,lat=-9,zoom=6) %>%
      addLayersControl(
        overlayGroups = c(layerName3),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(layerName3[1:2])
    new_map
  })
  
  plot_click_normal <- eventReactive(input$runrunrun, {
    new_ped <- updatemap_click()
    new_occ <- loadocc_click()
    plot(new_ped,col=c_newmap ,legend=FALSE,
         xlab=languages$text_lon1[LL], ylab=languages$text_lat1[LL])
    plot(Peru,add=T,col=NA,border="black")
    plot(IUCN,add=T,col=NA,border="green")
    plot(occ,add=T,col=c_oldocc,pch=17,cex=1.5)
    plot(new_occ,add=T,col=c_newocc,pch=17,cex=1.5)
  })
  
  output$newMap0 <- renderPlot({
    plot_click_normal()
  })
  
  output$newMap_leaf <- renderLeaflet({
    plot_click()
  })
  
  output$originalMap <- renderPlot({ 
    mm <- paper_ped
    mm[mm<original_MTP] <- NA
    plot(mm,col=c_oldmap ,legend=FALSE,
         xlab=languages$text_lon1[LL], ylab=languages$text_lat1[LL])
    plot(Peru,add=T,col=NA,border="black")
    plot(IUCN,add=T,col=NA,border="green")
    plot(occ,add=T,col=c_oldocc,pch=17,cex=1.5)
  })
  
  # default plot the old map
  output$originalMap_leaf <- renderLeaflet({
    mm <- paper_ped
    mm[mm<original_MTP] <- NA
    leaflet() %>%
      #addProviderTiles(providers$Esri.WorldImagery)%>%
      addTiles() %>%
      addPolygons(data=Peru,color=c_oldocc,weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      addPolygons(data=IUCN,color="green",weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0) %>%
      addCircleMarkers(as.numeric(as.character(occ$Longitude)), 
                       as.numeric(as.character(occ$Latitude)),
                       color=c_oldocc,
                       radius=4,
                       opacity = 0.6) %>%
      addRasterImage(mm, colors = c_oldmap, opacity = 0.7) %>%
      setView(lng=-77,lat=-9,zoom=6) 
    
  })
  
  updateniche_click <- eventReactive(input$runrunrun, {
    new_occ <- loadocc_click()
    p_new <- as.data.frame( extract(env,new_occ),na.rm=T )
    
    p_hull <- chull(p$bio1/10,p$bio12)
    p_all <- rbind(p_new,p)
    p_hull_new <- chull(p_all$bio1/10,p_all$bio12)
    nicheplot_new <- ggplot(aes(x=bio1/10,y=bio12),data=a)+
      geom_point(  colour = "gray",size=2    )  +
      geom_polygon(data = p[p_hull,], alpha = 0.3,fill=c_oldocc ) +
      geom_polygon(data = p_all[p_hull_new,], alpha = 0.3,fill=c_newmap ) +
      geom_point(aes(x=bio1/10,y=bio12),size=2,col=c_oldocc,data=p)+
      geom_point(aes(x=bio1/10,y=bio12),size=2,col=c_newocc,data=p_new)+
      xlab(languages$text_fig3_l1[LL])+
      ylab(languages$text_fig3_l2[LL])+
      theme(legend.position="none")+
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            legend.text = element_text(size = 18))
    nicheplot_new  
  })
  
  output$niche2d_old <- renderPlot({ 
    p_hull <- chull(p$bio1/10,p$bio12)
    nicheplot <- ggplot(aes(x=bio1/10,y=bio12),data=a)+
      geom_point(  colour = "gray",size=2    )  +
      geom_polygon(data = p[p_hull,], alpha = 0.5,fill=c_oldocc ) +
      geom_point(aes(x=bio1/10,y=bio12),size=2,col=c_oldocc,data=p)+
      xlab(languages$text_fig3_l1[LL])+
      ylab(languages$text_fig3_l2[LL])+
      theme(legend.position="none")+
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=18,face="bold"),
            legend.text = element_text(size = 18))
    nicheplot  
  })
  
  output$niche2d_new <- renderPlot({ 
    updateniche_click()
  })

  # user upload data
  observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile))
      return()
    
    if( !file.exists(user_path) ) dir.create(user_path)
    file.copy(inFile$datapath, 
              file.path(user_path, 
                        inFile$name) )
  })
  
  dataset <- reactive({
    # Make sure requirements are met
    req(input$submit_email)
    get(input$datasetName, "package:datasets", inherits = FALSE)
  })
  
  observeEvent(input$subsubsub, {
    if( !file.exists(user_path) ) dir.create(user_path)
    outtext <- paste(input$submit_name,
                     input$submit_email,
                     input$submit_loca,
                     input$submit_longitude,
                     input$submit_latitude,
                     sep="\n")
    cat(text=outtext, file=user_txt,append=TRUE)
  })
    
    checkocc_click <- eventReactive(input$subsubsub, {
        validate(isValidEmail(input$submit_email) )
    })
})
