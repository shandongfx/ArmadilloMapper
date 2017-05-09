
# load(file="tempData/occ")
# env <- list.files("climateData/2_5m/",pattern="bio*.*.bil$",full.names=T)
# env <- stack(env)
# crs(occ) <- crs(env)
# save(occ,file="../cleaned_version_2017_5/raw_data_clean/occ")
# save(env,file="../cleaned_version_2017_5/raw_data_clean/env")
setwd("d:/projects/2016.4_dasypus_pilosus/cleaned_version_2017_5")
load("raw_data_clean/occ")
load("raw_data_clean/env")

######## process new occ
newocc <- read.csv("raw_data_clean/test_input.csv")
#newocc <- cbind(lat=10,lon=10)
#newOcc <- as.data.frame(newocc)
coordinates(newocc) <- ~ Latitude + Longitude
crs(newocc) <- crs(env)
occ <- newocc+occ


########

cell <- cellFromXY(env[[1]], occ)
dup <- duplicated(cell)
occ_unique <- occ[!dup,]

# generate buffer & bg
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
# plot(bg)
# save(bg,file="tempData/bg.temp")
#load("tempData/bg.temp")


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

best_variables <- varContribution(fullM)
selected <- filterVariables(layers=env_train,pre.var= best_variables,threshold=0.7 )

updatedM <- dismo::maxent(x=pder[selected],p=pa,path=paste0("d:/projects/2016.4_dasypus_pilosus/cleaned_version_2017_5/maxent_outputs/",maxent_folder2),args=c("responsecurves"))
updatedMap_clamp <- predict(env_ext,updatedM)

map <- updatedMap_clamp
predicted_occ_train <- extract(map,occ_unique)

# plot map
plot(updatedMap_clamp)

plot(updatedMap_clamp>=min(predicted_occ_train))
