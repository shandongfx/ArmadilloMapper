# launch my app
setwd("d:/projects/2017_4_dasypus_web/Dasypus_web/")
library(shiny)
library(leaflet)
runApp("../Dasypus_web")
runApp("../Dasypus_web",display.mode="showcase")


library(plyr)

languages <- read.csv("raw_data_clean/web translation_v1.csv", header = TRUE, as.is = TRUE) 
selectedLanguage <- as.data.frame(t(languages[,"id"]))
colnames(selectedLanguage) <- languages[,"key"]
selectedLanguage$text_species

