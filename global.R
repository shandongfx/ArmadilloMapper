languages_raw <- read.csv("raw_data_clean/web translation_v1.csv", header = TRUE, as.is = TRUE) 
languages <- as.data.frame(t(languages_raw[,c("en","sp")]))
colnames(languages) <- languages_raw[,"key"]
LL <- 1 # 1 for English; 2 for Spanish

c_oldmap <- "skyblue"

c_newmap <- "red"
c_overmap <- "gray"

c_oldocc <- "black"
c_newocc <- "green"
