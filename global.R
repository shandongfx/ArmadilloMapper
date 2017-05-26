languages_raw <- read.csv("raw_data_clean/web translation_v1.csv", header = TRUE, as.is = TRUE) 
languages <- as.data.frame(t(languages_raw[,c("en","sp")]))
colnames(languages) <- languages_raw[,"key"]
LL <- 1
