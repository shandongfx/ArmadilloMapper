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

# function of checking emails
isValidEmail <- function(x) {
  judgement <- grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  check_report <- NULL
  if(!judgement) {check_report <- "Please provide valid email."}
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
  return( var )
}
