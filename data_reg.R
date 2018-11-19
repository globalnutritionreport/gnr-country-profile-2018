####Setup#####
list.of.packages <- c("reshape2","data.table","openxlsx","plyr","gdata","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018/Dataset working directory"
setwd(wd)

master_dat = read.csv("../data.csv",na.strings="",as.is=T)

numericable = function(vec){
  vec = vec[complete.cases(vec)]
  num.vec = as.numeric(vec)
  num.vec = num.vec[complete.cases(num.vec)]
  if(length(num.vec)==length(vec)){
    return(T)
  }
  return(F)
}

adaptive_mean = function(vec){
  vec = vec[complete.cases(vec)]
  if(numericable(vec)){
    return(
      list(
        "numeric"=T,
        "n"=length(vec),
        "mean"=mean(as.numeric(vec))
      )
    )
  }
  t = table(vec)
  l = list()
  tnames = names(t)
  for(tname in tnames){
    l[[tname]] = t[tname][[1]]
  }
  return(
    list(
      "numeric"=F,
      "n"=length(vec),
      "counts"=l
    )
  )
}

indicators = unique(master_dat$indicator)
for(indicator in indicators){
  message(indicator)
  vec = master_dat[,indicator]
  am = adaptive_mean(vec)
}