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

master_dat_reg_list = list()
master_dat_reg_index = 1

indicators = unique(master_dat$indicator)
for(this.indicator in indicators){
  master_dat_sub = subset(master_dat,indicator==this.indicator)
  master_dat_sub = master_dat_sub[complete.cases(master_dat_sub$value),]
  master_dat_sub = data.table(master_dat_sub)
  if(nrow(master_dat_sub)>0){
    if(numericable(master_dat_sub$value)){
      dat_reg = master_dat_sub[,.(value=mean(as.numeric(value)),n=nrow(.SD)),by=.(region,year,indicator,disaggregation,disagg.value,component)]
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }else{
      uni.vals = unique(master_dat_sub$value)
      master_dat_sub$count = 1
      for(uni.val in uni.vals){
        master_dat_clone = master_dat_sub
        master_dat_clone$value = uni.val
        master_dat_clone$count = 0 
        master_dat_sub = rbind(master_dat_sub,master_dat_clone)
      }
      dat_reg = data.table(master_dat_sub)[,.(n=sum(count)),by=.(region,year,indicator,disaggregation,disagg.value,component,value)]
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }
  }
}

indicators = unique(master_dat$indicator)
for(this.indicator in indicators){
  master_dat_sub = subset(master_dat,indicator==this.indicator)
  master_dat_sub = master_dat_sub[complete.cases(master_dat_sub$value),]
  master_dat_sub = data.table(master_dat_sub)
  if(nrow(master_dat_sub)>0){
    if(numericable(master_dat_sub$value)){
      dat_reg = master_dat_sub[,.(value=mean(as.numeric(value)),n=nrow(.SD)),by=.(subregion,year,indicator,disaggregation,disagg.value,component)]
      setnames(dat_reg,"subregion","region")
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }else{
      uni.vals = unique(master_dat_sub$value)
      master_dat_sub$count = 1
      for(uni.val in uni.vals){
        master_dat_clone = master_dat_sub
        master_dat_clone$value = uni.val
        master_dat_clone$count = 0 
        master_dat_sub = rbind(master_dat_sub,master_dat_clone)
      }
      dat_reg = data.table(master_dat_sub)[,.(n=sum(count)),by=.(subregion,year,indicator,disaggregation,disagg.value,component,value)]
      setnames(dat_reg,"subregion","region")
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }
  }
}

master_dat_reg = rbindlist(master_dat_reg_list)

write.csv(master_dat_reg,"../data_reg.csv",na="",row.names=F)
