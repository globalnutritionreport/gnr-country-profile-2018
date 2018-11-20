####Setup#####
list.of.packages <- c("reshape2","data.table","openxlsx","plyr","gdata","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018/Dataset working directory"
setwd(wd)

dat = read.xlsx("ECONOMICS AND DEMOGRAPHY total pop country and region.xlsx",sheet=1,rows=c(17:290))
population = dat[c("Region,.subregion,.country.or.area.*",as.character(c(1950:2015)))]
names(population)[1] = c("country")
population = melt(population,id.vars="country",variable.name="year")
population$year = as.numeric(unfactor(population$year))
setnames(population,"value","total.pop")
dat = read.xlsx("ECONOMICS AND DEMOGRAPHY total pop country and region.xlsx",sheet=2,rows=c(17:290))
population2 = dat[c("Region,.subregion,.country.or.area.*",as.character(c(2016:2020)))]
names(population2)[1] = c("country")
population2 = melt(population2,id.vars="country",variable.name="year")
population2$year = as.numeric(unfactor(population2$year))
setnames(population2,"value","total.pop")

population = rbind(population,population2)
population = subset(population,country!="Micronesia")

master_countries = read.csv("master_countries.csv",na.strings="")
master_countries = subset(master_countries,!is.na(iso3))
master_countries = unique(master_countries)
names(master_countries) = c("iso3","country")
population = merge(population,master_countries,by="country")
population$country = NULL

master_dat = read.csv("../data.csv",na.strings="",as.is=T)
master_dat$year[which(is.na(master_dat$year))] = 2018
master_dat = merge(master_dat,population,by=c("iso3","year"))

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
      dat_reg = master_dat_sub[,.(
        value=mean(as.numeric(value)),
        value.weighted=weighted.mean(as.numeric(value),total.pop),
        value.sum=sum(as.numeric(value)),
        n=nrow(.SD)
      ),by=.(region,year,indicator,disaggregation,disagg.value,component)]
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
      dat_reg = data.table(master_dat_sub)[,.(
        n=sum(count)
      ),by=.(region,year,indicator,disaggregation,disagg.value,component,value)]
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
      dat_reg = master_dat_sub[,.(
        value=mean(as.numeric(value)),
        n=nrow(.SD)
      ),by=.(subregion,year,indicator,disaggregation,disagg.value,component)]
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
      dat_reg = data.table(master_dat_sub)[,.(
        n=sum(count)
      ),by=.(subregion,year,indicator,disaggregation,disagg.value,component,value)]
      setnames(dat_reg,"subregion","region")
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }
  }
}

master_dat_reg = rbindlist(master_dat_reg_list,fill=T)

write.csv(master_dat_reg,"../data_reg.csv",na="",row.names=F)
