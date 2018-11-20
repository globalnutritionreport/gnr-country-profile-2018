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

must_sum_to_100s = c("basic_water","limited_water","safely_managed_water","surface_water","unimproved_water",
                     "basic_sanitation","limited_sanitation","open_defecation","safely_managed_sanitation","unimproved_sanitation")
must_sum_to_100s_sub = subset(master_dat,indicator %in% must_sum_to_100s)
country.years = unique(must_sum_to_100s_sub[,c("iso3","country","region","subregion","year","total.pop")])

latest.year.inds = c("coexistence")

just.recips = c("ODA_received","ODA_specific")

indicators = unique(master_dat$indicator)
for(this.indicator in indicators){
  master_dat_sub = subset(master_dat,indicator==this.indicator)
  master_dat_sub = master_dat_sub[complete.cases(master_dat_sub$value),]
  master_dat_sub = data.table(master_dat_sub)
  if(nrow(master_dat_sub)>0){
    if(numericable(master_dat_sub$value)){
      if(this.indicator %in% must_sum_to_100s){
        # Ensure we have a 0 to mean
        for(i in 1:nrow(country.years)){
          row = country.years[i,]
          master_dat_sub_sub = subset(master_dat_sub,country==row$country[1] & year==row$year[1] )
          if(nrow(master_dat_sub_sub)==0){
            dummy_row = row
            dummy_row$value = 0
            dummy_row$disaggregation = "all"
            dummy_row$component = "V"
            dummy_row$indicator = this.indicator
            master_dat_sub = rbindlist(list(master_dat_sub,dummy_row),fill=T)
          }
        }
      }
      if(this.indicator %in% latest.year.inds){
        # Only take latest year for each combo
        master_dat_sub = master_dat_sub[master_dat_sub[,.I[year==max(year)],by=.(country,indicator,disaggregation,disagg.value)]$V1]
        master_dat_sub$year = paste(min(master_dat_sub$year,na.rm=T),max(master_dat_sub$year,na.rm=T),sep="–")
      }
      if(this.indicator %in% just.recips){
        master_dat_sub = subset(master_dat_sub,recip)
      }
      dat_reg = master_dat_sub[,.(
        value.unweighted=mean(as.numeric(value)),
        value=weighted.mean(as.numeric(value),total.pop),
        value.sum=sum(as.numeric(value)),
        total.pop=sum(as.numeric(total.pop)),
        n=nrow(.SD)
      ),by=.(region,year,indicator,disaggregation,disagg.value,component,rec,unit)]
      dat_reg$regional = 1
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
        total.pop=sum(as.numeric(total.pop)),
        n=sum(count)
      ),by=.(region,year,indicator,disaggregation,disagg.value,component,value)]
      dat_reg_N = data.table(master_dat_sub)[,.(
        N=sum(count)
      ),by=.(region,year,indicator,disaggregation,disagg.value,component)]
      dat_reg = merge(dat_reg,dat_reg_N)
      dat_reg$regional = 1
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }
  }
}

for(this.indicator in indicators){
  master_dat_sub = subset(master_dat,indicator==this.indicator)
  master_dat_sub = master_dat_sub[complete.cases(master_dat_sub$value),]
  master_dat_sub = data.table(master_dat_sub)
  if(nrow(master_dat_sub)>0){
    if(numericable(master_dat_sub$value)){
      if(this.indicator %in% must_sum_to_100s){
        # Ensure we have a 0 to mean
        for(i in 1:nrow(country.years)){
          row = country.years[i,]
          master_dat_sub_sub = subset(master_dat_sub,country==row$country[1] & year==row$year[1] )
          if(nrow(master_dat_sub_sub)==0){
            dummy_row = row
            dummy_row$value = 0
            dummy_row$disaggregation = "all"
            dummy_row$component = "V"
            dummy_row$indicator = this.indicator
            master_dat_sub = rbindlist(list(master_dat_sub,dummy_row),fill=T)
          }
        }
      }
      if(this.indicator %in% latest.year.inds){
        # Only take latest year for each combo
        master_dat_sub = master_dat_sub[master_dat_sub[,.I[year==max(year)],by=.(country,indicator,disaggregation,disagg.value)]$V1]
        master_dat_sub$year = paste(min(master_dat_sub$year,na.rm=T),max(master_dat_sub$year,na.rm=T),sep="–")
      }
      if(this.indicator %in% just.recips){
        master_dat_sub = subset(master_dat_sub,recip)
      }
      dat_reg = master_dat_sub[,.(
        value.unweighted=mean(as.numeric(value)),
        value=weighted.mean(as.numeric(value),total.pop),
        value.sum=sum(as.numeric(value)),
        total.pop=sum(as.numeric(total.pop)),
        n=nrow(.SD)
      ),by=.(subregion,year,indicator,disaggregation,disagg.value,component,rec,unit)]
      dat_reg$regional = 0
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
        total.pop=sum(as.numeric(total.pop)),
        n=sum(count)
      ),by=.(subregion,year,indicator,disaggregation,disagg.value,component,value)]
      dat_reg_N = data.table(master_dat_sub)[,.(
        N=sum(count)
      ),by=.(subregion,year,indicator,disaggregation,disagg.value,component)]
      dat_reg = merge(dat_reg,dat_reg_N)
      dat_reg$regional = 0
      setnames(dat_reg,"subregion","region")
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }
  }
}

master_dat_reg = rbindlist(master_dat_reg_list,fill=T)

write.csv(master_dat_reg,"../data_reg.csv",na="",row.names=F)
