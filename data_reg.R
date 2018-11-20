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
master_dat$year_range = ""

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

latest.year.inds = c("coexistence","physicians")

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
        master_dat_sub$year = max(master_dat_sub$year,na.rm=T)
        year.min = min(master_dat_sub$year,na.rm=T)
        year.max = max(master_dat_sub$year,na.rm=T)
        if(year.min==year.max){
          master_dat_sub$year_range = master_dat_sub$year
        }else{
          master_dat_sub$year_range = paste(year.min,year.max,sep="–")
        }
        
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
      ),by=.(region,year,indicator,disaggregation,disagg.value,component,rec,unit,year_range)]
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
        master_dat_sub$year = max(master_dat_sub$year,na.rm=T)
        year.min = min(master_dat_sub$year,na.rm=T)
        year.max = max(master_dat_sub$year,na.rm=T)
        if(year.min==year.max){
          master_dat_sub$year_range = master_dat_sub$year
        }else{
          master_dat_sub$year_range = paste(year.min,year.max,sep="–")
        }
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
      ),by=.(subregion,year,indicator,disaggregation,disagg.value,component,rec,unit,year_range)]
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
master_dat_class = unique(master_dat_reg[,c("region","regional")])
master_dat_class_list = master_dat_class$regional
names(master_dat_class_list) = master_dat_class$region

# Three year avgs for under5s
indicators = c("stunting_percent","overweight_percent")
to_average = subset(master_dat_reg,indicator %in% indicators)
master_dat_reg = subset(master_dat_reg,!indicator %in% indicators)
year_seq = c(1999,2005,2010,2016)
to_average$old.year = to_average$year
to_average$year = NA
for(i in 2:length(year_seq)){
  start = year_seq[i-1]
  end = year_seq[i]
  if(i==length(year_seq)){
    to_average$year[which(to_average$old.year>=start & to_average$old.year<=end)] = round((end+start)/2)
  }else{
    to_average$year[which(to_average$old.year>=start & to_average$old.year<end)] = round((end+start)/2)
  }
}
to_average = to_average[,.(
  value.unweighted=mean(as.numeric(value)),
  value=weighted.mean(as.numeric(value),total.pop),
  value.sum=sum(as.numeric(value)),
  total.pop=sum(as.numeric(total.pop)),
  n=sum(n)
),by=.(region,year,indicator,disaggregation,disagg.value,component,rec,unit,year_range)]
to_average$regional = master_dat_class_list[to_average$region]
master_dat_reg = rbindlist(list(master_dat_reg,to_average),fill=T)


# Fix data here
ex.num <- function(s){
  # Uppercase
  s_upper <- toupper(s)
  # Convert string to a vector of single letters
  s_split <- unlist(strsplit(s_upper, split=""))
  # Convert each letter to the corresponding number
  s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
  # Derive the numeric value associated with each letter
  numbers <- 26^((length(s_number)-1):0)
  # Calculate the column number
  column_number <- sum(s_number * numbers)
  column_number
}
ex.num <- Vectorize(ex.num)

master_dat_fix_list = list()
master_dat_fix_index = 1

wd <- "~/git/gnr-country-profile-2018/Dataset working directory_reg"
setwd(wd)

master_dat_reg = subset(master_dat_reg,indicator!="coexistence")
coexistence = read.xlsx("CHILD STATUS coexistence.xlsx")
names(coexistence) = c(
  "region",
  "Wasting alone",
  "Wasting and stunting",
  "Stunting alone",
  "Stunting and overweight",
  "Overweight alone",
  "Free from",
  "n"
)
coexistence = subset(coexistence,!is.na(region))
coexistence = melt(coexistence,id.vars=c("region","n"),variable.name="disagg.value")
coexistence$indicator = "coexistence"
coexistence$disaggregation = "all"
coexistence$component = "G"
coexistence$value = coexistence$value*100
# unique(coexistence$region) %in% unique(master_dat_reg$region)
master_dat_fix_list[[master_dat_fix_index]] = coexistence
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!(indicator=="stunting_percent" & disagg.value=="Both"))
stunting = read.xlsx(
  "CHILD STATUS U5.xlsx",
  sheet=1,
  rows=c(7:29),
  cols=ex.num(c("a","m","r","w","ab","ag","al","aq","av","ba","bf")),
  na.strings="-"
)
names(stunting) = c(
  "region",
  "2000","2005","2010","2011","2012","2013","2014","2015","2016","2017"
)
stunting$region = gsub('[0-9]+', '', stunting$region)
stunting$region[which(stunting$region=="Latin American and Caribbean")] = "Latin America and Caribbean"
stunting = subset(stunting,region %in% unique(master_dat_reg$region))
stunting = melt(stunting,id.vars="region",variable.name="year")
stunting$indicator = "stunting_percent"
stunting$component = "C"
stunting$disaggregation = "gender"
stunting$disagg.value = "Both"
# unique(stunting$region) %in% unique(master_dat_reg$region)
master_dat_fix_list[[master_dat_fix_index]] = stunting
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!(indicator=="overweight_percent" & disagg.value=="Both"))
overweight = read.xlsx(
  "CHILD STATUS U5.xlsx",
  sheet=3,
  rows=c(7:29),
  cols=ex.num(c("a","m","r","w","ab","ag","al","aq","av","ba","bf")),
  na.strings="-"
)
names(overweight) = c(
  "region",
  "2000","2005","2010","2011","2012","2013","2014","2015","2016","2017"
)
overweight$region = gsub('[0-9]+', '', overweight$region)
overweight$region[which(overweight$region=="Latin American and Caribbean")] = "Latin America and Caribbean"
overweight = subset(overweight,region %in% unique(master_dat_reg$region))
overweight = melt(overweight,id.vars="region",variable.name="year")
overweight$indicator = "overweight_percent"
overweight$component = "C"
overweight$disaggregation = "gender"
overweight$disagg.value = "Both"
# unique(overweight$region) %in% unique(master_dat_reg$region)
master_dat_fix_list[[master_dat_fix_index]] = overweight
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!(indicator=="wasting_percent" & disagg.value=="Both"))
wasting = read.xlsx(
  "CHILD STATUS U5.xlsx",
  sheet=5,
  rows=c(7:29),
  cols=ex.num(c("a","c")),
  na.strings="-"
)
names(wasting) = c(
  "region",
  "2017"
)
wasting$region = gsub('[0-9]+', '', wasting$region)
wasting$region[which(wasting$region=="Latin American and Caribbean")] = "Latin America and Caribbean"
wasting = subset(wasting,region %in% unique(master_dat_reg$region))
wasting = melt(wasting,id.vars="region",variable.name="year")
wasting$indicator = "wasting_percent"
wasting$component = "C"
wasting$disaggregation = "gender"
wasting$disagg.value = "Both"
# unique(wasting$region) %in% unique(master_dat_reg$region)
master_dat_fix_list[[master_dat_fix_index]] = wasting
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="u5mr")
u5mr = read.xlsx(
  "DEMOGRAPHY U5 mort.xlsx",
  rows=c(17,19,20,71,92,102,92,110,116,133,134,140,149,161,171,190,191,202,214,227,235,236,254
         ,263,277,280,281,284,290,294)
  ,cols=c(ex.num("c"),ex.num("p"):ex.num("s"))
)
names(u5mr) = c("region",seq(2000,2015,5))
u5mr = melt(u5mr,id.vars="region",variable.name="year")
u5mr$year = unfactor(u5mr$year)
u5mr$region[which(u5mr$region=="Sub-Saharan Africa")] = "Southern Africa"
u5mr$region[which(u5mr$region=="South-Eastern Asia")] = "South-eastern Asia"
u5mr$region[which(u5mr$region=="Australia/New Zealand")] = "Australia and New Zealand"
u5mr$component = "R"
u5mr$indicator = "u5mr"
u5mr$disaggregation = "all"
master_dat_fix_list[[master_dat_fix_index]] = u5mr
master_dat_fix_index = master_dat_fix_index + 1

#TODO: pickup here "DIETARY_NEEDS_2016_regional.csv" 

master_dat_fix = rbindlist(master_dat_fix_list,fill=T)
master_dat_fix$regional = master_dat_class_list[master_dat_fix$region]
master_dat_reg = rbindlist(list(master_dat_reg,master_dat_fix),fill=T)

write.csv(master_dat_reg,"../data_reg.csv",na="",row.names=F)
