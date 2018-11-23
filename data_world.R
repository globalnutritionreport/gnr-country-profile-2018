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

must_sum_to_100s = c("basic_water","limited_water","safely_managed_water","surface_water","unimproved_water"
                     ,"basic_sanitation","limited_sanitation","open_defecation","safely_managed_sanitation","unimproved_sanitation"
                    )

latest.year.inds = c("coexistence",
                     "physicians",
                     "nurses_and_midwives",
                     "community_health_workers",
                     "early_childbearing_prev",
                     "diarrhea_zinc","vit_a","iron_supp","iron_and_folic","iodised_salt")

just.recips = c("ODA_received","ODA_specific")

# three year avgs
indicators = c(
  "stunting_percent",
  "overweight_percent",
  "continued_breastfeeding_2yr",
  "continued_breastfeeding_1yr",
  "minimum_accept_diet",
  "minimum_diet_diversity",
  "minimum_meal",
  "solid_foods",
  "exclusive_breastfeeding",
  "early_initiation"
)
to_average = data.table(subset(master_dat,indicator %in% indicators))
master_dat = subset(master_dat,!indicator %in% indicators)
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
to_average$old.year = NULL

master_dat = rbind(master_dat,to_average)

indicators = unique(master_dat$indicator)
for(this.indicator in indicators){
  master_dat_sub = subset(master_dat,indicator==this.indicator)
  master_dat_sub = master_dat_sub[complete.cases(master_dat_sub$value),]
  master_dat_sub = data.table(master_dat_sub)
  if(nrow(master_dat_sub)>0){
    if(numericable(master_dat_sub$value)){
      if(this.indicator %in% must_sum_to_100s){
        # Multiply by population
        master_dat_sub$value = (as.numeric(master_dat_sub$value)/100)*master_dat_sub$total.pop
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
        n=length(unique(iso3))
      ),by=.(year,indicator,disaggregation,disagg.value,component,rec,unit,year_range)]
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
      ),by=.(year,indicator,disaggregation,disagg.value,component,value)]
      dat_reg_N = data.table(master_dat_sub)[,.(
        N=sum(count)
      ),by=.(year,indicator,disaggregation,disagg.value,component)]
      dat_reg = merge(dat_reg,dat_reg_N)
      master_dat_reg_list[[master_dat_reg_index]] = dat_reg
      master_dat_reg_index = master_dat_reg_index + 1
    }
  }
}

master_dat_reg = rbindlist(master_dat_reg_list,fill=T)
master_dat_reg$region = "World"

# Ensure that some vars sum to 100
water = subset(master_dat_reg,indicator %in% must_sum_to_100s[1:5])
master_dat_reg = subset(master_dat_reg,!indicator %in% must_sum_to_100s[1:5])
water[,water.sum:=sum(.SD$value.sum),by=.(region,year)]
water$value = (water$value.sum/water$water.sum)*100
water$value.sum = NA
water$value.unweighted = NA
water$water.sum = NULL
master_dat_reg = rbind(master_dat_reg,water)

sanitation = subset(master_dat_reg,indicator %in% must_sum_to_100s[6:10])
master_dat_reg = subset(master_dat_reg,!indicator %in% must_sum_to_100s[6:10])
sanitation[,sanitation.sum:=sum(.SD$value.sum),by=.(region,year)]
sanitation$value = (sanitation$value.sum/sanitation$sanitation.sum)*100
sanitation$value.sum = NA
sanitation$value.unweighted = NA
sanitation$sanitation.sum = NULL
master_dat_reg = rbind(master_dat_reg,sanitation)


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

# master_dat_reg = subset(master_dat_reg,indicator!="coexistence")
# coexistence = read.xlsx("CHILD STATUS coexistence.xlsx")
# names(coexistence) = c(
#   "region",
#   "Wasting alone",
#   "Wasting and stunting",
#   "Stunting alone",
#   "Stunting and overweight",
#   "Overweight alone",
#   "Free from",
#   "n"
# )
# coexistence = subset(coexistence,!is.na(region))
# coexistence = melt(coexistence,id.vars=c("region","n"),variable.name="disagg.value")
# coexistence$indicator = "coexistence"
# coexistence$disaggregation = "all"
# coexistence$component = "G"
# coexistence$value = coexistence$value*100
# # unique(coexistence$region) %in% unique(master_dat_reg$region)
# master_dat_fix_list[[master_dat_fix_index]] = coexistence
# master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!(indicator=="stunting_percent" & disagg.value %in% c("Both","Girls","Boys")))
stunting = read.xlsx(
  "CHILD STATUS U5.xlsx",
  sheet=1,
  rows=c(3:5),
  cols=ex.num(c("a","m","r","w","ab","ag","al","aq","av","ba","bf")),
  na.strings="-"
)
names(stunting) = c(
  "region",
  "2000","2005","2010","2011","2012","2013","2014","2015","2016","2017"
)
stunting$region = "World"
stunting = melt(stunting,id.vars="region",variable.name="year")
stunting$indicator = "stunting_percent"
stunting$component = "C"
stunting$disaggregation = "gender"
stunting$disagg.value = "Children under 5"
# unique(stunting$region) %in% unique(master_dat_reg$region)
master_dat_fix_list[[master_dat_fix_index]] = stunting
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!(indicator=="overweight_percent" & disagg.value %in% c("Both","Girls","Boys")))
overweight = read.xlsx(
  "CHILD STATUS U5.xlsx",
  sheet=3,
  rows=c(3:5),
  cols=ex.num(c("a","m","r","w","ab","ag","al","aq","av","ba","bf")),
  na.strings="-"
)
names(overweight) = c(
  "region",
  "2000","2005","2010","2011","2012","2013","2014","2015","2016","2017"
)
overweight$region = "World"
overweight = melt(overweight,id.vars="region",variable.name="year")
overweight$indicator = "overweight_percent"
overweight$component = "C"
overweight$disaggregation = "gender"
overweight$disagg.value = "Children under 5"
# unique(overweight$region) %in% unique(master_dat_reg$region)
master_dat_fix_list[[master_dat_fix_index]] = overweight
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!(indicator=="wasting_percent" & disagg.value %in% c("Both","Girls","Boys")))
# master_dat_reg = subset(master_dat_reg,!(indicator=="wasting_percent" & disaggregation=="income" & year==2014))
wasting = read.xlsx(
  "CHILD STATUS U5.xlsx",
  sheet=5,
  rows=c(3:5),
  cols=ex.num(c("a","c")),
  na.strings="-"
)
names(wasting) = c(
  "region",
  "2017"
)
wasting$region = "World"
wasting = melt(wasting,id.vars="region",variable.name="year")
wasting$indicator = "wasting_percent"
wasting$component = "C"
wasting$disaggregation = "gender"
wasting$disagg.value = "Children under 5"
# unique(wasting$region) %in% unique(master_dat_reg$region)
master_dat_fix_list[[master_dat_fix_index]] = wasting
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="u5mr")
u5mr = read.xlsx(
  "DEMOGRAPHY U5 mort.xlsx",
  rows=c(17,18)
  ,cols=c(ex.num("c"),ex.num("p"):ex.num("s"))
)
names(u5mr) = c("region",seq(2000,2015,5))
u5mr = melt(u5mr,id.vars="region",variable.name="year")
u5mr$year = unfactor(u5mr$year)
u5mr$region = "World"
u5mr$component = "R"
u5mr$indicator = "u5mr"
u5mr$disaggregation = "all"
master_dat_fix_list[[master_dat_fix_index]] = u5mr
master_dat_fix_index = master_dat_fix_index + 1

# oda_per_cap = read.xlsx(
#   "FINANCIAL regional.xlsx"
# )
# oda_per_cap = melt(oda_per_cap,id.vars="region")
# oda_per_cap$variable = unfactor(oda_per_cap$variable)
# oda_per_cap$year = substr(oda_per_cap$variable,nchar(oda_per_cap$variable)-3,nchar(oda_per_cap$variable))
# oda_per_cap$variable = NULL
# oda_per_cap$indicator = "oda_per_capita"
# oda_per_cap$disaggregation = "all"
# oda_per_cap$component = "P"
# master_dat_fix_list[[master_dat_fix_index]] = oda_per_cap
# master_dat_fix_index = master_dat_fix_index + 1


indicators = c(
  "under_5_stunting_track",       
  "under_5_wasting_track",
  "under_5_overweight_track",   
  "wra_anaemia_track",        
  "ebf_track",            
  "adult_fem_obesity_track", 
  "adult_mal_obesity_track",   
  "adult_fem_diabetes_track",
  "adult_mal_diabetes_track"
)
master_dat_reg = subset(master_dat_reg,!indicator %in% indicators)
overview = read.xlsx(
  "OVERVIEW progress.xlsx"
  ,rows = c(1:7)
)
names(overview) = c("region",indicators)
overview = melt(overview,id.vars="region",variable.name="indicator")
overview$n = sapply(strsplit(overview$value,split="/"),`[`,index=1)
overview$N = sapply(strsplit(overview$value,split="/"),`[`,index=2)
overview$value = "On course"
overview$disaggregation = "all"
overview$component = "A"
overview$region[which(overview$region=="Latin American and Caribbean")] = "Latin America and the Caribbean"
overview = data.table(overview)[,.(n=sum(as.numeric(n)),N=sum(as.numeric(N))),by=.(indicator,value,disaggregation,component)]
overview$region = "World"
master_dat_fix_list[[master_dat_fix_index]] = overview
master_dat_fix_index = master_dat_fix_index + 1

indicators = c(
  "sugar_tax"                            
  ,"salt_leg"
  ,"multi_sec"    
  ,"fbdg"                                 
  ,"stunting_plan"                        
  ,"anaemia_plan"                         
  ,"LBW_plan"                             
  ,"child_overweight_plan"                
  ,"EBF_plan"                             
  ,"wasting_plan"                          
  ,"sodium_plan"                          
  ,"overweight_adults_adoles_plan"    
)
master_dat_reg = subset(master_dat_reg,!indicator %in% indicators)
policy = read.xlsx(
  "POLICY regional.xlsx"
  ,rows = c(1:7)
)
names(policy) = c("region",indicators)
policy = melt(policy,id.vars="region",variable.name="indicator")
policy$n = sapply(strsplit(policy$value,split="/"),`[`,index=1)
policy$N = sapply(strsplit(policy$value,split="/"),`[`,index=2)
policy$value = "Yes"
policy$disaggregation = "all"
policy$component = "O"
policy$region[which(policy$region=="Latin American and Caribbean")] = "Latin America and the Caribbean"
policy = data.table(policy)[,.(n=sum(as.numeric(n)),N=sum(as.numeric(N))),by=.(indicator,value,disaggregation,component)]
policy$region = "World"
master_dat_fix_list[[master_dat_fix_index]] = policy
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="total_calories_non_staple")
total_calories_non_staple = read.xlsx(
  "UNDERLYING_non_staples.xlsx"
  ,cols=c(ex.num("d"),ex.num("j"),ex.num("l"))
  ,rows=c(1:14)
)
names(total_calories_non_staple) = c("region","year","value")
total_calories_non_staple$year = substr(total_calories_non_staple$year,6,9)
total_calories_non_staple$component = "R"
total_calories_non_staple$indicator = "total_calories_non_staple"
total_calories_non_staple$disaggregation = "all"
total_calories_non_staple = subset(total_calories_non_staple,region %in% unique(master_dat_reg$region))
total_calories_non_staple = subset(total_calories_non_staple,year %in% c(2001,2004,2008,2012,2013))
total_calories_non_staple$value = 100-as.numeric(total_calories_non_staple$value)
master_dat_fix_list[[master_dat_fix_index]] = total_calories_non_staple
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="undernourishment_prev")
undernourishment_prev = read.xlsx(
  "UNDERLYING_Undernourishment.xlsx"
  ,rows=c(4:252)
  ,cols=ex.num(c("a","b","c","g","k","o","s"))
)
names(undernourishment_prev) = c(
  "iso3","region","2000","2004","2008","2012","2016"
)
undernourishment_prev = subset(undernourishment_prev,region=="World")
undernourishment_prev$iso3 = NULL
undernourishment_prev = melt(undernourishment_prev,id.vars=c("region"),variable.name="year")
undernourishment_prev$indicator = "undernourishment_prev"
undernourishment_prev$disaggregation = "all"
undernourishment_prev$component = "S"
master_dat_fix_list[[master_dat_fix_index]] = undernourishment_prev
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="fruit_veg_availability")
food = read.csv("UNDERLYING_Food stuffs_world.csv",na.strings="",as.is=T,check.names=F)
names(food)[1] = "region"
food = melt(food,id.vars="region",variable.name="year")
food$indicator = "fruit_veg_availability"
food$component = "S"
food$disaggregation = "all"
master_dat_fix_list[[master_dat_fix_index]] = food
master_dat_fix_index = master_dat_fix_index + 1

master_dat_fix = rbindlist(master_dat_fix_list,fill=T)
master_dat_reg = rbindlist(list(master_dat_reg,master_dat_fix),fill=T)

write.csv(master_dat_reg,"../data_world.csv",na="",row.names=F)
