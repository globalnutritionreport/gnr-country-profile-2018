####Setup#####
list.of.packages <- c("reshape2","data.table","openxlsx","plyr","gdata","varhandle","WDI")
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

# master_dat_reg = subset(master_dat_reg,indicator!="u5mr")
# u5mr = read.xlsx(
#   "DEMOGRAPHY U5 mort.xlsx",
#   rows=c(17,18)
#   ,cols=c(ex.num("c"),ex.num("p"):ex.num("s"))
# )
# names(u5mr) = c("region",seq(2000,2015,5))
# u5mr = melt(u5mr,id.vars="region",variable.name="year")
# u5mr$year = unfactor(u5mr$year)
# u5mr$region = "World"
# u5mr$component = "R"
# u5mr$indicator = "u5mr"
# u5mr$disaggregation = "all"
# master_dat_fix_list[[master_dat_fix_index]] = u5mr
# master_dat_fix_index = master_dat_fix_index + 1

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

wd <- "~/git/gnr-country-profile-2018/Dataset working directory_world"
setwd(wd)

master_dat_reg = subset(master_dat_reg,indicator!="population")
pop = data.frame(
  region = "World",
  year = 2018,
  component = "R",
  indicator = "population",
  disaggregation = "all",
  value.sum =  7632819
)
master_dat_fix_list[[master_dat_fix_index]] = pop
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% c("190_percent","320_percent"))
pov190 = read.csv("ECONOMICS_AND_DEMOGRAPHY_1.90 Poverty Line.csv",na.strings="",as.is=T,skip=4,check.names=F)
names(pov190)[1:4] = c("region","code","ind","ind.code")
pov190 = subset(pov190,region=="World")
pov190$code = NULL
pov190$ind = NULL
pov190$ind.code = NULL
pov190 = melt(pov190,id.vars="region",variable.name="year")
pov190$year = unfactor(pov190$year)
pov190 = subset(pov190,year %in% c(1999,2005,2010,2015))
pov190$component = "R"
pov190$indicator = "190_percent"
pov190$disaggregation = "all"
master_dat_fix_list[[master_dat_fix_index]] = pov190
master_dat_fix_index = master_dat_fix_index + 1

pov320 = read.csv("ECONOMICS_AND_DEMOGRAPHY_3.20 Poverty Line.csv",na.strings="",as.is=T,skip=4,check.names=F)
names(pov320)[1:4] = c("region","code","ind","ind.code")
pov320 = subset(pov320,region=="World")
pov320$code = NULL
pov320$ind = NULL
pov320$ind.code = NULL
pov320 = melt(pov320,id.vars="region",variable.name="year")
pov320$year = unfactor(pov320$year)
pov320 = subset(pov320,year %in% c(1999,2005,2010,2015))
pov320$component = "R"
pov320$indicator = "320_percent"
pov320$disaggregation = "all"
master_dat_fix_list[[master_dat_fix_index]] = pov320
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% c("GDP_capita_PPP"))
gdp_cap = read.csv("ECONOMICS_AND_DEMOGRAPHY_GDP Per Capita.csv",na.strings="",as.is=T,skip=4,check.names=F)
names(gdp_cap)[1:4] = c("region","code","ind","ind.code")
gdp_cap = subset(gdp_cap,region=="World")
gdp_cap$code = NULL
gdp_cap$ind = NULL
gdp_cap$ind.code = NULL
gdp_cap = melt(gdp_cap,id.vars="region",variable.name="year")
gdp_cap$year = unfactor(gdp_cap$year)
gdp_cap = subset(gdp_cap,year>=1999 & !is.na(value))
gdp_cap$component = "R"
gdp_cap$indicator = "GDP_capita_PPP"
gdp_cap$disaggregation = "all"
master_dat_fix_list[[master_dat_fix_index]] = gdp_cap
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="u5mr")
u5mr = read.xlsx(
  "ECONOMICS_AND_DEMOGRAPHY_U5MR_GLOBAL.xlsx",
  rows=c(16:352)
  ,cols=c(ex.num("a"),ex.num("b"),ex.num("d"))
)
names(u5mr) = c("region","year","value")
u5mr = subset(u5mr,region=="World" & year>=1999)
u5mr$component = "R"
u5mr$disaggregation = "all"
u5mr$indicator = "u5mr"
master_dat_fix_list[[master_dat_fix_index]] = u5mr
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="female_secondary_enroll_net")
enrol = read.xlsx(
  "UNDERLYING_DETERMINANTS_FEMALE_ENROLMENT_NET_GLOBAL.xlsx"
  ,rows = c(5,263)
  )
names(enrol)[1:4] = c("region","code","ind","ind.code")
enrol = subset(enrol,region=="World")
enrol$code = NULL
enrol$ind = NULL
enrol$ind.code = NULL
enrol = melt(enrol,id.vars="region",variable.name="year")
enrol$year = unfactor(enrol$year)
enrol = subset(enrol,year>=2013 & !is.na(value))
enrol$component = "U"
enrol$indicator = "female_secondary_enroll_net"
enrol$disaggregation = "all"
master_dat_fix_list[[master_dat_fix_index]] = enrol
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% must_sum_to_100s)
drinking = read.xlsx("UNDERLYING_DETERMINANTS_DRINKING_WATER_GLOBAL.xlsx")
names(drinking) = c("region","safely_managed_water","basic_water","limited_water","unimproved_water","surface_water")
drinking = melt(drinking,id.vars="region",variable.name="indicator")
drinking$year = 2015
drinking$disaggregation = "all"
drinking$component = "V"
master_dat_fix_list[[master_dat_fix_index]] = drinking
master_dat_fix_index = master_dat_fix_index + 1

sanitation = read.xlsx("UNDERLYING_DETERMINANTS_SANITATION_GLOBAL.xlsx",rows=c(1,2),cols=c(1:6))
names(sanitation) = c("safely_managed_sanitation","basic_sanitation","limited_sanitation","unimproved_sanitation","open_defecation")
sanitation$region = "World"
sanitation = melt(sanitation,id.vars="region",variable.name="indicator")
sanitation$year = 2015
sanitation$disaggregation = "all"
sanitation$component = "V"
master_dat_fix_list[[master_dat_fix_index]] = sanitation
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% c("social_protection_spending","education_spending","health_spending","agriculture_expenditure"))
exp = read.xlsx("UNDERLYING_DETERMINANTS_GOV_EXP_GLOBAL.xlsx",rows=c(7:10))
names(exp) = c("year","agriculture_expenditure","education_spending","health_spending","social_protection_spending")
exp$region = "World"
exp = melt(exp,id.vars=c("region","year"),variable.name="indicator")
exp$value = exp$value*100
exp$disaggregation = "all"
exp$component = "V"
master_dat_fix_list[[master_dat_fix_index]] = exp
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="coexistence")
coexistence = read.xlsx("U5 coexistence global.xlsx")
names(coexistence) = c(
  "region",
  "Wasting alone",
  "Wasting and stunting",
  "Stunting alone",
  "Stunting and overweight",
  "Overweight alone",
  "Free from"
)
coexistence = subset(coexistence,region=="Global")
coexistence$region = "World"
coexistence$n = 106
coexistence = melt(coexistence,id.vars=c("region","n"),variable.name="disagg.value")
coexistence$indicator = "coexistence"
coexistence$disaggregation = "all"
coexistence$component = "G"
coexistence$value = coexistence$value*100
master_dat_fix_list[[master_dat_fix_index]] = coexistence
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% c("adolescent_obesity","adolescent_overweight","adolescent_underweight"))
adol = read.csv("ADOLESCENT STATUS - NCD_RisC_Lancet_2017_BMI_child_adolescent__world.csv",na.strings="",as.is=T,check.names=F)
adol = adol[,c("Country/Region/World","Sex","Year","Prevalence of BMI>2SD (obesity)","Prevalence of BMI>1SD (overweight)","Prevalence of BMI<minus1SD (underweight)")]
names(adol) = c("region","disagg.value","year","adolescent_obesity","adolescent_overweight","adolescent_underweight")
adol = melt(adol,id.vars=c("region","disagg.value","year"),variable.name="indicator")
adol = subset(adol,year>=1999)
adol$disaggregation = "gender"
adol$component = "G"
master_dat_fix_list[[master_dat_fix_index]] = adol
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% c("ODA_specific","ODA_received"))
oda = read.xlsx(
  "ODA.xlsx"
)
names(oda)[1]="indicator"
oda$indicator[which(oda$indicator=="% of total ODA")] = "ODA_received"
oda$indicator[which(oda$indicator=="total ODA")] = "ODA_specific"
oda$region = "World"
oda = melt(oda,id.vars=c("region","indicator"),variable.name="year")
oda$year = unfactor(oda$year)
oda$disaggregation = "all"
oda$component = "P"
master_dat_fix_list[[master_dat_fix_index]] = oda
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% c("vit_a","diarrhea_zinc","iron_supp","iodised_salt","iron_and_folic"))

master_dat_reg = subset(master_dat_reg,indicator != "adult_diabetes")
dia = read.xlsx(
  "ADULT STATUS - Age standardised (Country-Region-World) (NCD-RisC_Lancet_2016_Diabetes_age_standardised_by_Country_Region_World).xlsx",
  sheet=2,
  rows=c(5:7)
  )
names(dia)[1]="disagg.value"
dia$disagg.value[which(dia$disagg.value=="Men")] = "Male"
dia$disagg.value[which(dia$disagg.value=="Women")] = "Female"
dia$Grand.Total=NULL
dia = melt(dia,id.vars="disagg.value",variable.name="year")
dia$region = "World"
dia$disaggregation = "gender"
dia$component = "H"
dia$year = unfactor(dia$year)
dia$indicator = "adult_diabetes"
dia = subset(dia,year>=1999)
master_dat_fix_list[[master_dat_fix_index]] = dia
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator != "adult_blood_pressure")
bp = read.xlsx(
  "ADULT STATUS - Age standardised (Country-Region-World) (NCD-RisC_Lancet_2017_BP_age_standardised_by_Country_Region_World).xlsx",
  sheet=2,
  rows=c(5:7)
)
names(bp)[1]="disagg.value"
bp$disagg.value[which(bp$disagg.value=="Men")] = "Male"
bp$disagg.value[which(bp$disagg.value=="Women")] = "Female"
bp$Grand.Total=NULL
bp = melt(bp,id.vars="disagg.value",variable.name="year")
bp$region = "World"
bp$disaggregation = "gender"
bp$component = "H"
bp$year = unfactor(bp$year)
bp = subset(bp,year>=1999)
bp$indicator = "adult_blood_pressure"
master_dat_fix_list[[master_dat_fix_index]] = bp
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,!indicator %in% c("adult_obesity","adult_overweight"))
adult = read.csv("ADULT overweight and obseity.csv",na.strings="",as.is=T,check.names=F)
adult = adult[,c(1,3,4,8,23)]
names(adult) = c("region","disagg.value","year","adult_obesity","adult_overweight")
adult = subset(adult,!is.na(region) & year>=1999)
adult$disagg.value[which(adult$disagg.value=="Men")] = "Male"
adult$disagg.value[which(adult$disagg.value=="Women")] = "Female"
adult = melt(adult,id.vars=c("region","disagg.value","year"),variable.name="indicator")
adult$value = adult$value
adult$disaggregation = "gender"
adult$component = "H"
master_dat_fix_list[[master_dat_fix_index]] = adult
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator != "adult_anemia")
# all_anemia = WDI(indicator="SH.ANM.ALLW.ZS",country="1W",start=1999,end=2018)
# write.csv(all_anemia,"all_anemia.csv",na="",row.names=F)
all_anemia = read.csv("all_anemia.csv",na.strings="",as.is=T)
all_anemia$iso2c = NULL
names(all_anemia) = c("region","value","year")
all_anemia$disaggregation = "pregnancy"
all_anemia$disagg.value = "All women"
all_anemia$indicator = "adult_anemia"
all_anemia$component = "I"
master_dat_fix_list[[master_dat_fix_index]] = all_anemia
master_dat_fix_index = master_dat_fix_index + 1

# np_anemia = WDI(indicator="SH.ANM.NPRG.ZS",country="1W",start=1999,end=2018)
# write.csv(np_anemia,"np_anemia.csv",na="",row.names=F)
np_anemia = read.csv("np_anemia.csv",na.strings="",as.is=T)
np_anemia$iso2c = NULL
names(np_anemia) = c("region","value","year")
np_anemia$disaggregation = "pregnancy"
np_anemia$disagg.value = "Non-pregnant women"
np_anemia$indicator = "adult_anemia"
np_anemia$component = "I"
master_dat_fix_list[[master_dat_fix_index]] = np_anemia
master_dat_fix_index = master_dat_fix_index + 1

# p_anemia = WDI(indicator="SH.PRG.ANEM",country="1W",start=1999,end=2018)
# write.csv(p_anemia,"p_anemia.csv",na="",row.names=F)
p_anemia = read.csv("p_anemia.csv",na.strings="",as.is=T)
p_anemia$iso2c = NULL
names(p_anemia) = c("region","value","year")
p_anemia$disaggregation = "pregnancy"
p_anemia$disagg.value = "Pregnant women"
p_anemia$indicator = "adult_anemia"
p_anemia$component = "I"
master_dat_fix_list[[master_dat_fix_index]] = p_anemia
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,indicator!="adult_sodium")
adult_sodium = data.frame(
  year=c(2016,2016,2016),
  disagg.value=c("Male","World","Female"),
  value=c(5.8,5.6,5.4),
  indicator="adult_sodium",
  disaggregation="gender",
  component = "H",
  region="World"
  )
master_dat_fix_list[[master_dat_fix_index]] = adult_sodium
master_dat_fix_index = master_dat_fix_index + 1

master_dat_reg = subset(master_dat_reg,component!="M")
needs = read.csv("dietary_needs.csv")
needs$year = 2016
master_dat_fix_list[[master_dat_fix_index]] = needs
master_dat_fix_index = master_dat_fix_index + 1

master_dat_fix = rbindlist(master_dat_fix_list,fill=T)
master_dat_reg = rbindlist(list(master_dat_reg,master_dat_fix),fill=T)

write.csv(master_dat_reg,"../data_world.csv",na="",row.names=F)
