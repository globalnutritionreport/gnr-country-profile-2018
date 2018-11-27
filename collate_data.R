####Setup#####
list.of.packages <- c("reshape2","data.table","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018/"
setwd(wd)

country = read.csv("data.csv",na.strings="",as.is=T)
country_meta = unique(country[,c("iso3","country","region","subregion")])
region = read.csv("data_reg.csv",na.strings="",as.is=T)
world = read.csv("data_world.csv",na.strings="",as.is=T)

value_type_mapping = read.csv("value_type_mapping.csv",na.strings="",as.is=T)
vtm_region = subset(value_type_mapping,location=="region")
vtm_region$location = NULL
vtm_world = subset(value_type_mapping,location=="world")
vtm_world$location = NULL
region = merge(region,vtm_region,by="indicator",all.x=T)
world = merge(world,vtm_world,by="indicator",all.x=T)

region$value[which(region$indicator %in% c("population","65_years","u5_pop"))] = region$value.sum[which(region$indicator %in% c("population","65_years","u5_pop"))]
world$value[which(world$indicator %in% c("population","65_years","u5_pop"))] = world$value.sum[which(world$indicator %in% c("population","65_years","u5_pop"))]

world$value.type[which(world$indicator=="population")] = "modelled estimate"

region$value.type[which(region$indicator %in% c("overweight_percent","stunting_percent","wasting_percent") & region$disagg.value=="Children under 5")] = "modelled estimate"
world$value.type[which(world$indicator %in% c("overweight_percent","stunting_percent","wasting_percent") & world$disagg.value=="Children under 5")] = "modelled estimate"

# region = subset(region,!(indicator %in% c("overweight_percent","stunting_percent","wasting_percent") & disagg.value %in% c("Boys","Girls")))

region$disaggregation[which(region$indicator %in% c("overweight_percent","stunting_percent","wasting_percent") & region$disagg.value=="Children under 5")] = "all"
world$disaggregation[which(world$indicator %in% c("overweight_percent","stunting_percent","wasting_percent") & world$disagg.value=="Children under 5")] = "all"


mult_by_100 = c(
  "adolescent_underweight",
  "adolescent_overweight",
  "adolescent_obesity",
  "adult_diabetes",
  "adult_overweight",
  "adult_obesity",
  "adult_blood_pressure"
)

country$value[which(country$indicator %in% mult_by_100)] = as.numeric(country$value[which(country$indicator %in% mult_by_100)])*100
region$value[which(region$indicator %in% mult_by_100)] = as.numeric(region$value[which(region$indicator %in% mult_by_100)])*100
world$value[which(world$indicator %in% mult_by_100)] = as.numeric(world$value[which(world$indicator %in% mult_by_100)])*100

round.simple = function(x, digits=0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

round.simple = Vectorize(round.simple)

numericable = function(x){
  if(is.na(as.numeric(x))){
    return(F)
  }
  return(T)
}

numericable = Vectorize(numericable)

country$value[which(numericable(country$value))] = round.simple(as.numeric(country$value[which(numericable(country$value))]),digits=2)
country$value.sum[which(numericable(country$value.sum))] = round.simple(as.numeric(country$value.sum[which(numericable(country$value.sum))]),digits=2)
country$value[which(country$indicator=="gini_rank")] = round.simple(as.numeric(country$value[which(country$indicator=="gini_rank")]))
region$value[which(numericable(region$value))] = round.simple(as.numeric(region$value[which(numericable(region$value))]),digits=2)
region$value.sum[which(numericable(region$value.sum))] = round.simple(as.numeric(region$value.sum[which(numericable(region$value.sum))]),digits=2)
world$value[which(numericable(world$value))] = round.simple(as.numeric(world$value[which(numericable(world$value))]),digits=2)
world$value.sum[which(numericable(world$value.sum))] = round.simple(as.numeric(world$value.sum[which(numericable(world$value.sum))]),digits=2)

wb <- createWorkbook("all_profile_data.xlsx")

com.dict = c(
  "A"="overview",
  "C"="child nutrition",
  "G"="adolescent",
  "H"="adult",
  "I"="adult",
  "K"="child feeding",
  "M"="dietary needs",
  "O"="policy",
  "P"="intervention",
  "Q"="policy",
  "R"="economics",
  "S"="underlying",
  "U"="underlying",
  "V"="underlying"
)

# Profile order for sections X
# Need verbose name for section, e.g. economics -> Economics and demography X 
# Replace world sodium with 5.6 X
# two decimal points for numbers, multiply adolescent percents by 100 first X
# add burden text back in X
# fbdg to policy tab X
# Don't melt by n, to keep Africa all in the same line X
# For dietary needs, rename National, replace world X
# remove gini and gini rank for region+world X
# total_calories_non_staple move to underlying X
# For recip==0, change name to ODA_disbursed X
# Round gini rank to whole number X
# change value.type to "modelled estimate" for World population X
# Add dummy rows for 4 missing countries in adolescent tab
# Make sure all country tabs have 194

com.dict.override = c(
  "coexistence"="child nutrition",
  "ODA_specific"="ODA",
  "ODA_received"="ODA",
  "fbdg"="policy",
  "total_calories_non_staple" = "underlying"
)

country$section = NA
country$section = com.dict[country$component]
country$section[which(country$indicator %in% names(com.dict.override))] = com.dict.override[country$indicator[which(country$indicator %in% names(com.dict.override))]]
country = subset(country,!(component=="M" & disagg.value=="Global"))
country$component = NULL
country$unit = NULL
country$rec = NULL
country$indicator[which(country$indicator=="ODA_received" & country$recip==0)] = "ODA_disbursed"
country$recip = NULL
# country = subset(country,!indicator %in% c("burden_text","country_class"))

region$section = NA
region$section = com.dict[region$component]
region$section[which(region$indicator %in% names(com.dict.override))] = com.dict.override[region$indicator[which(region$indicator %in% names(com.dict.override))]]
region = subset(region,!(component=="M" & disagg.value %in% c("Global","Regional")))
region$disagg.value[which(region$component=="M" & region$disagg.value=="National")] = "Regional"
region$component = NULL
region$unit = NULL
region$rec = NULL
region$total.pop = NULL
region$regional = NULL
region$value.sum = NULL
region$value.unweighted = NULL
region = subset(region,!indicator %in% c("burden_text","country_class","gini","gini_rank","gender_inequality_rank","gender_inequality_score"))

world$section = NA
world$section = com.dict[world$component]
world$section[which(world$indicator %in% names(com.dict.override))] = com.dict.override[world$indicator[which(world$indicator %in% names(com.dict.override))]]
region = subset(region,!(component=="M" & disagg.value %in% c("National","Regional")))
world$component = NULL
world$unit = NULL
world$rec = NULL
world$total.pop = NULL
world$value.sum = NULL
world$value.unweighted = NULL
world = subset(world,!indicator %in% c("burden_text","country_class","gini","gini_rank","gender_inequality_rank","gender_inequality_score"))

sections = c(
  "overview",
  "economics",
  "underlying",
  "child nutrition",
  "child feeding",
  "adolescent",
  "adult",
  "dietary needs",
  "ODA",
  "policy",
  "intervention"
)

section.names = c(
  "Overview",
  "Economics and demography",
  "Underlying determinants",
  "Children (under5) nutrition status",
  "Child feeding practices",
  "Children and adolescent (aged 5-19) nutrition status",
  "Adult nutrition status",
  "Dietary needs",
  "Financial resources",
  "Policy, legislation, and institutional arrangements",
  "Intervention coverage"
)
names(section.names) = sections

for(this.section in sections){
  message(this.section)
  ctry.sub = subset(country,section==this.section)
  ctry.sub = ctry.sub[order(ctry.sub$year),]
  ctry.sub = melt(ctry.sub,id.vars=c("iso3","country","disaggregation","disagg.value","indicator","region","subregion","section","year"))
  ctry.sub$variable = NULL
  ctry.sub = dcast(ctry.sub,iso3+country+disaggregation+disagg.value+region+subregion+section~indicator+year)
  na.names = names(ctry.sub)[which(substr(names(ctry.sub),nchar(names(ctry.sub))-2,nchar(names(ctry.sub)))=="_NA")]
  names(ctry.sub)[which(substr(names(ctry.sub),nchar(names(ctry.sub))-2,nchar(names(ctry.sub)))=="_NA")] = substr(na.names,1,nchar(na.names)-3)
  ctry.sub$section = section.names[ctry.sub$section]
  if(length(unique(ctry.sub$country))<194){
    ctry.sub = merge(ctry.sub,country_meta,all=T)
    ctry.sub$section = section.names[this.section]
  }
  message(length(unique(ctry.sub$country)))
  
  reg.sub = subset(region,section==this.section)
  if(this.section %in% c("overview","policy")){
    reg.sub$value = paste0(reg.sub$n,"/",reg.sub$N)
  }
  reg.sub$n = NULL
  reg.sub$N = NULL
  reg.sub = reg.sub[order(reg.sub$year),]
  reg.sub = melt(reg.sub,id.vars=c("region","disaggregation","disagg.value","indicator","section","year","year_range","value.type"))
  reg.sub$variable = NULL
  reg.sub = dcast(reg.sub,region+disaggregation+disagg.value+value.type+section~indicator+year)
  na.names = names(reg.sub)[which(substr(names(reg.sub),nchar(names(reg.sub))-2,nchar(names(reg.sub)))=="_NA")]
  names(reg.sub)[which(substr(names(reg.sub),nchar(names(reg.sub))-2,nchar(names(reg.sub)))=="_NA")] = substr(na.names,1,nchar(na.names)-3)
  reg.sub$section = section.names[reg.sub$section]
  
  if(this.section=="intervention"){
    wld.sub = data.frame(
      "indicator"=c("diarrhea_zinc","vit_a","iron_supp","iron_and_folic","iodised_salt"),
      "n"=c(46,58,56,62,52),
      "minimum"=c(0.1,4.5,1.3,22.6,18),
      "maximum"=c(50.2,86.4,45.4,96.6,99.8),
      "mean"=c(8.6,57,14.6,74.6,82.7),
      "median"=c(2.8,60.9,11.6,81,90.9),
      "section"=section.names[this.section],
      "value.type"="modelled estimate"
    )
  }else{
    wld.sub = subset(world,section==this.section)
    if(this.section %in% c("overview","policy")){
      wld.sub$value = paste0(wld.sub$n,"/",wld.sub$N)
    }
    wld.sub$n = NULL
    wld.sub$N = NULL
    wld.sub = wld.sub[order(wld.sub$year),]
    wld.sub = melt(wld.sub,id.vars=c("region","disaggregation","disagg.value","indicator","section","year","year_range","value.type"))
    wld.sub$variable = NULL
    wld.sub = dcast(wld.sub,region+disaggregation+disagg.value+value.type+section~indicator+year)
    na.names = names(wld.sub)[which(substr(names(wld.sub),nchar(names(wld.sub))-2,nchar(names(wld.sub)))=="_NA")]
    names(wld.sub)[which(substr(names(wld.sub),nchar(names(wld.sub))-2,nchar(names(wld.sub)))=="_NA")] = substr(na.names,1,nchar(na.names)-3)
    wld.sub$section = section.names[wld.sub$section]
  }

  
  
  ctry.sheet = paste("Country",this.section)
  addWorksheet(wb,ctry.sheet)
  writeData(wb,sheet=ctry.sheet,ctry.sub,colNames=TRUE,rowNames=FALSE)
  
  if(this.section!="intervention"){
    reg.sub = rbindlist(list(reg.sub,wld.sub),fill=T)
    reg.sheet = paste("Region+World",this.section)
    addWorksheet(wb,reg.sheet)
    writeData(wb,sheet=reg.sheet,reg.sub,colNames=TRUE,rowNames=FALSE)
  }else{
    # reg.sheet = paste("Region",this.section)
    # addWorksheet(wb,reg.sheet)
    # writeData(wb,sheet=reg.sheet,reg.sub,colNames=TRUE,rowNames=FALSE)
    
    wld.sheet = paste("World",this.section)
    addWorksheet(wb,wld.sheet)
    writeData(wb,sheet=wld.sheet,wld.sub,colNames=TRUE,rowNames=FALSE)
  }
  
  
}

saveWorkbook(wb, "profile_data.xlsx", overwrite = TRUE)