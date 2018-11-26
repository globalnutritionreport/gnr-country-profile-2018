####Setup#####
list.of.packages <- c("reshape2","data.table","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018/"
setwd(wd)

country = read.csv("data.csv",na.strings="",as.is=T)
region = read.csv("data_reg.csv",na.strings="",as.is=T)
world = read.csv("data_world.csv",na.strings="",as.is=T)

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

com.dict.override = c(
  "coexistence"="child nutrition",
  "ODA_specific"="ODA",
  "ODA_received"="ODA"
)

country$section = NA
country$section = com.dict[country$component]
country$section[which(country$indicator %in% names(com.dict.override))] = com.dict.override[country$indicator[which(country$indicator %in% names(com.dict.override))]]
country$component = NULL
country$unit = NULL
country$rec = NULL
country$recip = NULL
country = subset(country,!indicator %in% c("burden_text","country_class"))

region$section = NA
region$section = com.dict[region$component]
region$section[which(region$indicator %in% names(com.dict.override))] = com.dict.override[region$indicator[which(region$indicator %in% names(com.dict.override))]]
region$component = NULL
region$unit = NULL
region$rec = NULL
region$total.pop = NULL
region$regional = NULL
region$value.unweighted = NULL
region = subset(region,!indicator %in% c("burden_text","country_class"))

world$section = NA
world$section = com.dict[world$component]
world$section[which(world$indicator %in% names(com.dict.override))] = com.dict.override[world$indicator[which(world$indicator %in% names(com.dict.override))]]
world$component = NULL
world$unit = NULL
world$rec = NULL
world$total.pop = NULL
world$value.unweighted = NULL
world = subset(world,!indicator %in% c("burden_text","country_class"))

sections = unique(country$section)
sections = sort(sections)

# this.section = sections[1]

for(this.section in sections){
  message(this.section)
  ctry.sub = subset(country,section==this.section)
  ctry.sub = ctry.sub[order(ctry.sub$year),]
  ctry.sub = melt(ctry.sub,id.vars=c("iso3","country","disaggregation","disagg.value","indicator","region","subregion","section","year"))
  ctry.sub = dcast(ctry.sub,iso3+country+disaggregation+disagg.value+region+subregion+section~indicator+year)
  na.names = names(ctry.sub)[which(substr(names(ctry.sub),nchar(names(ctry.sub))-2,nchar(names(ctry.sub)))=="_NA")]
  names(ctry.sub)[which(substr(names(ctry.sub),nchar(names(ctry.sub))-2,nchar(names(ctry.sub)))=="_NA")] = substr(na.names,1,nchar(na.names)-3)
  
  reg.sub = subset(region,section==this.section)
  reg.sub = reg.sub[order(reg.sub$year),]
  reg.sub = melt(reg.sub,id.vars=c("region","disaggregation","disagg.value","indicator","section","year","year_range","n","N"))
  reg.sub$value.type = NA
  reg.sub$value.type[which(reg.sub$variable=="value.unweighted")] = "unweighted mean"
  reg.sub$value.type[which(reg.sub$variable=="value")] = "weighted mean"
  reg.sub$value.type[which(reg.sub$variable=="value.sum")] = "sum"
  reg.sub$variable = NULL
  if(this.section %in% c("overview","policy")){
    reg.sub$value = paste0(reg.sub$n,"/",reg.sub$N)
  }
  reg.sub$N = NULL
  reg.sub = dcast(reg.sub,region+disaggregation+disagg.value+n+value.type~indicator+year)
  na.names = names(reg.sub)[which(substr(names(reg.sub),nchar(names(reg.sub))-2,nchar(names(reg.sub)))=="_NA")]
  names(reg.sub)[which(substr(names(reg.sub),nchar(names(reg.sub))-2,nchar(names(reg.sub)))=="_NA")] = substr(na.names,1,nchar(na.names)-3)
  
  if(this.section=="intervention"){
    wld.sub = data.frame(
      "indicator"=c("diarrhea_zinc","vit_a","iron_supp","iron_and_folic","iodised_salt"),
      "n"=c(46,58,56,62,52),
      "minimum"=c(0.1,4.5,1.3,22.6,18),
      "maximum"=c(50.2,86.4,45.4,96.6,99.8),
      "mean"=c(8.6,57,14.6,74.6,82.7),
      "median"=c(2.8,60.9,11.6,81,90.9)
    )
  }else{
    wld.sub = subset(world,section==this.section)
    wld.sub = wld.sub[order(wld.sub$year),]
    wld.sub = melt(wld.sub,id.vars=c("region","disaggregation","disagg.value","indicator","section","year","year_range","n","N"))
    wld.sub$value.type = NA
    wld.sub$value.type[which(wld.sub$variable=="value.unweighted")] = "unweighted mean"
    wld.sub$value.type[which(wld.sub$variable=="value")] = "weighted mean"
    wld.sub$value.type[which(wld.sub$variable=="value.sum")] = "sum"
    wld.sub$variable = NULL
    if(this.section %in% c("overview","policy")){
      wld.sub$value = paste0(wld.sub$n,"/",wld.sub$N)
    }
    wld.sub$N = NULL
    wld.sub = dcast(wld.sub,region+disaggregation+disagg.value+n+value.type~indicator+year)
    na.names = names(wld.sub)[which(substr(names(wld.sub),nchar(names(wld.sub))-2,nchar(names(wld.sub)))=="_NA")]
    names(wld.sub)[which(substr(names(wld.sub),nchar(names(wld.sub))-2,nchar(names(wld.sub)))=="_NA")] = substr(na.names,1,nchar(na.names)-3)
  }

  
  
  ctry.sheet = paste("Country",this.section)
  addWorksheet(wb,ctry.sheet)
  writeData(wb,sheet=ctry.sheet,ctry.sub,colNames=TRUE,rowNames=FALSE)
  
  reg.sheet = paste("Region",this.section)
  addWorksheet(wb,reg.sheet)
  writeData(wb,sheet=reg.sheet,reg.sub,colNames=TRUE,rowNames=FALSE)
  
  wld.sheet = paste("World",this.section)
  addWorksheet(wb,wld.sheet)
  writeData(wb,sheet=wld.sheet,wld.sub,colNames=TRUE,rowNames=FALSE)
}

saveWorkbook(wb, "profile_data.xlsx", overwrite = TRUE)