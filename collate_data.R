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

region$section = NA
region$section = com.dict[region$component]
region$section[which(region$indicator %in% names(com.dict.override))] = com.dict.override[region$indicator[which(region$indicator %in% names(com.dict.override))]]
region$component = NULL

world$section = NA
world$section = com.dict[world$component]
world$section[which(world$indicator %in% names(com.dict.override))] = com.dict.override[world$indicator[which(world$indicator %in% names(com.dict.override))]]
world$component = NULL

sections = unique(country$section)
sections = sort(sections)

for(this.section in sections){
  ctry.sub = subset(country,section==this.section)
  reg.sub = subset(region,section==this.section)
  wld.sub = subset(world,section==this.section)
  
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