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

components = unique(country$component)
components = sort(components)

for(this.component in components){
  ctry.sub = subset(country,component==this.component)
  reg.sub = subset(region,component==this.component)
  wld.sub = subset(world,component==this.component)
  
  ctry.sheet = paste("Country",this.component,sep=".")
  addWorksheet(wb,ctry.sheet)
  writeData(wb,sheet=ctry.sheet,ctry.sub,colNames=TRUE,rowNames=FALSE)
  
  reg.sheet = paste("Region",this.component,sep=".")
  addWorksheet(wb,reg.sheet)
  writeData(wb,sheet=reg.sheet,reg.sub,colNames=TRUE,rowNames=FALSE)
  
  wld.sheet = paste("World",this.component,sep=".")
  addWorksheet(wb,wld.sheet)
  writeData(wb,sheet=wld.sheet,wld.sub,colNames=TRUE,rowNames=FALSE)
}

saveWorkbook(wb, "profile_data.xlsx", overwrite = TRUE)