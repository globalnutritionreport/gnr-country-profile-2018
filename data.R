####Setup#####
list.of.packages <- c("reshape2","data.table","openxlsx","plyr","gdata","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018/Dataset working directory"
setwd(wd)

firstup <- function(x) {
  x = tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

csvs = list.files(pattern="*.csv")
xlsxs = list.files(pattern="*.xlsx")
xlss = list.files(pattern="*.xls")
xlss = setdiff(xlss,xlsxs)

master_dat_list = list()
master_dat_index = 1

dat = read.csv("ADOLESCENT STATUS - NCD_RisC_Lancet_2017_BMI_child_adolescent_country.csv",na.strings="",as.is=T)
adolescent_underweight = dat[c("Country.Region.World","ISO","Sex","Year","Prevalence.of.BMI.minus1SD..underweight.")]
names(adolescent_underweight) = c("country","iso3","disagg.value","year","value")
adolescent_underweight$indicator = "adolescent_underweight"
adolescent_underweight$disaggregation = "gender"
adolescent_underweight$component = "G"
master_dat_list[[master_dat_index]] = adolescent_underweight
master_dat_index = master_dat_index + 1

adolescent_overweight = dat[c("Country.Region.World","ISO","Sex","Year","Prevalence.of.BMI.1SD..overweight.")]
names(adolescent_overweight) = c("country","iso3","disagg.value","year","value")
adolescent_overweight$indicator = "adolescent_overweight"
adolescent_overweight$disaggregation = "gender"
adolescent_overweight$component = "G"
master_dat_list[[master_dat_index]] = adolescent_overweight
master_dat_index = master_dat_index + 1

adolescent_obesity = dat[c("Country.Region.World","ISO","Sex","Year","Prevalence.of.BMI.2SD..obesity.")]
names(adolescent_obesity) = c("country","iso3","disagg.value","year","value")
adolescent_obesity$indicator = "adolescent_obesity"
adolescent_obesity$disaggregation = "gender"
adolescent_obesity$component = "G"
master_dat_list[[master_dat_index]] = adolescent_obesity
master_dat_index = master_dat_index + 1

dat = read.csv("ADULT STATUS - GNR_adult_18plus_female_obesity_country.csv",na.strings="",as.is=T)
adult_obesity_fem = dat[c("country","year","mean")]
names(adult_obesity_fem) = c("country","year","value")
adult_obesity_fem$indicator = "adult_obesity"
adult_obesity_fem$disaggregation = "gender"
adult_obesity_fem$disagg.value = "Female"
adult_obesity_fem$component = "H"
master_dat_list[[master_dat_index]] = adult_obesity_fem
master_dat_index = master_dat_index + 1

dat = read.csv("ADULT STATUS - GNR_adult_18plus_female_overweight_country.csv",na.strings="",as.is=T)
adult_overweight_fem = dat[c("country","year","mean")]
names(adult_overweight_fem) = c("country","year","value")
adult_overweight_fem$indicator = "adult_overweight"
adult_overweight_fem$disaggregation = "gender"
adult_overweight_fem$disagg.value = "Female"
adult_overweight_fem$component = "H"
master_dat_list[[master_dat_index]] = adult_overweight_fem
master_dat_index = master_dat_index + 1

dat = read.csv("ADULT STATUS - GNR_adult_18plus_male_overweight_country.csv",na.strings="",as.is=T)
adult_overweight_male = dat[c("country","year","mean")]
names(adult_overweight_male) = c("country","year","value")
adult_overweight_male$indicator = "adult_overweight"
adult_overweight_male$disaggregation = "gender"
adult_overweight_male$disagg.value = "Male"
adult_overweight_male$component = "H"
master_dat_list[[master_dat_index]] = adult_overweight_male
master_dat_index = master_dat_index + 1

dat = read.csv("ADULT STATUS - Prevalence of anemia among women (%) (World Bank).csv",skip=4,na.strings="",as.is=T)
adult_anemia_fem = dat[c("Country.Name","Country.Code",paste0("X",1960:2017))]
adult_anemia_fem = melt(adult_anemia_fem,id.vars=c("Country.Name","Country.Code"))
names(adult_anemia_fem) = c("country","iso3","year","value")
adult_anemia_fem$year = substr(adult_anemia_fem$year,2,5)
adult_anemia_fem$indicator = "adult_anemia"
adult_anemia_fem$disaggregation = "pregnancy"
adult_anemia_fem$disagg.value = "All women"
adult_anemia_fem$component = "I"
master_dat_list[[master_dat_index]] = adult_anemia_fem
master_dat_index = master_dat_index + 1

dat = read.csv("ADULT STATUS - sodium_with_location_names.csv",na.strings="",as.is=T)
adult_sodium = dat[c("location_name","ihme_loc_id","year_id","mapvar")]
names(adult_sodium) = c("country","iso3","year","value")
adult_sodium$indicator = "adult_sodium"
adult_sodium$disaggregation = "all"
adult_sodium$disagg.value = ""
adult_sodium$component = "I"
master_dat_list[[master_dat_index]] = adult_sodium
master_dat_index = master_dat_index + 1

dat = read.csv("DIETARY_NEEDS_2016_national.csv",na.strings="",as.is=T)
indicators = c(
  "Diet low in nuts and seeds (g/day)",
  "Diet low in milk (g/day)",
  "Diet low in calcium (g/day)",
  "Diet low in fruits (g/day)",
  "Diet low in whole grains (g/day)",
  "Diet low in seafood omega-3 fatty acids (g/day)",
  "Diet low in polyunsaturated fatty acids (% energy/day)",
  "Diet low in vegetables (g/day)",
  "Diet low in legumes (g/day)",
  "Diet high in trans fatty acids (% energy/day)",
  "Diet high in saturated fatty acids (% energy/day)",
  "Diet high in red meat (g/day)",
  "Diet high in processed meat (g/day)",
  "Diet high in sodium (g/day)",
  "Diet high in sugar-sweetened beverages (g/day)"
)
ind.names = c(
  "Nuts and seeds",
  "Milk",
  "Calcium",
  "Fruit",
  "Whole grain",
  "Omega 3",
  "Polyunsaturated fat",
  "Vegetables",
  "Legumes",
  "Trans fat",
  "Saturated fat",
  "Red meat",
  "Processed meat",
  "Sodium",
  "Sugar-sweetened beverages"
)
ind.units = c(
  "g",
  "g",
  "g",
  "g",
  "g",
  "g",
  "%",
  "g",
  "g",
  "%",
  "%",
  "g",
  "g",
  "g",
  "g"
)
recs = c(
  20.5,
  435,
  1.25,
  250,
  125,
  0.25,
  11,
  360,
  60,
  0.5,
  7,
  22.5,
  2,
  2,
  2.5
)
unit.dat = data.frame(modelable_entity_name=indicators,indicator=ind.names,unit=ind.units,rec=recs)
diet_mapping = read.csv("dietary_mapping.csv",na.strings="")
dat = subset(dat,age_group_id==27)
dat = subset(dat,sex_id==3)
dietary_needs = dat[c("location_name","year_id","mean","modelable_entity_name")]
dietary_needs = merge(dietary_needs,unit.dat,by="modelable_entity_name")
dietary_needs$modelable_entity_name = NULL
names(dietary_needs) = c("country","year","value","indicator","unit","rec")
dietary_needs$disaggregation = "location"
dietary_needs$disagg.value = "National"
dietary_needs$component = "M"
dietary_needs = merge(dietary_needs,diet_mapping,by="country")
dietary_needs$region = NULL
master_dat_list[[master_dat_index]] = dietary_needs
master_dat_index = master_dat_index + 1

dat = read.csv("DIETARY_NEEDS_2016_regional.csv",na.strings="",as.is=T)
#TODO: Check this is the filter we want
dat = subset(dat,age_group_name=="age-standardized 25+")
dat = subset(dat,sex_id==3)
dat.global = data.table(dat)[,.(value=weighted.mean(mean,population)),by=.(year_id,modelable_entity_name)]
dietary_needs_reg = dat[c("region_name","year_id","mean","modelable_entity_name")]
dietary_needs_reg = merge(dietary_needs_reg,unit.dat,by="modelable_entity_name")
dietary_needs_reg$modelable_entity_name = NULL
names(dietary_needs_reg) = c("region","year","value","indicator","unit","rec")
dietary_needs_reg$disaggregation = "location"
dietary_needs_reg$disagg.value = "Regional"
dietary_needs_reg$component = "M"
dietary_needs_reg = join(diet_mapping,dietary_needs_reg,by="region")
dietary_needs_reg$region = NULL
master_dat_list[[master_dat_index]] = dietary_needs_reg
master_dat_index = master_dat_index + 1

dietary_needs_global = merge(dat.global,unit.dat,by="modelable_entity_name")
dietary_needs_global$modelable_entity_name = NULL
names(dietary_needs_global) = c("year","value","indicator","unit","rec")
dietary_needs_global$global = "global"
dietary_needs_global$disaggregation = "location"
dietary_needs_global$disagg.value = "Global"
dietary_needs_global$component = "M"
diet_mapping$global = "global"
dietary_needs_global = join(diet_mapping,dietary_needs_global,by="global")
dietary_needs_global$global = NULL
dietary_needs_global$region = NULL
master_dat_list[[master_dat_index]] = dietary_needs_global
master_dat_index = master_dat_index + 1

dat = read.csv("pov190.csv",na.strings="",as.is=T)
pov190 = dat[c("CountryCode","CountryName","RequestYear","HeadCount")]
names(pov190) = c("iso3","country","year","value")
pov190$indicator = "190_percent"
pov190$disaggregation = "all"
pov190$component = "R"
pov190$value = 100*as.numeric(pov190$value)
master_dat_list[[master_dat_index]] = pov190
master_dat_index = master_dat_index + 1

dat = read.csv("povcal_320.csv",na.strings="",as.is=T)
pov320 = dat[c("CountryCode","CountryName","RequestYear","HeadCount")]
names(pov320) = c("iso3","country","year","value")
pov320$indicator = "320_percent"
pov320$disaggregation = "all"
pov320$component = "R"
pov320$value = 100*as.numeric(pov320$value)
master_dat_list[[master_dat_index]] = pov320
master_dat_index = master_dat_index + 1

dat = read.csv("UNDERLYING_Government Expenditure.csv",na.strings="",as.is=T)
gov_exp = melt(dat,id.vars=c("ISO","country"))
gov_exp$variable = unfactor(gov_exp$variable)
gov_exp$year = substr(gov_exp$variable,nchar(gov_exp$variable)-3,nchar(gov_exp$variable))
gov_exp$indicator = substr(gov_exp$variable,1,nchar(gov_exp$variable)-4)
gov_exp$variable = NULL
names(gov_exp) = c("iso3","country","value","year","indicator")
indicators = c(
  "totag_ppp",
  "toteducation_ppp",
  "tothealth_ppp",
  "totsp_ppp"
)
ind.names = c(
  "agriculture_expenditure",
  "education_spending",
  "health_spending",
  "social_protection_spending"
)
for(i in 1:length(indicators)){
  ind = indicators[i]
  ind.name = ind.names[i]
  gov_exp$indicator[which(gov_exp$indicator==ind)] = ind.name
}
gov_exp$component = "V"
master_dat_list[[master_dat_index]] = gov_exp
master_dat_index = master_dat_index + 1

dat = read.csv("u5mr_wdi.csv",na.strings="",as.is=T)
u5mr = dat[c("country","iso3c","year","SH.DYN.MORT")]
names(u5mr) = c("country","iso3","year","value")
u5mr$component = "R"
u5mr$indicator = "u5mr"
u5mr$disaggregation = "all"
master_dat_list[[master_dat_index]] = u5mr
master_dat_index = master_dat_index + 1

dat = read.csv("fem_sec_enrol_net.csv",na.strings="",as.is=T)
female_secondary_enroll_net = dat[c("country","iso3c","year","SE.SEC.NENR.FE")]
names(female_secondary_enroll_net) = c("country","iso3","year","value")
female_secondary_enroll_net$component = "U"
female_secondary_enroll_net$indicator = "female_secondary_enroll_net"
female_secondary_enroll_net$disaggregation = "all"
master_dat_list[[master_dat_index]] = female_secondary_enroll_net
master_dat_index = master_dat_index + 1

dat = read.xls("ADULT STATUS - Anaemia pregnant.xls",sheet=1,na.strings="",skip=2)
adult_anemia_preg = dat[c("Country.Name","Country.Code",paste0("X",1960:2017))]
adult_anemia_preg = melt(adult_anemia_preg,id.vars=c("Country.Name","Country.Code"))
names(adult_anemia_preg) = c("country","iso3","year","value")
adult_anemia_preg$year = substr(adult_anemia_preg$year,2,5)
adult_anemia_preg$indicator = "adult_anemia"
adult_anemia_preg$disaggregation = "pregnancy"
adult_anemia_preg$disagg.value = "Pregnant women"
adult_anemia_preg$component = "I"
master_dat_list[[master_dat_index]] = adult_anemia_preg
master_dat_index = master_dat_index + 1

dat = read.xls("ADULT STATUS - Anamia non-pregnant.xls",sheet=1,na.strings="",skip=2)
adult_anemia_nonpreg = dat[c("Country.Name","Country.Code",paste0("X",1960:2017))]
adult_anemia_nonpreg = melt(adult_anemia_nonpreg,id.vars=c("Country.Name","Country.Code"))
names(adult_anemia_nonpreg) = c("country","iso3","year","value")
adult_anemia_nonpreg$year = substr(adult_anemia_nonpreg$year,2,5)
adult_anemia_nonpreg$indicator = "adult_anemia"
adult_anemia_nonpreg$disaggregation = "pregnancy"
adult_anemia_nonpreg$disagg.value = "Non-pregnant women"
adult_anemia_nonpreg$component = "I"
master_dat_list[[master_dat_index]] = adult_anemia_nonpreg
master_dat_index = master_dat_index + 1

dat = read.xlsx("ADULT STATUS - Age standardised Country-Region-World NCD-RisC_Lancet_2016_Diabetes_age_standardised_by_Country_Region_World.xlsx")
adult_diabetes = dat[c("Country/Region/World","Year","Sex","Age-standardised.diabetes.prevalence")]
names(adult_diabetes) = c("country","year","disagg.value","value")
adult_diabetes$indicator = "adult_diabetes"
adult_diabetes$disaggregation = "gender"
adult_diabetes$component = "H"
adult_diabetes$disagg.value[which(adult_diabetes$disagg.value=="Men")] = "Male"
adult_diabetes$disagg.value[which(adult_diabetes$disagg.value=="Women")] = "Female"
master_dat_list[[master_dat_index]] = adult_diabetes
master_dat_index = master_dat_index + 1

dat = read.xlsx("ADULT STATUS - Age standardised Country-Region-World NCD-RisC_Lancet_2017_BP_age_standardised_by_Country_Region_World.xlsx")
adult_blood_pressure = dat[c("Country/Region/World","Year","Sex","Prevalence.of.raised.blood.pressure")]
names(adult_blood_pressure) = c("country","year","disagg.value","value")
adult_blood_pressure$indicator = "adult_blood_pressure"
adult_blood_pressure$disaggregation = "gender"
adult_blood_pressure$component = "H"
adult_blood_pressure$disagg.value[which(adult_blood_pressure$disagg.value=="Men")] = "Male"
adult_blood_pressure$disagg.value[which(adult_blood_pressure$disagg.value=="Women")] = "Female"
master_dat_list[[master_dat_index]] = adult_blood_pressure
master_dat_index = master_dat_index + 1

dat = read.xlsx("ADULT STATUS - GNR_adult_18plus_male_obesity_country.xlsx")
adult_obesity_male = dat[c("country","year","mean")]
names(adult_obesity_male) = c("country","year","value")
adult_obesity_male$indicator = "adult_obesity"
adult_obesity_male$disaggregation = "gender"
adult_obesity_male$disagg.value = "Male"
adult_obesity_male$component = "H"
master_dat_list[[master_dat_index]] = adult_obesity_male
master_dat_index = master_dat_index + 1

dat = read.xlsx("CHILD COEXISTENCE - UNICEF_Global_Databases_Stunting_Wasting_Overweight_May_2018.xlsx",sheet=3,rows=c(7:115))
dat = subset(dat,!is.na(ISO))
coexistence = dat[c(
  "ISO",
  "Countries.and.areas",
  "Year*",
  "Wasted.Only",
  "Wasted.and.Stunted",
  "Stunted.Only",
  "Stunted.and.Overweight",
  "Overweight.Only",
  "Free.from.Wasting,.Overweight,.Stunting"
)]
coexistence = melt(coexistence,id.vars=c("ISO","Countries.and.areas","Year*"))
names(coexistence) = c("iso3","country","year","disagg.value","value")
coexistence$indicator = "coexistence"
indicators = c(
  "Wasted.Only",
  "Wasted.and.Stunted",
  "Stunted.Only",
  "Stunted.and.Overweight",
  "Overweight.Only",
  "Free.from.Wasting,.Overweight,.Stunting"
)
ind.names = c(
  "Wasting alone",
  "Wasting and stunting",
  "Stunting alone",
  "Stunting and overweight",
  "Overweight alone",
  "Free from"
)
coexistence$disagg.value = unfactor(coexistence$disagg.value)
for(i in 1:length(indicators)){
  ind = indicators[i]
  ind.name = ind.names[i]
  coexistence$disagg.value[which(coexistence$disagg.value==ind)] = ind.name
}
coexistence$disaggregation = "all"
coexistence$component = "G"
master_dat_list[[master_dat_index]] = coexistence
master_dat_index = master_dat_index + 1

# Using IYCF instead
# dat = read.xlsx("CHILD FEEDING - Differences in infant and young child feeding practices.xlsx",rows=c(2:197))
# dat = subset(dat,X1!="ISO")
# indicators = c(
#   "early_initiation",
#   "exclusive_breastfeeding",
#   "solid_foods",
#   "minimum_meal",
#   "minimum_diet_diversity",
#   "minimum_accept_diet",
#   "continued_breastfeeding_1yr",
#   "continued_breastfeeding_2yr"
# )
# disagg.values = c("Poorest quintile","Wealthiest quintile")
# start_col = 3
# names(dat)[1:2] = c("iso3","country")
# for(disagg.value in disagg.values){
#   for(ind in indicators){
#     new_name = paste(ind,disagg.value,sep=".")
#     names(dat)[start_col] = new_name
#     start_col = start_col + 1
#   }
# }
# child_feeding_income = melt(dat,id.vars=c("iso3","country"))
# child_feeding_income$variable = unfactor(child_feeding_income$variable)
# child_feeding_income$indicator = sapply(strsplit(child_feeding_income$variable,split=".",fixed=T),`[`,index=1)
# child_feeding_income$disagg.value = sapply(strsplit(child_feeding_income$variable,split=".",fixed=T),`[`,index=2)
# child_feeding_income$variable = NULL
# child_feeding_income$disaggregation = "income"
# child_feeding_income$component = "J"
# master_dat_list[[master_dat_index]] = child_feeding_income
# master_dat_index = master_dat_index + 1
# 
# dat = read.xlsx("CHILD FEEDING - Differences in infant and young child feeding practices.xlsx",rows=c(202:397))
# dat = subset(dat,X1!="ISO")
# indicators = c(
#   "early_initiation",
#   "exclusive_breastfeeding",
#   "solid_foods",
#   "minimum_meal",
#   "minimum_diet_diversity",
#   "minimum_accept_diet",
#   "continued_breastfeeding_1yr",
#   "continued_breastfeeding_2yr"
# )
# disagg.values = c("Rural","Urban")
# start_col = 3
# names(dat)[1:2] = c("iso3","country")
# for(disagg.value in disagg.values){
#   for(ind in indicators){
#     new_name = paste(ind,disagg.value,sep=".")
#     names(dat)[start_col] = new_name
#     start_col = start_col + 1
#   }
# }
# child_feeding_location = melt(dat,id.vars=c("iso3","country"))
# child_feeding_location$variable = unfactor(child_feeding_location$variable)
# child_feeding_location$indicator = sapply(strsplit(child_feeding_location$variable,split=".",fixed=T),`[`,index=1)
# child_feeding_location$disagg.value = sapply(strsplit(child_feeding_location$variable,split=".",fixed=T),`[`,index=2)
# child_feeding_location$variable = NULL
# child_feeding_location$disaggregation = "location"
# child_feeding_location$component = "J"
# master_dat_list[[master_dat_index]] = child_feeding_location
# master_dat_index = master_dat_index + 1

# Skipping "CHILD STATUS - Co-existence of wasting, stunting and overweight in children under five with numbers.xlsx"

# Replaced with expanded DBs
# dat = read.xlsx("CHILD STATUS - numbers affected JME chika.xlsx",sheet=1,rows=c(14:847))
# dat = subset(dat,!is.na(ISO.code))
# child_jme = dat[c("ISO.code","Country.and.areas","Year*","Wasting","Stunting","Overweight")]
# names(child_jme) = c("iso3","country","year","wasting_percent","stunting_percent","overweight_percent")
# child_jme = melt(child_jme,id.vars=c("iso3","country","year"))
# setnames(child_jme,"variable","indicator")
# child_jme$disaggregation = "gender"
# child_jme$disagg.value = "Both"
# child_jme$component = "C"
# master_dat_list[[master_dat_index]] = child_jme
# master_dat_index = master_dat_index + 1

dat = read.xlsx("CHILD STATUS - UNICEF_Expanded_Global_Databases_Overweight_May_2018.xlsx",sheet=2,rows=c(7:693),na.strings="")
dat = subset(dat,!is.na(ISO))
child_overweight = dat[c("ISO","Countries.and.areas","Year*","National","Male","Female","Urban.1","Rural.1","Wealth.Quintile.1.1","Wealth.Quintile.2","Wealth.Quintile.3.1","Wealth.Quintile.4.1","Wealth.Quintile.5.1")]
names(child_overweight) = c("iso3","country","year","Both","Boys","Girls","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
child_overweight = melt(child_overweight,id.vars=c("iso3","country","year"))
setnames(child_overweight,"variable","disagg.value")
child_overweight$indicator = "overweight_percent"
child_overweight$component = "C"
child_overweight_gender = subset(child_overweight,disagg.value %in% c("Boys","Girls","Both"))
child_overweight_gender$disaggregation = "gender"
child_overweight_location = subset(child_overweight,disagg.value %in% c("Urban","Rural"))
child_overweight_location$disaggregation = "location"
child_overweight_income = subset(child_overweight,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
child_overweight_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = child_overweight_gender
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = child_overweight_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = child_overweight_income
master_dat_index = master_dat_index + 1

dat = read.xlsx("CHILD STATUS - UNICEF_Expanded_Global_Databases_Stunting_May_2018c.xlsx",na.strings="")
dat = subset(dat,!is.na(ISO))
child_stunting = dat[c("ISO","Countries.and.areas","Year*","National","Male","Female","Urban.1","Rural.1","Wealth.Quintile.1.1","Wealth.Quintile.2","Wealth.Quintile.3.1","Wealth.Quintile.4.1","Wealth.Quintile.5.1")]
names(child_stunting) = c("iso3","country","year","Both","Boys","Girls","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
child_stunting = melt(child_stunting,id.vars=c("iso3","country","year"))
setnames(child_stunting,"variable","disagg.value")
child_stunting$indicator = "stunting_percent"
child_stunting$component = "C"
child_stunting_gender = subset(child_stunting,disagg.value %in% c("Boys","Girls","Both"))
child_stunting_gender$disaggregation = "gender"
child_stunting_location = subset(child_stunting,disagg.value %in% c("Urban","Rural"))
child_stunting_location$disaggregation = "location"
child_stunting_income = subset(child_stunting,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
child_stunting_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = child_stunting_gender
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = child_stunting_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = child_stunting_income
master_dat_index = master_dat_index + 1

dat = read.xlsx("CHILD STATUS - UNICEF_Expanded_Global_Databases_Wasting_May_2018b.xlsx",na.strings="")
dat = subset(dat,!is.na(ISO))
child_wasting = dat[c("ISO","Countries.and.areas","Year*","National","Male","Female","Urban.1","Rural.1","Wealth.Quintile.1.1","Wealth.Quintile.2","Wealth.Quintile.3.1","Wealth.Quintile.4.1","Wealth.Quintile.5.1")]
names(child_wasting) = c("iso3","country","year","Both","Boys","Girls","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
child_wasting = melt(child_wasting,id.vars=c("iso3","country","year"))
setnames(child_wasting,"variable","disagg.value")
child_wasting$indicator = "wasting_percent"
child_wasting$component = "C"
child_wasting_gender = subset(child_wasting,disagg.value %in% c("Boys","Girls","Both"))
child_wasting_gender$disaggregation = "gender"
child_wasting_location = subset(child_wasting,disagg.value %in% c("Urban","Rural"))
child_wasting_location$disaggregation = "location"
child_wasting_income = subset(child_wasting,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
child_wasting_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = child_wasting_gender
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = child_wasting_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = child_wasting_income
master_dat_index = master_dat_index + 1

dat = read.xlsx("ECONOMICS AND DEMOGRAPHY_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx",sheet=2,rows=c(17:20743))
u5_pop = dat[c("Region,.subregion,.country.or.area.*","Reference.date.(as.of.1.July)","0-4")]
names(u5_pop) = c("country","year","value")
u5_pop = subset(u5_pop,year==2018)
u5_pop$component = "R"
u5_pop$indicator = "u5_pop"
u5_pop$disaggregation = "all"
u5_pop = subset(u5_pop,country!="Micronesia")
master_dat_list[[master_dat_index]] = u5_pop
master_dat_index = master_dat_index + 1

dat = read.xlsx("ECONOMICS AND DEMOGRAPHY_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx",sheet=2,rows=c(17:20743))
dat$value = rowSums(dat[c(20:27)])
o65_pop = dat[c(3,6,28)]
names(o65_pop) = c("country","year","value")
o65_pop = subset(o65_pop,year==2018)
o65_pop$component = "R"
o65_pop$indicator = "65_years"
o65_pop$disaggregation = "all"
o65_pop = subset(o65_pop,country!="Micronesia")
master_dat_list[[master_dat_index]] = o65_pop
master_dat_index = master_dat_index + 1

dat = read.xlsx("ECONOMICS AND DEMOGRAPHY total pop country and region.xlsx",sheet=2,rows=c(17:290))
population = dat[c("Region,.subregion,.country.or.area.*","2017")]
names(population) = c("country","value")
population$year = 2017
population$component = "R"
population$indicator = "population"
population$disaggregation = "all"
population = subset(population,country!="Micronesia")
master_dat_list[[master_dat_index]] = population
master_dat_index = master_dat_index + 1

dat = read.xlsx("ECONOMICS AND DEMOGRAPHY_GDP PER CAPITA_COUNTRY.xlsx",rows=c(4:268))
gdp_per_cap = dat[c("Country.Name","Country.Code",as.character(c(1990:2017)))]
gdp_per_cap = melt(gdp_per_cap,id.vars=c("Country.Name","Country.Code"))
names(gdp_per_cap) = c("country","iso3","year","value")
gdp_per_cap$indicator = "GDP_capita_PPP"
gdp_per_cap$component = "R"
gdp_per_cap$disaggregation = "all"
master_dat_list[[master_dat_index]] = gdp_per_cap
master_dat_index = master_dat_index + 1

# Which max here?
dat = read.xlsx("ECONOMICS AND DEMOGRAPHY_GINI.xlsx",rows=c(4:268))
gini = dat[c("Country.Name","Country.Code",as.character(c(2000:2017)))]
gini = melt(gini,id.vars=c("Country.Name","Country.Code"))
names(gini) = c("country","iso3","year","value")
gini$indicator = "gini"
gini$component = "R"
gini$disaggregation = "all"
gini = subset(gini,!is.na(value))
gini = data.table(gini)
gini = gini[,.SD[which.max(.SD$year)],by=.(country)]
master_dat_list[[master_dat_index]] = gini
master_dat_index = master_dat_index + 1

gini_rank = gini
gini_rank$indicator = "gini_rank"
gini_rank$value = rank(gini_rank$value)
master_dat_list[[master_dat_index]] = gini_rank
master_dat_index = master_dat_index + 1

dat = read.xlsx("ECONOMICS AND DEMOGRAPHY_RURAL_POP_PERCENTAGE.xlsx",rows=c(4:268))
rural = dat[c("Country.Name","Country.Code","2017")]
names(rural) = c("country","iso3","value")
rural$year = 2017
rural$indicator = "rural_percent"
rural$component = "R"
rural$disaggregation = "all"
master_dat_list[[master_dat_index]] = rural
master_dat_index = master_dat_index + 1

# Replaced with pov190.csv
# dat = read.xlsx("ECONOMICS AND DEMOGRAPHY_Povery rates_1.90.xlsx",rows=c(4:268))
# pov190 = dat[c("Country.Name","Country.Code",as.character(c(2000:2017)))]
# pov190 = melt(pov190,id.vars=c("Country.Name","Country.Code"))
# names(pov190) = c("country","iso3","year","value")
# pov190$indicator = "190_percent"
# pov190$component = "R"
# pov190$disaggregation = "all"
# master_dat_list[[master_dat_index]] = pov190
# master_dat_index = master_dat_index + 1

# Replaced with u5mr csv
# dat = read.xlsx("ECONOMICS AND DEMOGRAPHY U5 mortality.xlsx",sheet=1,rows=c(17:297))
# u5mr = dat[c("Region,.subregion,.country.or.area.*","2010-2015")]
# names(u5mr) = c("country","value")
# u5mr$year = 2015
# u5mr$component = "R"
# u5mr$indicator = "u5mr"
# u5mr$disaggregation = "all"
# master_dat_list[[master_dat_index]] = u5mr
# master_dat_index = master_dat_index + 1

dat = read.xlsx("FINANCIAL - ODA.xlsx",sheet=1,rows=c(11:369),cols=c(1:12),na.strings=c("NA"))
names(dat)= c(
  "sector","country","iso3",
  as.character(c(2008:2016))
)
dat$sector[c(1:179)] = "total"
dat$sector[c(180:358)] = "ODA_specific"
oda_recip = melt(dat,id.vars=c("country","iso3","sector"),variable.name="year")
oda_recip = dcast(oda_recip,country+iso3+year~sector)
oda_recip$ODA_specific[which(is.na(oda_recip$ODA_specific))] = 0
oda_recip$ODA_received = as.numeric(oda_recip$ODA_specific)/as.numeric(oda_recip$total)
oda_recip$ODA_received[which(is.na(oda_recip$ODA_received))] = 0
oda_recip$ODA_specific[which(is.na(oda_recip$ODA_specific))] = 0
oda_recip$total = NULL
oda_recip = melt(oda_recip,id.vars=c("country","iso3","year"),variable.name="indicator")
oda_recip$component = "P"
oda_recip$disaggregation = "all"
oda_recip$recip = T
oda_recip = subset(oda_recip,country!="States Ex-Yugoslavia unspecified")
oda_recip = subset(oda_recip,!country %in% c("United Arab Emirates","Kuwait","Korea","Slovenia"))
master_dat_list[[master_dat_index]] = oda_recip
master_dat_index = master_dat_index + 1

dat = read.xlsx("FINANCIAL - ODA.xlsx",sheet=2,rows=c(11:109),cols=c(1:12),na.strings=c("NA"))
names(dat)= c(
  "sector","country","iso3",
  as.character(c(2008:2016))
)
dat$sector[c(1:49)] = "total"
dat$sector[c(50:98)] = "ODA_specific"
oda_donor = melt(dat,id.vars=c("country","iso3","sector"),variable.name="year")
oda_donor = dcast(oda_donor,country+iso3+year~sector)
oda_donor$ODA_specific[which(is.na(oda_donor$ODA_specific))] = 0
oda_donor$ODA_received = as.numeric(oda_donor$ODA_specific)/as.numeric(oda_donor$total)
oda_donor$ODA_received[which(is.na(oda_donor$ODA_received))] = 0
oda_donor$ODA_specific[which(is.na(oda_donor$ODA_specific))] = 0
oda_donor$total = NULL
oda_donor = melt(oda_donor,id.vars=c("country","iso3","year"),variable.name="indicator")
oda_donor$component = "P"
oda_donor$disaggregation = "all"
oda_donor$recip = F
oda_donor = subset(oda_donor,country!="States Ex-Yugoslavia unspecified")
oda_donor = subset(oda_donor,!country %in% c("Azerbaijan","Cyprus","Croatia","Israel","Kazakhstan","Malta","SaudiArabia","Timor-Leste","Thailand","Turkey"))
master_dat_list[[master_dat_index]] = oda_donor
master_dat_index = master_dat_index + 1

dat = read.xlsx("INTERVENTION - Coverage of micronutrient supplementation programmes and salt iodization.xlsx",sheet=8,rows=c(3:59),cols=c(2,3,4,6))
dat = subset(dat,!is.na(X1))
iodised_salt = dat
names(iodised_salt) = c("iso3","country","survey","value")
iodised_salt$year = substr(iodised_salt$survey,1,4)
iodised_salt$survey = NULL
iodised_salt$component = "P"
iodised_salt$disaggregation = "all"
iodised_salt$indicator = "iodised_salt"
master_dat_list[[master_dat_index]] = iodised_salt
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "INTERVENTION _ POSSIBLE DISAGG - Copy of Micronutritent Coverage Tables GNR_GNR_30 may.xlsx"
  ,sheet=3
  ,rows=c(3:66)
  ,cols=c(2,3,7)
  ,na.strings="N/A"
)
diarrhea_zinc = dat
names(diarrhea_zinc) = c("country","survey","value")
diarrhea_zinc$year = substr(diarrhea_zinc$survey,1,4)
diarrhea_zinc$survey = NULL
diarrhea_zinc$component = "P"
diarrhea_zinc$disaggregation = "all"
diarrhea_zinc$indicator = "diarrhea_zinc"
master_dat_list[[master_dat_index]] = diarrhea_zinc
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "INTERVENTION _ POSSIBLE DISAGG - Copy of Micronutritent Coverage Tables GNR_GNR_30 may.xlsx"
  ,sheet=4
  ,rows=c(3:59)
  ,cols=c(2,3,5:7)
  ,na.strings="N/A"
)
iron_supp = dat
names(iron_supp) = c("country","survey","Both","Boys","Girls")
iron_supp$year = substr(iron_supp$survey,1,4)
iron_supp$survey = NULL
iron_supp = melt(iron_supp,id.vars=c("country","year"),variable.name="disagg.value")
iron_supp$component = "P"
iron_supp$disaggregation = "gender"
iron_supp$indicator = "iron_supp"
master_dat_list[[master_dat_index]] = iron_supp
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "INTERVENTION _ POSSIBLE DISAGG - Copy of Micronutritent Coverage Tables GNR_GNR_30 may.xlsx"
  ,sheet=6
  ,rows=c(5:63)
  ,cols=c(2,3,5:7)
  ,na.strings="N/A"
)
vit_a = dat
names(vit_a) = c("country","survey","Both","Boys","Girls")
vit_a$year = substr(vit_a$survey,1,4)
vit_a$survey = NULL
vit_a = melt(vit_a,id.vars=c("country","year"),variable.name="disagg.value")
vit_a$component = "P"
vit_a$disaggregation = "gender"
vit_a$indicator = "vit_a"
master_dat_list[[master_dat_index]] = vit_a
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "INTERVENTION _ POSSIBLE DISAGG - Copy of Micronutritent Coverage Tables GNR_GNR_30 may.xlsx"
  ,sheet=8
  ,rows=c(4:66)
  ,cols=c(2,3,5)
  ,na.strings="N/A"
)
iron_and_folic = dat
names(iron_and_folic) = c("country","survey","value")
iron_and_folic$year = substr(iron_and_folic$survey,1,4)
iron_and_folic$survey = NULL
iron_and_folic$component = "P"
iron_and_folic$disaggregation = "all"
iron_and_folic$indicator = "iron_and_folic"
master_dat_list[[master_dat_index]] = iron_and_folic
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF - UNICEF_Expanded_Global_Databases_Complementary_Feeding_May2018.xlsx"
  ,sheet=2
  ,rows=c(8:99)
  ,cols=c(2:4,23,28,58,63,68,73,78)
  ,na.strings=""
  ,fillMergedCells = T
)
minimum_accept_diet = dat
names(minimum_accept_diet) = c("iso3","country","survey","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
minimum_accept_diet$year = substr(minimum_accept_diet$survey,1,4)
minimum_accept_diet$survey = NULL
minimum_accept_diet = melt(minimum_accept_diet,id.vars=c("iso3","country","year"))
setnames(minimum_accept_diet,"variable","disagg.value")
minimum_accept_diet$indicator = "minimum_accept_diet"
minimum_accept_diet$component = "K"
minimum_accept_diet_location = subset(minimum_accept_diet,disagg.value %in% c("Urban","Rural"))
minimum_accept_diet_location$disaggregation = "location"
minimum_accept_diet_income = subset(minimum_accept_diet,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
minimum_accept_diet_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = minimum_accept_diet_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = minimum_accept_diet_income
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF - UNICEF_Expanded_Global_Databases_Complementary_Feeding_May2018.xlsx"
  ,sheet=3
  ,rows=c(8:139)
  ,cols=c(2:4,23,28,58,63,68,73,78)
  ,na.strings=""
  ,fillMergedCells = T
)
minimum_diet_diversity = dat
names(minimum_diet_diversity) = c("iso3","country","survey","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
minimum_diet_diversity$year = substr(minimum_diet_diversity$survey,1,4)
minimum_diet_diversity$survey = NULL
minimum_diet_diversity = melt(minimum_diet_diversity,id.vars=c("iso3","country","year"))
setnames(minimum_diet_diversity,"variable","disagg.value")
minimum_diet_diversity$indicator = "minimum_diet_diversity"
minimum_diet_diversity$component = "K"
minimum_diet_diversity_location = subset(minimum_diet_diversity,disagg.value %in% c("Urban","Rural"))
minimum_diet_diversity_location$disaggregation = "location"
minimum_diet_diversity_income = subset(minimum_diet_diversity,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
minimum_diet_diversity_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = minimum_diet_diversity_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = minimum_diet_diversity_income
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF - UNICEF_Expanded_Global_Databases_Complementary_Feeding_May2018.xlsx"
  ,sheet=4
  ,rows=c(8:148)
  ,cols=c(2:4,23,28,58,63,68,73,78)
  ,na.strings=""
  ,fillMergedCells = T
)
minimum_meal = dat
names(minimum_meal) = c("iso3","country","survey","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
minimum_meal$year = substr(minimum_meal$survey,1,4)
minimum_meal$survey = NULL
minimum_meal = melt(minimum_meal,id.vars=c("iso3","country","year"))
setnames(minimum_meal,"variable","disagg.value")
minimum_meal$indicator = "minimum_meal"
minimum_meal$component = "K"
minimum_meal_location = subset(minimum_meal,disagg.value %in% c("Urban","Rural"))
minimum_meal_location$disaggregation = "location"
minimum_meal_income = subset(minimum_meal,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
minimum_meal_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = minimum_meal_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = minimum_meal_income
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF - UNICEF_Expanded_Global_Databases_Continued-Breastfeeding_May2018b.xlsx"
  ,sheet=1
  ,cols=c(2:4,23,28,33,38,43,48,53)
  ,na.strings=""
)
dat = subset(dat,!is.na(ISO))
continued_breastfeeding_1yr = dat
names(continued_breastfeeding_1yr) = c("iso3","country","survey","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
continued_breastfeeding_1yr$year = substr(continued_breastfeeding_1yr$survey,1,4)
continued_breastfeeding_1yr$survey = NULL
continued_breastfeeding_1yr = melt(continued_breastfeeding_1yr,id.vars=c("iso3","country","year"))
setnames(continued_breastfeeding_1yr,"variable","disagg.value")
continued_breastfeeding_1yr$indicator = "continued_breastfeeding_1yr"
continued_breastfeeding_1yr$component = "K"
continued_breastfeeding_1yr_location = subset(continued_breastfeeding_1yr,disagg.value %in% c("Urban","Rural"))
continued_breastfeeding_1yr_location$disaggregation = "location"
continued_breastfeeding_1yr_income = subset(continued_breastfeeding_1yr,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
continued_breastfeeding_1yr_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = continued_breastfeeding_1yr_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = continued_breastfeeding_1yr_income
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF - UNICEF_Expanded_Global_Databases_Continued-Breastfeeding_May2018b.xlsx"
  ,sheet=2
  ,cols=c(2:4,23,28,33,38,43,48,53)
  ,na.strings=""
)
dat = subset(dat,!is.na(ISO))
continued_breastfeeding_2yr = dat
names(continued_breastfeeding_2yr) = c("iso3","country","survey","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
continued_breastfeeding_2yr$year = substr(continued_breastfeeding_2yr$survey,1,4)
continued_breastfeeding_2yr$survey = NULL
continued_breastfeeding_2yr = melt(continued_breastfeeding_2yr,id.vars=c("iso3","country","year"))
setnames(continued_breastfeeding_2yr,"variable","disagg.value")
continued_breastfeeding_2yr$indicator = "continued_breastfeeding_2yr"
continued_breastfeeding_2yr$component = "K"
continued_breastfeeding_2yr_location = subset(continued_breastfeeding_2yr,disagg.value %in% c("Urban","Rural"))
continued_breastfeeding_2yr_location$disaggregation = "location"
continued_breastfeeding_2yr_income = subset(continued_breastfeeding_2yr,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
continued_breastfeeding_2yr_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = continued_breastfeeding_2yr_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = continued_breastfeeding_2yr_income
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF -  UNICEF_Expanded_Global_Databases_Early_Initiation_May2018.xlsx"
  ,sheet=2
  ,rows = c(8:412)
  ,cols=c(2,3,5,24,29,34,39,44,49,54)
  ,na.strings=""
)
early_initiation = dat
names(early_initiation) = c("iso3","country","year","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
early_initiation = melt(early_initiation,id.vars=c("iso3","country","year"))
setnames(early_initiation,"variable","disagg.value")
early_initiation$indicator = "early_initiation"
early_initiation$component = "K"
early_initiation_location = subset(early_initiation,disagg.value %in% c("Urban","Rural"))
early_initiation_location$disaggregation = "location"
early_initiation_income = subset(early_initiation,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
early_initiation_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = early_initiation_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = early_initiation_income
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF -  UNICEF_Expanded_Global_Databases_Infant_Feeding_May2018.xlsx"
  ,sheet=2
  ,rows = c(8:520)
  ,cols=c(2,3,5,24,29,49,54,59,64,69)
  ,na.strings=""
)
exclusive_breastfeeding = dat
names(exclusive_breastfeeding) = c("iso3","country","year","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
exclusive_breastfeeding = melt(exclusive_breastfeeding,id.vars=c("iso3","country","year"))
setnames(exclusive_breastfeeding,"variable","disagg.value")
exclusive_breastfeeding$indicator = "exclusive_breastfeeding"
exclusive_breastfeeding$component = "K"
exclusive_breastfeeding_location = subset(exclusive_breastfeeding,disagg.value %in% c("Urban","Rural"))
exclusive_breastfeeding_location$disaggregation = "location"
exclusive_breastfeeding_income = subset(exclusive_breastfeeding,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
exclusive_breastfeeding_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = exclusive_breastfeeding_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = exclusive_breastfeeding_income
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "IYCF - UNICEF_Expanded_Global_Databases_ISSSF_May2018.xlsx"
  ,sheet=2
  ,rows = c(8:271)
  ,cols=c(2:4,23,28,33,38,43,48,53)
  ,na.strings=""
)
solid_foods = dat
names(solid_foods) = c("iso3","country","survey","Urban","Rural","Poorest","Second poorest","Middle","Second wealthiest","Wealthiest")
solid_foods$year = substr(solid_foods$survey,1,4)
solid_foods$survey = NULL
solid_foods = melt(solid_foods,id.vars=c("iso3","country","year"))
setnames(solid_foods,"variable","disagg.value")
solid_foods$indicator = "solid_foods"
solid_foods$component = "K"
solid_foods_location = subset(solid_foods,disagg.value %in% c("Urban","Rural"))
solid_foods_location$disaggregation = "location"
solid_foods_income = subset(solid_foods,disagg.value %in% c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"))
solid_foods_income$disaggregation = "income"
master_dat_list[[master_dat_index]] = solid_foods_location
master_dat_index = master_dat_index + 1
master_dat_list[[master_dat_index]] = solid_foods_income
master_dat_index = master_dat_index + 1

# OVERVIEW, POLICY, TRACKING, and UNDERLYING XLSX (Starting 32)
dat = read.xlsx(
  "OVERVIEW - childhood stunting, anaemia and overweight in adult women.xlsx"
  ,sheet=2
  ,rows=c(5:146)
  ,cols=c(2:4)
  )
overview = dat
names(overview) = c("country","iso3","burden_text")
overview$burden_text = tolower(overview$burden_text)
overview$count = 1
overview$count[which(grepl("and",overview$burden_text))] = overview$count[which(grepl("and",overview$burden_text))] + 1
overview$count[which(grepl(",",overview$burden_text))] = overview$count[which(grepl(",",overview$burden_text))] + 1
country_classes = c(
  "a country experiencing one form of malnutrition",
  "a country experiencing two forms of malnutrition",
  "a country experiencing three forms of malnutrition"
)
overview$country_class = country_classes[overview$count]
overview$count = NULL
overview = melt(overview,id.vars=c("country","iso3"),variable.name="indicator")
overview$component = "A"
overview$disaggregation = "all"
master_dat_list[[master_dat_index]] = overview
master_dat_index = master_dat_index + 1

dat = read.xlsx("POLICY - FBDG.xlsx")
dat = subset(dat,!is.na(X1))
fbdg = dat
names(fbdg) = c("iso3","country","value")
fbdg$indicator = "fbdg"
fbdg$component = "A"
fbdg$disaggregation = "all"
master_dat_list[[master_dat_index]] = fbdg
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "POLICY - GINA.xlsx"
  ,rows = c(2:196)
  ,cols = c(2:11,14)
  )
plans = dat
names(plans) = c(
"country",
"iso3",
"multi_sec",
"stunting_plan",
"anaemia_plan",
"LBW_plan",
"child_overweight_plan",
"EBF_plan",
"wasting_plan",
"sodium_plan",
"overweight_adults_adoles_plan"
)
plans = melt(plans,id.vars=c("country","iso3"),variable.name="indicator")
plans$value = sapply(plans$value,firstup)
plans$component = "Q"
plans$disaggregation = "all"
master_dat_list[[master_dat_index]] = plans
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "POLICY - salt.xlsx"
  ,rows=c(3:199)
  ,cols=c(1:3)
)
salt_leg = dat
names(salt_leg) = c("iso3","country","value")
salt_leg_isos = salt_leg[c("iso3","country")]
salt_leg$indicator = "salt_leg"
salt_leg$component = "O"
salt_leg$disaggregation = "all"
salt_leg$value[which(salt_leg$value=="MANDATORY")] = "Yes"
salt_leg$value[which(salt_leg$value=="NO")] = "No"
salt_leg$value[which(salt_leg$value=="UNKNOWN")] = NA
master_dat_list[[master_dat_index]] = salt_leg
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "POLICY - SSB.xlsx"
  ,rows=c(8:46)
  ,cols=c(2:4)
)
sugar_tax = dat
names(sugar_tax) = c("country","iso3","value")
salt_leg_isos = subset(salt_leg_isos,!(iso3 %in% sugar_tax$iso3))
sugar_tax = merge(sugar_tax,salt_leg_isos,all=T)
sugar_tax$value[which(is.na(sugar_tax$value))] = "No"
sugar_tax$indicator = "sugar_tax"
sugar_tax$component = "O"
sugar_tax$disaggregation = "all"
sugar_tax$value = sapply(sugar_tax$value,firstup)
master_dat_list[[master_dat_index]] = sugar_tax
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "TRACKING - EBF monitoring rules May 2018 update.xlsx",
  sheet=2,
  rows=c(3:136),
  cols=c(1:5)
)
ebf_track = dat
names(ebf_track) = c(
  "country",
  "No data",
  "No progress or worsening",
  "On course",
  "Some progress"
)
ebf_track = melt(ebf_track,id.vars="country")
ebf_track = subset(ebf_track,!is.na(value))
ebf_track$value = NULL
setnames(ebf_track,"variable","value")
ebf_track$indicator = "ebf_track"
ebf_track$disaggregation = "all"
ebf_track$component = "A"
master_dat_list[[master_dat_index]] = ebf_track
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "TRACKING - Lancet_2016_Diabetes_projection_2025_by_Country_Region_World.xlsx",
  sheet=2,
  rows = c(2:206),
  cols = c(1,2,12)
)
adult_fem_diabetes_track = dat
names(adult_fem_diabetes_track) = c("iso3","country","value")
adult_fem_diabetes_track$indicator = "adult_fem_diabetes_track"
adult_fem_diabetes_track$disaggregation = "all"
adult_fem_diabetes_track$component = "A"
adult_fem_diabetes_track$value[which(adult_fem_diabetes_track$value=="Off track")] = "No progress or worsening"
adult_fem_diabetes_track$value[which(adult_fem_diabetes_track$value=="On track")] = "On course"
master_dat_list[[master_dat_index]] = adult_fem_diabetes_track
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "TRACKING - Lancet_2016_Diabetes_projection_2025_by_Country_Region_World.xlsx",
  sheet=3,
  rows = c(2:206),
  cols = c(1,2,12)
)
adult_mal_diabetes_track = dat
names(adult_mal_diabetes_track) = c("iso3","country","value")
adult_mal_diabetes_track$indicator = "adult_mal_diabetes_track"
adult_mal_diabetes_track$disaggregation = "all"
adult_mal_diabetes_track$component = "A"
adult_mal_diabetes_track$value[which(adult_mal_diabetes_track$value=="Off track")] = "No progress or worsening"
adult_mal_diabetes_track$value[which(adult_mal_diabetes_track$value=="On track")] = "On course"
master_dat_list[[master_dat_index]] = adult_mal_diabetes_track
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "TRACKING - obesity.xlsx"
  ,rows=c(2:196)
  ,cols=c(1,2,4,6)
  )
obesity_tracks = dat
names(obesity_tracks) = c("iso3","country","adult_fem_obesity_track","adult_mal_obesity_track")
obesity_tracks = melt(obesity_tracks,id.vars=c("iso3","country"),variable.name="indicator")
obesity_tracks$value[which(obesity_tracks$value=="Off track")] = "No progress or worsening"
obesity_tracks$value[which(obesity_tracks$value=="On track")] = "On course"
obesity_tracks$disaggregation = "all"
obesity_tracks$component = "A"
master_dat_list[[master_dat_index]] = obesity_tracks
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "TRACKING - Tracking Anaemia monitoring rules JME 2017 edition.xlsx",
  sheet=2,
  rows=c(3:192),
  cols=c(1:3)
)
wra_anaemia_track = dat
names(wra_anaemia_track) = c(
  "country",
  "No progress or worsening",
  "Some progress"
)
wra_anaemia_track = melt(wra_anaemia_track,id.vars="country")
wra_anaemia_track = subset(wra_anaemia_track,!is.na(value))
wra_anaemia_track$value = NULL
setnames(wra_anaemia_track,"variable","value")
wra_anaemia_track$indicator = "wra_anaemia_track"
wra_anaemia_track$disaggregation = "all"
wra_anaemia_track$component = "A"
master_dat_list[[master_dat_index]] = wra_anaemia_track
master_dat_index = master_dat_index + 1


dat = read.xlsx(
  "TRACKING - Tracking Overweight monitoring rules JME 2018 edition.xlsx",
  sheet=2,
  rows=c(3:153),
  cols=c(1:4)
)
under_5_overweight_track = dat
names(under_5_overweight_track) = c(
  "country",
  "No progress or worsening",
  "On course",
  "No data"
)
under_5_overweight_track = melt(under_5_overweight_track,id.vars="country")
under_5_overweight_track = subset(under_5_overweight_track,!is.na(value))
under_5_overweight_track$value = NULL
setnames(under_5_overweight_track,"variable","value")
under_5_overweight_track$indicator = "under_5_overweight_track"
under_5_overweight_track$disaggregation = "all"
under_5_overweight_track$component = "A"
master_dat_list[[master_dat_index]] = under_5_overweight_track
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "TRACKING - Tracking Stunting monitoring rules JME 2018 edition.xlsx",
  sheet=2,
  rows=c(3:153),
  cols=c(1:5)
)
under_5_stunting_track = dat
names(under_5_stunting_track) = c(
  "iso3",
  "No progress or worsening",
  "On course",
  "Some progress",
  "No data"
)
under_5_stunting_track = melt(under_5_stunting_track,id.vars="iso3")
under_5_stunting_track = subset(under_5_stunting_track,!is.na(value))
under_5_stunting_track$value = NULL
setnames(under_5_stunting_track,"variable","value")
under_5_stunting_track$indicator = "under_5_stunting_track"
under_5_stunting_track$disaggregation = "all"
under_5_stunting_track$component = "A"
master_dat_list[[master_dat_index]] = under_5_stunting_track
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "TRACKING - Tracking Wasting monitoring rules JME 2018 edition.xlsx",
  sheet=2,
  rows=c(3:153),
  cols=c(1:5)
)
under_5_wasting_track = dat
names(under_5_wasting_track) = c(
  "country",
  "No progress or worsening",
  "On course",
  "Some progress",
  "No data"
)
under_5_wasting_track = melt(under_5_wasting_track,id.vars="country")
under_5_wasting_track = subset(under_5_wasting_track,!is.na(value))
under_5_wasting_track$value = NULL
setnames(under_5_wasting_track,"variable","value")
under_5_wasting_track$indicator = "under_5_wasting_track"
under_5_wasting_track$disaggregation = "all"
under_5_wasting_track$component = "A"
master_dat_list[[master_dat_index]] = under_5_wasting_track
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Births by age 18.xlsx"
  ,rows=c(8:331)
  ,cols=c(1:4)
)
early_childbearing_prev = dat
names(early_childbearing_prev) = c("iso3","country","survey","value")
early_childbearing_prev$indicator = "early_childbearing_prev"
early_childbearing_prev$year = substr(early_childbearing_prev$survey,nchar(early_childbearing_prev$survey)-3,nchar(early_childbearing_prev$survey))
early_childbearing_prev$survey = NULL
early_childbearing_prev$disaggregation = "all"
early_childbearing_prev$component = "U"
early_childbearing_prev = data.table(early_childbearing_prev)
early_childbearing_prev = early_childbearing_prev[,.SD[which.max(.SD$year)],by=.(country)]
master_dat_list[[master_dat_index]] = early_childbearing_prev
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Community health workers.xlsx",
  rows=c(4:268),
  na.strings=c("","No data")
)
community_health_workers = dat[c("Country.Name","Country.Code","latest.year","latest.value")]
names(community_health_workers) = c("country","iso3","year","value")
community_health_workers = subset(community_health_workers,year!="No data")
community_health_workers$indicator = "community_health_workers"
community_health_workers$disaggregation = "all"
community_health_workers$component = "U"
master_dat_list[[master_dat_index]] = community_health_workers
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Drinking water.xlsx"
  ,rows=c(7:471)
  ,cols=c(1:7,20)
  ,na.strings="-"
)
drinking_water = dat
names(drinking_water) = c(
  "iso3",
  "country",
  "year",
  "basic_water",
  "limited_water",
  "unimproved_water",
  "surface_water",
  "safely_managed_water"
)
drinking_water$sum = rowSums(drinking_water[,c("basic_water",
                                               "limited_water",
                                               "unimproved_water",
                                               "surface_water",
                                               "safely_managed_water")],na.rm=T)
drinking_water$basic_water[which(drinking_water$sum>100)] = drinking_water$basic_water[which(drinking_water$sum>100)] - drinking_water$safely_managed_water[which(drinking_water$sum>100)]
drinking_water$sum = NULL
drinking_water = melt(drinking_water,id.vars=c("iso3","country","year"),variable.name="indicator")
drinking_water$disaggregation = "all"
drinking_water$component = "V"
master_dat_list[[master_dat_index]] = drinking_water
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Drinking water.xlsx"
  ,sheet=2
  ,rows=c(8:472)
  ,cols=c(1:7,23)
  ,na.strings="-"
)
sanitation = dat
names(sanitation) = c(
  "iso3",
  "country",
  "year",
  "basic_sanitation",
  "limited_sanitation",
  "unimproved_sanitation",
  "open_defecation",
  "safely_managed_sanitation"
)
sanitation$sum = rowSums(sanitation[,c("basic_sanitation",
                                               "limited_sanitation",
                                               "unimproved_sanitation",
                                               "open_defecation",
                                               "safely_managed_sanitation")],na.rm=T)
sanitation$basic_sanitation[which(sanitation$sum>100)] = sanitation$basic_sanitation[which(sanitation$sum>100)] - sanitation$safely_managed_sanitation[which(sanitation$sum>100)]
sanitation$sum = NULL
sanitation = melt(sanitation,id.vars=c("iso3","country","year"),variable.name="indicator")
sanitation$disaggregation = "all"
sanitation$component = "V"
master_dat_list[[master_dat_index]] = sanitation
master_dat_index = master_dat_index + 1

# Replaced by csv
# dat = read.xlsx(
#   "UNDERLYING_Female enrollment rate, secondary (%), net.xlsx",
#   rows=c(4:285),
#   na.strings=".."
# )
# female_secondary_enroll_net = dat
# names(female_secondary_enroll_net)[1] = "country"
# female_secondary_enroll_net = melt(female_secondary_enroll_net,id.vars="country",variable.name="year")
# female_secondary_enroll_net = subset(female_secondary_enroll_net,!is.na(value))
# female_secondary_enroll_net = data.table(female_secondary_enroll_net)
# female_secondary_enroll_net = female_secondary_enroll_net[,.SD[which.max(.SD$year)],by=.(country)]
# female_secondary_enroll_net$indicator = "female_secondary_enroll_net"
# female_secondary_enroll_net$disaggregation = "all"
# female_secondary_enroll_net$component = "U"
# master_dat_list[[master_dat_index]] = female_secondary_enroll_net
# master_dat_index = master_dat_index + 1

# dat = read.xlsx(
#   "UNDERLYING_Food stuffs.xlsx",
#   sheet=2
# )
# write.csv(dat,"UNDERLYING_Food stuffs.csv",na="",row.names=F)
dat = read.csv("UNDERLYING_Food stuffs.csv",na.strings="",as.is=T)
dat = subset(dat,Item %in% c("Fruits - Excluding Wine","Vegetables"))
dat = subset(dat,Unit=="g/capita/day")
dat = subset(dat,Element=="Food supply quantity (g/capita/day)")
fruit_veg = dat[c("Area",paste0("Y",1961:2013))]
fruit_veg = melt(fruit_veg,id.vars="Area",variable.name="year")
fruit_veg$year = substr(fruit_veg$year,2,5)
names(fruit_veg)[1] = "country"
fruit_veg$country[which(fruit_veg$country=="Sudan (former)")] = "Sudan"
fruit_veg = data.table(fruit_veg)[,.(value=sum(value,na.rm=T)),by=.(country,year)]
fruit_veg$value[which(fruit_veg$value==0)] = NA
fruit_veg$indicator = "fruit_veg_availability"
fruit_veg$disaggregation = "all"
fruit_veg$component = "S"
master_dat_list[[master_dat_index]] = fruit_veg
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_GII.xlsx",
  rows=c(2:162),
  cols=c(2,15:17)
)
gender_inequality = dat
names(gender_inequality) = c("country","year","gender_inequality_score","gender_inequality_rank")
gender_inequality = melt(gender_inequality,id.vars=c("country","year"),variable.name="indicator")
gender_inequality$disaggregation = "all"
gender_inequality$component = "U"
master_dat_list[[master_dat_index]] = gender_inequality
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Non-staples.xlsx"
  ,rows=c(3:205)
  ,cols=c(1,2,5:9)
)
non_staples = dat
names(non_staples) = c(
  "iso3",
  "country",
  "2000",
  "2003",
  "2006",
  "2009",
  "2012"
)
non_staples = melt(non_staples,id.vars=c("iso3","country"),variable.name="year")
non_staples$indicator = "total_calories_non_staple"
non_staples$disaggregation = "all"
non_staples$component = "S"
master_dat_list[[master_dat_index]] = non_staples
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Nurses and Midwives.xlsx",
  rows=c(4:268),
  cols=c(1,2,33,34)
  )
nurses_and_midwives = dat
names(nurses_and_midwives) = c("country","iso3","year","value")
nurses_and_midwives = subset(nurses_and_midwives,value!="No data")
nurses_and_midwives$indicator = "nurses_and_midwives"
nurses_and_midwives$disaggregation = "all"
nurses_and_midwives$component = "U"
master_dat_list[[master_dat_index]] = nurses_and_midwives
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Physicians per 1000 people.xlsx",
  rows = c(4:268)
  ,cols = c(1,2,63,64)
)
physicians = dat
names(physicians) = c("country","iso3","year","value")
physicians = subset(physicians,value!="No data")
physicians$indicator = "physicians"
physicians$disaggregation = "all"
physicians$component = "U"
master_dat_list[[master_dat_index]] = physicians
master_dat_index = master_dat_index + 1

dat = read.xlsx(
  "UNDERLYING_Undernourishment.xlsx"
  ,rows=c(4:252)
  ,cols=c(1,2,21,25,29,33,37)
)
undernourishment_prev = dat
names(undernourishment_prev) = c(
  "iso3","country","2000","2004","2008","2012","2016"
)
undernourishment_prev = melt(undernourishment_prev,id.vars=c("iso3","country"),variable.name="year")
undernourishment_prev$indicator = "undernourishment_prev"
undernourishment_prev$disaggregation = "all"
undernourishment_prev$component = "S"
undernourishment_prev = subset(undernourishment_prev,country!="Micronesia")
master_dat_list[[master_dat_index]] = undernourishment_prev
master_dat_index = master_dat_index + 1

for(i in 1:length(master_dat_list)){
  df = master_dat_list[[i]]
  if(is.factor(df$country)){
    master_dat_list[[i]]$country = unfactor(master_dat_list[[i]]$country)
  }
}

master_dat = rbindlist(master_dat_list,fill=T)
master_dat$country = trimws(master_dat$country)

region_key = read.xlsx("Regional dataset - key.xlsx")
isos = region_key[c("ISO-alpha3.Code","Country.or.Area")]
names(isos) = c("iso3","key.country")
isos = subset(isos,!is.na(iso3))
master_dat = merge(master_dat,isos,by="iso3",all.x=T)
master_dat$country[which(!is.na(master_dat$key.country))] = master_dat$key.country[which(!is.na(master_dat$key.country))]
master_dat$key.country = NULL

names(isos) = c("key.iso3","country")
master_dat = merge(master_dat,isos,by="country",all.x=T)
master_dat$iso3[which(!is.na(master_dat$key.iso3))] = master_dat$key.iso3[which(!is.na(master_dat$key.iso3))]
master_dat$key.iso3 = NULL

# write.csv(unique(master_dat[,c("iso3","country")]),"master_countries.csv",na="",row.names=F)
master_countries = read.csv("master_countries.csv",na.strings="")
master_countries = subset(master_countries,!is.na(iso3))
master_countries = unique(master_countries)

names(master_countries) = c("key.iso3","country")
master_dat = merge(master_dat,master_countries,by="country")
master_dat$iso3 = master_dat$key.iso3
master_dat$key.iso3 = NULL

names(isos) = c("iso3","key.country")
master_dat = merge(master_dat,isos,by="iso3")
master_dat$country = master_dat$key.country
master_dat$key.country = NULL

regions = region_key[c("ISO-alpha3.Code","GNR1","GNR2")]
names(regions) = c("iso3","region","subregion")
master_dat = merge(master_dat,regions,all.x=T)

master_dat$year = unfactor(master_dat$year)
master_dat = subset(master_dat,year>=1999 | is.na(year))

backup = read.csv("../data.backup.csv",na.strings="",as.is=T)
diff = setdiff(backup$indicator,master_dat$indicator)
depr = c("calcium","eggs","fish ","fruit ",                       
         "legumes","milk","nuts","omega 3","poultry",                      
         "processed meat","read meat","refined grains ","sodium ","SSD",                          
         "starchy veg","total daity ","total sugar ","veg","whole grains",                 
         "polyunsaturated fats","sat fat","transfat","whole_grains","vegetables",                   
         "omega_3","poly_unsat_fat","nuts_seeds","fruit","fibre",                        
         "trans_fatty_acids","SSB","sodium","red meat","processed_meat","310_percent",
         "adult_anaemia","continued_breastfeeding","urban_percent","ODA_nutspecific_perceent"
         ,"gini_year","specific_nutrition_plan","blood_pressure_plan","diabetes_plan")
diff[which(!(diff %in% depr))]
write.csv(master_dat,"../data.csv",na="",row.names=F)

master_dat_melt = melt(master_dat[,c("iso3","country","year","indicator","disaggregation","disagg.value","value")],id.vars=c("iso3","country","year","indicator","disaggregation","disagg.value"))
# return_one = function(vec){return(vec[1])}
master_dat_wide = dcast(master_dat_melt,iso3+country+year~indicator+disaggregation+disagg.value+variable,sep="."
                        # ,fun=return_one
                        )
# master_dat_unwide = melt(master_dat_wide,id.vars=c("iso3","country","year"))
# master_dat_unwide = subset(master_dat_unwide,value>1)
# write.csv(master_dat_unwide,"../dups.csv")
write.csv(master_dat_wide,"../data_wide.csv",na="",row.names=T)

