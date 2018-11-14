####Setup#####
list.of.packages <- c("reshape2","data.table","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018/Dataset working directory"
setwd(wd)

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
adult_anemia_fem$disaggregation = "gender"
adult_anemia_fem$disagg.value = "Female"
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
  "Sugar-sweeted beverages"
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
#TODO: Check this is the filter we want
dat = subset(dat,age_group_id==22)
dat = subset(dat,sex_id==3)
dietary_needs = dat[c("location_name","year_id","mean","modelable_entity_name")]
dietary_needs = merge(dietary_needs,unit.dat,by="modelable_entity_name")
dietary_needs$modelable_entity_name = NULL
names(dietary_needs) = c("country","year","value","indicator","unit","rec")
dietary_needs$disaggregation = "location"
dietary_needs$disagg.value = "National"
dietary_needs$component = "M"
master_dat_list[[master_dat_index]] = dietary_needs
master_dat_index = master_dat_index + 1

dat = read.csv("DIETARY_NEEDS_2016_regional.csv",na.strings="",as.is=T)
#TODO: Check this is the filter we want
dat = subset(dat,age_group_name=="all ages 25+")
dat = subset(dat,sex_id==3)
dietary_needs_reg = dat[c("region_name","year_id","mean","modelable_entity_name")]
dietary_needs_reg = merge(dietary_needs_reg,unit.dat,by="modelable_entity_name")
dietary_needs_reg$modelable_entity_name = NULL
names(dietary_needs_reg) = c("region","year","value","indicator","unit","rec")
dietary_needs_reg$disaggregation = "location"
dietary_needs_reg$disagg.value = "Regional"
dietary_needs_reg$component = "M"
diet_mapping = read.csv("dietary_mapping.csv",na.strings="")
dietary_needs_reg = join(diet_mapping,dietary_needs_reg,by="region")
dietary_needs_reg$region = NULL
master_dat_list[[master_dat_index]] = dietary_needs_reg
master_dat_index = master_dat_index + 1

master_dat = rbindlist(master_dat_list,fill=T)

