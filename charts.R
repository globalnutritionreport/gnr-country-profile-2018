####Setup#####
library(ggplot2)
library(reshape2)
library(data.table)
library(scales)
library(varhandle)
library(Cairo)
library(plyr)

#Needed for UTF-8
# Sys.setlocale("LC_CTYPE","russian")
Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

wd <- "C:/git/alexm-util/DevInit/GNR/2017"
setwd(wd)

dat <- read.csv("data.csv",na.strings=c("","."," "),as.is=TRUE)
dist_data <- read.csv("dist_data.csv",na.strings=c("","."),as.is=TRUE)
setnames(dist_data,"quin1","Poorest   ")
setnames(dist_data,"quin2","\nSecond   \npoorest")
setnames(dist_data,"quin3","Middle   ")
setnames(dist_data,"quin4","\nSecond   \nwealthiest")
setnames(dist_data,"quin5","Wealthiest   ")
dist_data <- melt(dist_data,measure.vars=c("Poorest   ","\nSecond   \npoorest","Middle   ","\nSecond   \nwealthiest","Wealthiest   "))
dist_data$value <- dist_data$value*100

countries <- unique(dat$country)

wd <- "C:/Users/Alex/Documents/Data/GNR/Country profiles"
setwd(wd)

unlink(
  dir(wd, full.names = TRUE)
  , recursive = TRUE
)
blank <- data.frame(x=0,y=0,text="No data")
no.data <- ggplot(blank,aes(x,y,label=text)) +
  geom_text(size=20,color="grey") +
  theme(
    axis.line = element_blank()
    ,axis.text = element_blank()
    ,axis.ticks = element_blank()
    ,axis.title = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )
blank2 <- data.frame(x=0,y=0,text=" ")
cblank <- ggplot(blank2,aes(x,y,label=text)) +
  geom_text(size=20) +
  theme(
    axis.line = element_blank()
    ,axis.text = element_blank()
    ,axis.ticks = element_blank()
    ,axis.title = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
  )

simple_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.line = element_line(colour = "black"))

yellow <- "#bfc200"
orange <- "#de5d09"
purple <- "#71105f"
blue <- "#93cac9"
grey <- "#a0adbb"
white <- "#ffffff"
black <- "#443e42"

yellowOrangeFill <- scale_fill_manual(values=c(yellow,orange))
yellowOrangePurpleFill <- scale_fill_manual(values=c(yellow,orange,purple))
purpleFill <- scale_fill_manual(values=c(purple))
orangeFill <- scale_fill_manual(values=c(orange))
blueFill <- scale_fill_manual(values=c(blue))
quintileFill <-  scale_fill_manual(values=c(grey,yellow,purple,blue,orange))
purpleOrangeBlueFill <-  scale_fill_manual(values=c(purple,orange,blue))

quintileFillValues <- c(grey,yellow,purple,blue,orange)

yellowOrangeColor <- scale_color_manual(values=c(yellow,orange))
purpleColor <- scale_color_manual(values=c(purple))
orangeColor <- scale_color_manual(values=c(orange))
blueColor <- scale_color_manual(values=c(blue))
quintileColor <-  scale_color_manual(values=c(orange,blue,purple,yellow,grey))
purpleOrangeBlueColor <-  scale_color_manual(values=c(purple,orange,blue))

textQuintileOffset <- scale_color_manual(values=c(black,black,white,black,black))

safeFormat <- function(vec){
  results <- c()
  for(x in vec){
    #Missing
    if(is.na(x)){
      result <- ""
      #Large Negative
    }else if(x<= -1000){
      result <- format(round(x, digits = 0),format="d",big.mark=",")
      #Middle Negative
    }else if(x< -1){
      result <- round(x,digits=0)
      #Small negative
    }else if(x<0){
      result <- round(x,digits=1)
      #Zero
    }else if(x==0){
      result <- "0"
      #Small positive
    }else if(x<1){
      result <- round(x,digits=1)
      #Middle positive
    }else if(x<1000){
      result <- round(x,digits=0)
      #Large positive
    }else{
      result <- format(round(x, digits = 0),format="d",big.mark=",")
    }
    results <- c(results,result)
  }
  return(results)
}

c1values <- list(
  "$1.90/day (% pop)"=list(
  "poverty_190_1"="yearpov190_1"
  ,"poverty190_2"="yearpov190_2"
  ),
  "$3.10/day (% pop)"=list("poverty310_1"="yearpov310_1"
  ,"poverty310_2"="yearpov310_2"
  ),
  "PPP($) GDP per capita"=list("gdp_1990"="gdp_1990_year"
  ,"gdp_2000"="gdp_2000_year"
  ,"gdp_2010"="gdp_2010_year"
  ,"gdp_2016"="gdp_2016_year"
  )
)
dat$gdp_1990_year <- 1990
dat$gdp_2000_year <- 2000
dat$gdp_2010_year <- 2010
dat$gdp_2016_year <- 2016

c2values <- c(
  "u5mr2011"
  ,"u5mr2012"
  ,"u5mr2013"
  ,"u5mr2014"
  ,"u5mr2015"
)

c3values = list("rate_stuntingtrend1"="year_stuntingtrend1"
    ,"rate_stuntingtrend2"="year_stuntingtrend2"
    ,"rate_stuntingtrend3"="year_stuntingtrend3"
    ,"rate_stuntingtrend4"="year_stuntingtrend4"
    ,"rate_stuntingtrend5"="year_stuntingtrend5"
)

c4values <- NA

c5values <- list(
  "Raised blood pressure (%), 2015"=list(
    "BPmale" = "Men"
    ,"BPfemale" = "Women"
    # ,"BPboth" = "Both sexes"
  ),
  "Raised blood glucose (%), 2014"=list(
    "BGmale" = "Men"
    ,"BGfemale" = "Women"
    # ,"BGboth" = "Both sexes"
  ),
  "Raised blood cholesterol (%), 2008"=list(
    "cholesterol_MALE" = "Men"
    ,"cholesterol_FEMALE" = "Women"
    # ,"cholesterol_BOTH" = "Both sexes"
  )
)

c6values <- list(
  "Overweight"=list(
    "ow_male" = "Men"
    ,"ow_female" = "Women"
    # ,"ow_bothsexes" = "Both sexes"
  ),
  "Obesity"=list(
    "ob_male" = "Men"
    ,"ob_female" = "Women"
    # ,"ob_bothsexes" = "Both sexes"
  )
)

c7values <- NA

c8values = list("ebf_trend1"="yr_ebf_trend1"
                ,"ebf_trend2"="yr_ebf_trend2"
                ,"ebf_trend3"="yr_ebf_trend3"
)

c9values <- list(
  "Undernourishment\n(% population)"=list(
    "value_undernourishment1990"="year_1990"
    ,"value_undernourishment1999"="year_1999"
    ,"value_undernourishment2009"="year_2009"
    ,"value_undernourishment2014"="year_2014"
  ),
  "Availability of fruit and\nvegetables (grams)"=list(
    "fruitandveg_gram1990"="year_1990"
    ,"fruitandveg_gram2000"="year_2000"
    ,"fruitandveg_gram2010"="year_2010"
    ,"fruitandveg_gram2013"="year_2013"
  ),
  "Available calories\nfrom non-staples (%)"=list(
      "value_nonstaples1990"="year_1990"
       ,"value_nonstaples1999"="year_1999"
       ,"value_nonstaples2008"="year_2008"
       ,"value_nonstaples2011"="year_2011"
  )
)
dat$year_1990 <- 1990
dat$year_1999 <- 1999
dat$year_2000 <- 2000
dat$year_2008 <- 2008
dat$year_2009 <- 2009
dat$year_2010 <- 2010
dat$year_2011 <- 2011
dat$year_2013 <- 2013
dat$year_2014 <- 2014

c10values = list(
  "rate_femaleED_1"="year_femaleED_1"
  ,"rate_femaleED_2"="year_femaleED_2"
  ,"rate_femaleED_3"="year_femaleED_3"
  ,"rate_femaleED_4"="year_femaleED_4"
  ,"rate_femaleED_5"="year_femaleED_5"
)

c11values <- list(
  "Surface water "=list(
    "water_surface2000" = "2000"
    ,"water_surface2010" = "2010"
    ,"water_surface2015" = "2015"
  ),
  "Unimproved"=list(
    "water_unimproved2000" = "2000"
    ,"water_unimproved2010" = "2010"
    ,"water_unimproved2015" = "2015"
  ),
  "Limited"=list(
    "water_limited2000" = "2000"
    ,"water_limited2010" = "2010"
    ,"water_limited2015" = "2015"
  ),
  "Basic"=list(
    "water_basic2000" = "2000"
    ,"water_basic2010" = "2010"
    ,"water_basic2015" = "2015"
  ),
  "Safely managed"=list(
    "water_safelymanaged2000" = "2000"
    ,"water_safelymanaged2010" = "2010"
    ,"water_safelymanaged2015" = "2015"
  )
)

c12values <- list(
  "Open defecation"=list(
    "san_open2000" = "2000"
    ,"san_open2010" = "2010"
    ,"san_open2015" = "2015"
  ),
  "Unimproved"=list(
    "san_unimproved2000" = "2000"
    ,"san_unimproved2010" = "2010"
    ,"san_unimproved2015" = "2015"
  ),
  "Limited"=list(
    "san_limited2000" = "2000"
    ,"san_limited2010" = "2010"
    ,"san_limited2015" = "2015"
  ),
  "Basic"=list(
    "san_basic2000" = "2000"
    ,"san_basic2010" = "2010"
    ,"san_basic2015" = "2015"
  ),
  "Safely managed "=list(
    "san_safelymanaged2000" = "2000"
    ,"san_safelymanaged2010" = "2010"
    ,"san_safelymanaged2015" = "2015"
  )
)

c13values <- list(
  "Agriculture "=list(
    "totag_ppp1990" = "1990"
    ,"totag_ppp2000" = "2000"
    ,"totag_ppp2010" = "2010"
    ,"totag_ppp2012" = "2012"
  ),
  "Education"=list(
    "toteducation_ppp1990" = "1990"
    ,"toteducation_ppp2000" = "2000"
    ,"toteducation_ppp2010" = "2010"
    ,"toteducation_ppp2012" = "2012"
  ),
  "Health"=list(
    "tothealth_ppp1990" = "1990"
    ,"tothealth_ppp2000" = "2000"
    ,"tothealth_ppp2010" = "2010"
    ,"tothealth_ppp2012" = "2012"
  ),
  "Social protection"=list(
    "totsp_ppp1990" = "1990"
    ,"totsp_ppp2000" = "2000"
    ,"totsp_ppp2010" = "2010"
    ,"totsp_ppp2012" = "2012"
  )
)

####End setup####
####Loop####
# countries <- c("India","Japan","Mozambique","The former Yugoslav Republic of Macedonia")
# countries <- c("Mozambique")
# countries <- c("Burundi","Djibouti","Sudan")
for(this.country in countries){
  message(this.country)
  dir.create(paste(wd,this.country,sep="/"))
  setwd(paste(wd,this.country,sep="/"))
  countrydat <- subset(dat,country==this.country)
  #Chart 1 part a and b
  c1list <- list()
  c1index <- 1
  for(indicator in names(c1values)){
    # message(indicator)
    value.names <- names(c1values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    year <- sapply(countrydat[1,sapply(c1values[[indicator]],'[[',index=1)],'[[',index=1)
    indicator.df <- data.frame(indicator,year,value)
    c1list[[c1index]] <- indicator.df
    c1index=c1index+1
  }
  c1data <- rbindlist(c1list)
  c1data <- subset(c1data,!is.na(year))
  if(nrow(c1data)!=0){
    c1data <- c1data[order(c1data$year),]
    c1wide <- reshape(c1data,v.names="value",timevar="indicator",idvar="year",direction="wide")
    names(c1wide)[2:length(c1wide)] <- substr(names(c1wide)[2:length(c1wide)],7,nchar(names(c1wide)[2:length(c1wide)]))
    c1.melt <- melt(c1wide,id.vars="year")
    c1.melt$year <- factor(c1.melt$year)
    c1a.melt <- subset(c1.melt,variable %in% c("$1.90/day (% pop)","$3.10/day (% pop)"))
    c1a.melt <- subset(c1a.melt,!is.na(value))
    c1a.max <- max(c1a.melt$value,na.rm=TRUE)
    c1b.melt <- subset(c1.melt,variable == "PPP($) GDP per capita")
    c1b.melt <- subset(c1b.melt,!is.na(value))
    c1b.max <- max(c1b.melt$value,na.rm=TRUE)
    c1a <- ggplot(c1a.melt,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity",color="transparent") +
      yellowOrangeFill +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0),limits=c(0,max(c1a.max*1.1,1))) +
      # expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color="#443e42")
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="#443e42", size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.2,"lines")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
    if(nrow(c1a.melt)==0){c1a.missing<-TRUE}else{c1a.missing<-FALSE}
    c1b <- ggplot(c1b.melt,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity",color="transparent") +
      purpleFill +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c1b.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color="#443e42")
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="#443e42", size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.2,"lines")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
    if(nrow(c1b.melt)==0){c1b.missing<-TRUE}else{c1b.missing<-FALSE}
  }else{
    c1a.missing <- TRUE
    c1b.missing <- TRUE
  }

  #Chart 2
  c2data <- countrydat[c2values]
  c2.melt <- data.frame(melt(c2data))
  if(is.factor(c2.melt$variable)){
    c2.melt$variable <- unfactor(c2.melt$variable)
  }
  c2.melt$year <- as.numeric(substr(c2.melt$variable,nchar(c2.melt$variable)-3,nchar(c2.melt$variable)))
  c2.max <- max(c2.melt$value,na.rm=TRUE)
  c2 <- ggplot(c2.melt,aes(year,value,fill="Deaths per 1,000 live births")) +
    geom_bar(stat="identity",width=0.6,color="transparent") +
    orangeFill +
    guides(fill=guide_legend(title=element_blank())) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(limits = c(2010.7,2015.3)) +
    expand_limits(y=c2.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color="#443e42")
  #Chart 3
  indicator <- "Under-5 stunting"
  value.names <- names(c3values)
  value <- sapply(countrydat[value.names],'[[',index=1)
  year <- sapply(countrydat[1,sapply(c3values,'[[',index=1)],'[[',index=1)
  c3data <- data.frame(indicator,year,value)
  c3data <- subset(c3data,!is.na(year) & !is.na(value))
  c3data <- c3data[order(c3data$year),]
  c3data$year <- factor(c3data$year)
  c3.max <- max(c3data$value,na.rm=TRUE)
  uniqueYears <- length(unique(c3data$year))
  if(uniqueYears>1){
    barWidth = 0.6
  }else{
    barWidth = 0.3
  }
  c3 <- ggplot(c3data,aes(year,value,fill="Blue")) +
    geom_bar(stat="identity",width=barWidth,color="transparent") +
    blueFill +
    guides(fill=FALSE) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c3.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color="#443e42")
  #Chart 4
  country_dist <- subset(dist_data,country==this.country)
  if(nrow(country_dist)==0){
    c4 <- no.data
  }else{
    country_dist$year <- factor(country_dist$year)
    c4 <- ggplot(country_dist,aes(x=value,y=year)) +
      geom_line(aes(group=year),size=1,color="#adb6c0") +
      geom_point(size=7,aes(group=variable,colour=variable)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      quintileColor +
      guides(colour=guide_legend(title=element_blank(),reverse=TRUE)) +
      simple_style  +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color="#443e42")
        ,legend.justification=c(0,0)
        ,legend.direction="horizontal"
        ,axis.title.y=element_blank()
        ,axis.title.x=element_text(color="#443e42",size=20)
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="#443e42", size = 1)
        ,axis.text.y = element_text(size=21,color="#443e42")
        ,axis.text.x = element_text(size=25,color="#443e42")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
      ) +
      xlab("Stunting prevalence (% population)")
  }

  #Chart 5
  c5list <- list()
  c5index <- 1
  for(indicator in names(c5values)){
    # message(indicator)
    value.names <- names(c5values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    sex <- sapply(c5values[[indicator]],'[[',index=1)
    indicator.df <- data.frame(indicator,sex,value)
    c5list[[c5index]] <- indicator.df
    c5index=c5index+1
  }
  c5data <- rbindlist(c5list)
  c5.max <- max(c5data$value,na.rm=TRUE)
  c5data$sex <- factor(c5data$sex,levels=c("Both sexes","Men","Women"))
  c5 <- ggplot(c5data,aes(sex,value,fill=indicator)) +
    geom_bar(position=position_dodge(0.8),stat="identity",width=0.7,color="transparent") +
    purpleOrangeBlueFill +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c5.max*1.2) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=30,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=10,aes(label=safeFormat(value)),position=position_dodge(0.8),vjust=-0.3,color="#443e42")
  #Chart 6
  c6list <- list()
  c6index <- 1
  for(indicator in names(c6values)){
    # message(indicator)
    value.names <- names(c6values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    sex <- sapply(c6values[[indicator]],'[[',index=1)
    indicator.df <- data.frame(indicator,sex,value)
    c6list[[c6index]] <- indicator.df
    c6index=c6index+1
  }
  c6data <- rbindlist(c6list)
  c6.max <- max(c6data$value,na.rm=TRUE)
  c6data$sex <- factor(c6data$sex,levels=c("Women","Men","Both sexes"))
  c6 <- ggplot(c6data,aes(sex,value,fill=indicator)) +
    geom_bar(position=position_dodge(0.8),stat="identity",width=0.8,color="transparent") +
    scale_fill_manual(
      labels=c(expression("Obesity (BMI ">="30)"),expression("Overweight (BMI ">="25)"))
      ,breaks=c("Obesity","Overweight")
      ,values=c(yellow,orange)
      ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c6.max*1.1) +
    theme(
      legend.position="right"
      ,legend.text = element_text(size=35,color="#443e42")
      ,legend.text.align=0
      ,legend.justification=c(1,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.x = element_blank()
      ,axis.line.y = element_line(color="#443e42", size = 1.1)
      ,axis.text.x = element_blank()
      ,axis.text.y = element_text(size=35,color="#443e42",margin=margin(t=0,r=20,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=10,aes(label=safeFormat(value)),position=position_dodge(0.8),hjust=-0.2,color="#443e42") + coord_flip()
  #Chart 7
  c7datalist <- list()
  c7index <- 1
  yr_anc <- countrydat$yr_anc[1]
  yr_sab <- countrydat$year_sab[1]
  yr_earlybf <- countrydat$yr_earlybf[1]
  yr_contbf <- countrydat$yr_contbf[1]
  yr_unmet_need <- countrydat$yr_unmet_need[1]
  c7missing <- 0
  if(!is.null(yr_anc) & !is.na(yr_anc)){
    if(nchar(yr_anc)>4){
      yr.adj <- 9
    }else{
      yr.adj <- 0
    }
    anctext <- paste0("Antenatal care (4+ visits) ",yr_anc)
    ancdat <- data.frame(ypos=c7index,value=countrydat$anc4[1],label=anctext,color=purple,outline=purple,vallab=countrydat$anc4[1],valpos=countrydat$anc4[1],superscript="1",sspos=(51+yr.adj)/100)
    c7datalist[[c7index]] <- ancdat
    c7index <- c7index + 1
  }else{
    anctext <- paste0("Antenatal care (4+ visits)")
    ancdat <- data.frame(ypos=c7index,value=NA,label=anctext,color=purple,outline=purple,vallab="No data",valpos=0,superscript="1",sspos=42/100)
    c7datalist[[c7index]] <- ancdat
    c7index <- c7index + 1
    c7missing <- c7missing + 1
  }
  if(!is.null(yr_sab) & !is.na(yr_sab)){
    if(nchar(yr_sab)>4){
      yr.adj <- 9
    }else{
      yr.adj <- 0
    }
    sabtext <- paste0("Skilled attendant at birth ",yr_sab)
    sabdat <- data.frame(ypos=c7index,value=countrydat$sab[1],label=sabtext,color=purple,outline=purple,vallab=countrydat$sab[1],valpos=countrydat$sab[1],superscript="1",sspos=(50+yr.adj)/100)
    c7datalist[[c7index]] <- sabdat
    c7index <- c7index + 1
  }else{
    sabtext <- paste0("Skilled attendant at birth")
    sabdat <- data.frame(ypos=c7index,value=NA,label=sabtext,color=purple,outline=purple,vallab="No data",valpos=0,superscript="1",sspos=41/100)
    c7datalist[[c7index]] <- sabdat
    c7index <- c7index + 1
    c7missing <- c7missing + 1
  }
  if(!is.null(yr_earlybf) & !is.na(yr_earlybf)){
    if(nchar(yr_earlybf)>4){
      yr.adj <- 8
    }else{
      yr.adj <- 0
    }
    earlybftext <- paste0("Initiation of breastfeeding within 1 hour after birth ",yr_earlybf)
    earlybfdat <- data.frame(ypos=c7index,value=countrydat$earlybf[1],label=earlybftext,color=purple,outline=purple,vallab=countrydat$earlybf[1],valpos=countrydat$earlybf[1],superscript="1",sspos=(91+yr.adj)/100)
    c7datalist[[c7index]] <- earlybfdat
    c7index <- c7index + 1
  }else{
    earlybftext <- paste0("Initiation of breastfeeding within 1 hour after birth")
    earlybfdat <- data.frame(ypos=c7index,value=NA,label=earlybftext,color=purple,outline=purple,vallab="No data",valpos=0,superscript="1",sspos=81/100)
    c7datalist[[c7index]] <- earlybfdat
    c7index <- c7index + 1
    c7missing <- c7missing + 1
  }
  if(!is.null(yr_contbf) & !is.na(yr_contbf)){
    if(nchar(yr_contbf)>4){
      yr.adj <- 9
    }else{
      yr.adj <- 0
    }
    contbftext <- paste0("Continued breastfeeding at 1 year ",yr_contbf)
    contbfdat <- data.frame(ypos=c7index,value=countrydat$contbf[1],label=contbftext,color=purple,outline=purple,vallab=countrydat$contbf[1],valpos=countrydat$contbf[1],superscript="1",sspos=(66+yr.adj)/100)
    c7datalist[[c7index]] <- contbfdat
    c7index <- c7index + 1
  }else{
    contbftext <- paste0("Continued breastfeeding at 1 year")
    contbfdat <- data.frame(ypos=c7index,value=NA,label=contbftext,color=purple,outline=purple,vallab="No data",valpos=0,superscript="1",sspos=57/100)
    c7datalist[[c7index]] <- contbfdat
    c7index <- c7index + 1
    c7missing <- c7missing + 1
  }
  if(!is.null(yr_unmet_need) & !is.na(yr_unmet_need)){
    if(nchar(yr_unmet_need)>4){
      yr.adj <- 9
    }else{
      yr.adj <- 0
    }
    unmet_needtext <- paste0("Unmet need for family planning ",yr_unmet_need)
    unmet_needdat <- data.frame(ypos=c(c7index,c7index),value=c(100-countrydat$unmetneed[1],countrydat$unmetneed[1]),label=c(unmet_needtext,""),color=c("transparent",orange),outline=c(purple,orange),vallab=c("",countrydat$unmetneed[1]),valpos=c(NA,92-countrydat$unmetneed[1]),superscript=c("2",""),sspos=c((62+yr.adj)/100,0))
    c7datalist[[c7index]] <- unmet_needdat
    c7index <- c7index + 1
  }else{
    unmet_needtext <- paste0("Unmet need for family planning")
    unmet_needdat <- data.frame(ypos=c(c7index,c7index),value=c(NA,NA),label=c(unmet_needtext,""),color=c("transparent",orange),outline=c(purple,orange),vallab=c("No data",""),valpos=c(0,NA),superscript=c("2",""),sspos=c(53/100,0))
    c7datalist[[c7index]] <- unmet_needdat
    c7index <- c7index + 1
    c7missing <- c7missing + 1
  }
  c7data <- rbindlist(c7datalist)
  if(nrow(c7data)==0 | c7missing==5){
    c7 = no.data
  }else{
    c7data$ypos <- (max(c7data$ypos)+1)-c7data$ypos
    c7max <- max(c7data$value,na.rm=TRUE)
    ss.adj = max(c7max*1.1,100)
    c7data$sspos = c7data$sspos * ss.adj
    c7yposmax <- max(c7data$ypos)
    ss.y.adj = 0.1/(6-c7yposmax)
    c7 <- ggplot(c7data,aes(y=value,x=ypos,fill=color
                            # ,colour=outline
                            )) +
      geom_bar(stat="identity",width=0.4) +
      scale_fill_identity() +
      # scale_color_identity() +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c7max*1.1) +
      theme(
        legend.position="right"
        ,legend.text = element_text(size=35,color="#443e42")
        ,legend.text.align=0
        ,legend.justification=c(1,0)
        ,legend.direction="horizontal"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.x = element_blank()
        ,axis.line.y = element_line(color="#443e42", size = 1.1)
        ,axis.text.x = element_blank()
        ,axis.text.y = element_blank()
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
      ) + geom_text(size=10,aes(label=vallab,y=valpos),hjust=-0.2,color="#443e42") +
      geom_text(size=10,aes(label=label,y=1,x=ypos+0.25),hjust=0,vjust=0,color="#443e42") +
      geom_text(size=7,aes(label=superscript,y=sspos,x=ypos+0.25+ss.y.adj),hjust=0,vjust=0,color="#443e42") +
      coord_flip()
  }
  #Chart 8
  indicator <- "Exclusive breast-feeding"
  value.names <- names(c8values)
  value <- sapply(countrydat[value.names],'[[',index=1)
  year <- sapply(countrydat[1,sapply(c8values,'[[',index=1)],'[[',index=1)
  c8data <- data.frame(indicator,year,value)
  c8data <- subset(c8data,!is.na(year))
  c8data <- c8data[order(c8data$year),]
  c8data$year <- factor(c8data$year)
  c8.max <- max(c8data$value,na.rm=TRUE)

  uniqueYears <- length(unique(c8data$year))
  if(uniqueYears>1){
    barWidth = 0.6
  }else{
    barWidth = 0.3
  }

  c8 <- ggplot(c8data,aes(year,value,fill="Blue")) +
    geom_bar(stat="identity",width=barWidth,color="transparent") +
    blueFill +
    guides(fill=FALSE) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c8.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color="#443e42")
  #Chart 9 part a and b
  c9list <- list()
  c9index <- 1
  for(indicator in names(c9values)){
    # message(indicator)
    value.names <- names(c9values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    year <- sapply(countrydat[1,sapply(c9values[[indicator]],'[[',index=1)],'[[',index=1)
    indicator.df <- data.frame(indicator,year,value)
    c9list[[c9index]] <- indicator.df
    c9index=c9index+1
  }
  c9data <- rbindlist(c9list)
  c9data <- subset(c9data,!is.na(year))
  if(nrow(c9data)!=0){
    c9data <- c9data[order(c9data$year),]
    c9wide <- reshape(c9data,v.names="value",timevar="indicator",idvar="year",direction="wide")
    names(c9wide)[2:length(c9wide)] <- substr(names(c9wide)[2:length(c9wide)],7,nchar(names(c9wide)[2:length(c9wide)]))
    c9.melt <- melt(c9wide,id.vars="year")
    c9.melt$year <- factor(c9.melt$year)
    c9a.data <- subset(c9.melt,variable==names(c9values)[1])
    c9a.data <- subset(c9a.data,!is.na(value))
    c9b.data <- subset(c9.melt,variable==names(c9values)[2])
    c9b.data <- subset(c9b.data,!is.na(value))
    c9c.data <- subset(c9.melt,variable==names(c9values)[3])
    c9c.data <- subset(c9c.data,!is.na(value))
    c9a.max <- max(c9a.data$value,na.rm=TRUE)
    c9b.max <- max(c9b.data$value,na.rm=TRUE)
    c9c.max <- max(c9c.data$value,na.rm=TRUE)
    if(nrow(c9a.data)!=0){
      c9a <- ggplot(c9a.data,aes(year,value,fill=variable)) +
        geom_bar(position="dodge",stat="identity",color="transparent",show.legend=FALSE) +
        geom_point(alpha=0.0,shape=22,size=12,color="transparent") +
        scale_fill_manual(
          labels=c(bquote(atop('Undernourishment' ^ 1,'(% population)')))
          ,breaks=c(names(c9values)[1])
          ,values=c(yellow)
        ) +
        guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
        simple_style  +
        scale_y_continuous(expand = c(0,0)) +
        expand_limits(y=c9a.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color="#443e42")
          ,legend.justification=c(0.08,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color="#443e42", size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
      c9a.missing <- FALSE
    }else{
      c9a <- no.data
      c9a.missing <- TRUE
    }
    if(nrow(c9b.data)!=0){
      uniqueYears <- length(unique(c9b.data$year))
      if(uniqueYears>1){
        barWidth = 0.9
      }else{
        barWidth = 0.5
      }
    c9b <- ggplot(c9b.data,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity",color="transparent",width=barWidth,show.legend=FALSE) +
      geom_point(alpha=0.0,shape=22,size=12,color="transparent") +
      scale_fill_manual(
        labels=c(bquote(atop('Availability of fruit and' ^ 1,'vegetables (grams)')))
        ,breaks=c(names(c9values)[2])
        ,values=c(orange)
      ) +
      guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c9b.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color="#443e42")
        ,legend.justification=c(0.08,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="#443e42", size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
      c9b.missing <- FALSE
    }else{
      c9b <- no.data
      c9b.missing <- TRUE
    }
    if(nrow(c9c.data)!=0){
    c9c <- ggplot(c9c.data,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity",color="transparent",show.legend=FALSE) +
      geom_point(alpha=0.0,shape=22,size=12,color="transparent") +
      scale_fill_manual(
        labels=c(bquote(atop('% of total calories' ^ 2,'from non-staples')))
        ,breaks=c(names(c9values)[3])
        ,values=c(purple)
      ) +
      guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c9c.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color="#443e42")
        ,legend.justification=c(0.08,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color="#443e42", size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3)
      c9c.missing <- FALSE
    }else{
      c9c <- no.data
      c9c.missing <- TRUE
    }
  }else{
    c9a <- no.data
    c9b <- no.data
    c9c <- no.data
    c9a.missing <- TRUE
    c9b.missing <- TRUE
    c9c.missing <- TRUE
  }
  #Chart 10
  indicator <- "Female secondary enrolment"
  value.names <- names(c10values)
  value <- sapply(countrydat[value.names],'[[',index=1)
  year <- sapply(countrydat[1,sapply(c10values,'[[',index=1)],'[[',index=1)
  c10data <- data.frame(indicator,year,value)
  c10data <- subset(c10data,!is.na(year))
  c10data <- c10data[order(c10data$year),]
  c10data$year <- factor(c10data$year)
  c10.max <- max(c10data$value,na.rm=TRUE)
  c10 <- ggplot(c10data,aes(year,value,fill="Blue")) +
    geom_bar(stat="identity",width=0.6,color="transparent") +
    orangeFill +
    guides(fill=FALSE) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c10.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color="#443e42")
  #Chart 11
  c11list <- list()
  c11index <- 1
  for(indicator in names(c11values)){
    # message(indicator)
    value.names <- names(c11values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    year <- sapply(c11values[[indicator]],'[[',index=1)
    indicator.df <- data.frame(indicator,year,value)
    c11list[[c11index]] <- indicator.df
    c11index=c11index+1
  }
  c11data <- rbindlist(c11list)
  c11.max <- max(c11data$value,na.rm=TRUE)
  c11data$year <- factor(c11data$year)
  c11data$indicator <- factor(c11data$indicator)
  c11data <- c11data[complete.cases(c11data$year),]
  c11data <- c11data[order(is.na(c11data$value),c11data$year,desc(c11data$indicator)),]
  c11data <- ddply(c11data, .(year),
                       transform, pos = cumsum(value) - (0.5 * value)
                   ,valid = sum(!is.na(value),na.rm=TRUE))
  c11data <- subset(c11data,valid>=1)
  c11 <- ggplot(c11data,aes(year,value,fill=indicator)) +
    geom_bar(stat="identity",width=0.7,color="transparent") +
    scale_fill_manual(
      labels=names(c11values)
      ,values=quintileFillValues
      ,drop = FALSE
    ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=30,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c11data,value>3),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=names(c11values),values=c(black,black,white,black,black),drop=FALSE)
  #Chart 12
  c12list <- list()
  c12index <- 1
  for(indicator in names(c12values)){
    # message(indicator)
    value.names <- names(c12values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    year <- sapply(c12values[[indicator]],'[[',index=1)
    indicator.df <- data.frame(indicator,year,value)
    c12list[[c12index]] <- indicator.df
    c12index=c12index+1
  }
  c12data <- rbindlist(c12list)
  c12.max <- max(c12data$value,na.rm=TRUE)
  c12data$year <- factor(c12data$year)
  c12data$indicator <- factor(c12data$indicator)
  c12data <- c12data[complete.cases(c12data$year),]
  c12data <- c12data[order(is.na(c12data$value),c12data$year,desc(c12data$indicator)),]
  c12data <- ddply(c12data, .(year),
                   transform, pos = cumsum(value) - (0.5 * value)
                   ,valid = sum(!is.na(value),na.rm=TRUE))
  c12data <- subset(c12data,valid>=1)
  c12 <- ggplot(c12data,aes(year,value,fill=indicator)) +
    geom_bar(stat="identity",width=0.7,color="transparent") +
    scale_fill_manual(
      labels=names(c12values)
      ,values=quintileFillValues
      ,drop = FALSE
    ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=30,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c12data,value>3),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=names(c12values),values=c(black,black,white,black,black),drop=FALSE)
  #Chart 13
  c13list <- list()
  c13index <- 1
  for(indicator in names(c13values)){
    # message(indicator)
    value.names <- names(c13values[[indicator]])
    value <- sapply(countrydat[value.names],'[[',index=1)
    year <- sapply(c13values[[indicator]],'[[',index=1)
    indicator.df <- data.frame(indicator,year,value)
    c13list[[c13index]] <- indicator.df
    c13index=c13index+1
  }
  c13data <- rbindlist(c13list)
  c13.max <- max(c13data$value,na.rm=TRUE)
  c13data$year <- factor(c13data$year)
  c13data$indicator <- factor(c13data$indicator)
  c13data <- c13data[complete.cases(c13data$year),]
  c13data <- c13data[order(is.na(c13data$value),c13data$year,desc(c13data$indicator)),]
  c13data <- ddply(c13data, .(year),
                   transform, pos = cumsum(value) - (0.5 * value)
                   ,valid = sum(!is.na(value),na.rm=TRUE))
  c13data <- subset(c13data,valid>=1)
  if(nrow(c13data)>0){

  uniqueYears <- length(unique(c13data$year))
  if(uniqueYears>1){
    barWidth = 0.7
  }else{
    barWidth = 0.4
  }
  c13 <- ggplot(c13data,aes(year,value,fill=indicator)) +
    geom_bar(stat="identity",width=barWidth,color="transparent") +
    scale_fill_manual(
      labels=names(c13values)
      ,values=quintileFillValues
      ,drop = FALSE
    ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=30,color="#443e42")
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color="#443e42", size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color="#443e42",margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c13data,value>0),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=names(c13values),values=c(black,black,white,black,black),drop=FALSE)
  }else{
    c13 <- no.data
  }
  #Chart 14
  c14data <- data.frame(
    value = c(
      countrydat$SUN_Process1
      ,countrydat$SUN_Process2
      ,countrydat$SUN_Process3
      ,countrydat$SUN_Process4
      ,countrydat$SUN_TotalAccomplished
    )
    ,label = c(
      "Bringing people into a shared space for action"
      ,"Ensuring a coherent policy and legal framework"
      ,"Aligning actions around a common results framework"
      ,"Financial tracking and resource mobilisation"
      ,"Total weighted"
    )
    ,ypos= c(5:1)
    ,color=c(yellow,orange,purple,blue,grey)
  )
  c14data <- c14data[complete.cases(c14data),]
  if(nrow(c14data)==0){
    c14 <- no.data
  }else{
    c14max = 100
    c14 <- ggplot(c14data,aes(y=value,x=ypos)) +
      geom_bar(aes(y=100),stat="identity",width=0.03) +
      geom_bar(aes(fill=color),stat="identity",width=0.1) +
      scale_fill_identity() +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c14max*1.1) +
      theme(
        legend.position="right"
        ,legend.text = element_text(size=35,color="#443e42")
        ,legend.text.align=0
        ,legend.justification=c(1,0)
        ,legend.direction="horizontal"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.x = element_blank()
        ,axis.line.y = element_blank()
        ,axis.text.x = element_blank()
        ,axis.text.y = element_blank()
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
      ) + geom_text(size=10,aes(y=100,x=ypos+0.25,label=safeFormat(value)),hjust=1,vjust=0,color="#443e42") +
      geom_text(size=9,aes(label=label,y=1,x=ypos+0.25),hjust=0,vjust=0,color="#443e42") +
      coord_flip()
  }
  #Have both c1a and c1b
  if(!c1a.missing && !c1b.missing){
    Cairo(file="c1a.png",width=400,height=600,units="px",bg="white")
    tryCatch({print(c1a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c1b.png",width=400,height=600,units="px",bg="white")
    tryCatch({print(c1b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c1.png",width=800,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have only c1a
  if(!c1a.missing && c1b.missing){
    Cairo(file="c1a.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c1b.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c1.png",width=800,height=600,units="px",bg="white")
    tryCatch({print(c1a)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have only c1b
  if(c1a.missing && !c1b.missing){
    Cairo(file="c1a.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c1b.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c1.png",width=800,height=600,units="px",bg="white")
    tryCatch({print(c1b)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have neither c1a or c1b
  if(c1a.missing && c1b.missing){
    Cairo(file="c1a.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c1b.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c1.png",width=800,height=600,units="px",bg="transparent")
    print(no.data)
    dev.off()
  }
  Cairo(file="c2.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c2)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c3.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c3)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c4.png",width=800,height=400,units="px",bg="white")
  tryCatch({print(c4)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c5.png",width=1000,height=400,units="px",bg="white")
  tryCatch({print(c5)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c6.png",width=1200,height=280,units="px",bg="white")
  tryCatch({print(c6)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c7.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c7)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c8.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c8)},error=function(e){message(e);print(no.data)})
  dev.off()
  #Have c9a, c9b, and c9c
  if(!c9a.missing && !c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c9a and c9b only
  if(!c9a.missing && !c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c9a and c9c only
  if(!c9a.missing && c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c9b and c9c only
  if(c9a.missing && !c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #have c9a only
  if(!c9a.missing && c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c9a)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have c9b only
  if(c9a.missing && !c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c9b)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #have c9c only
  if(c9a.missing && c9b.missing && !c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c9c)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have none of c9s
  if(c9a.missing && c9b.missing && c9c.missing){
    Cairo(file="c9a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c9.png",width=900,height=600,units="px",bg="transparent")
    print(no.data)
    dev.off()
  }
  Cairo(file="c10.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c10)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c11.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c11)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c12.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c12)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c13.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c13)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c14.png",width=800,height=500,units="px",bg="white")
  tryCatch({print(c14)},error=function(e){message(e);print(no.data)})
  dev.off()
}
####End loop####
