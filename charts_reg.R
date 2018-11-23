####Setup#####
list.of.packages <- c("ggplot2","reshape2","data.table","scales","varhandle","Cairo","plyr","eulerr","extrafont","ggrepel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018"
setwd(wd)

# font_import("fonts",prompt=F)
CairoFonts(
  regular="Averta Regular",
  bold="Averta Bold"
)

set.seed(12345)

dat <- read.csv("data_reg.csv",na.strings=c("","."," "),as.is=TRUE)

master_dat = read.csv("data.csv",na.strings=c("","."," "),as.is=TRUE)
regions = unique(master_dat[,c("region","subregion")])
subregion_list = c(regions$region,unique(regions$region))
names(subregion_list) = c(regions$subregion,unique(regions$region))

countries <- unique(dat$region)

wd <- "~/git/gnr-country-profile-2018/charts_reg"
setwd(wd)

unlink(
  dir(wd, full.names = TRUE)
  , recursive = TRUE
)
blank <- data.frame(x=0,y=0,text="No data")
no.data <- ggplot(blank,aes(x,y,label=text)) +
  geom_text(size=20,color="grey",family="Averta Regular") +
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
    ,axis.line = element_line(colour = "black")
    ,text = element_text(family="Averta Regular")
    )

yellow <- "#FCC97A" #light orange
orange <- "#F39000" #orange
red <- "#DE5D09" #dark orange
blue <- "#475C6D" #dark grey
light.blue <- "#93CAC9" #aqua
lighter.blue <- "#B2D8D7" #aqua light
dark.grey <- "#A0ADBB" #grey
grey <- "#CFD9E5" #light grey
white <- "#ffffff"

# dark_orange = "#DE5D09"
# orange = "#F39000"
# light_orange = "#FCC97A"
# dark_aqua = "#007495"
# aqua = "#93CAC9"
# aqua_light = "#B2D8D7"
# aqua_extra_light = "#D1E7E5"
# dark_grey = "#475C6D"
# grey = "#A0ADBB"
# light_grey = "#CFD9E5"

quintileFillValues <- c(red, orange, yellow, lighter.blue, light.blue)

lightBlueYellowRed = c(light.blue,yellow,red)

yellowOrangeFill <- scale_fill_manual(values=c(yellow,orange))
orangeYellowFill <- scale_fill_manual(values=c(orange,yellow))
redYellowFill <- scale_fill_manual(values=c(red,yellow))
yellowRedFill <- scale_fill_manual(values=c(yellow,red))
yellowOrangeRedFill <- scale_fill_manual(values=c(yellow,orange,red))
lightBlueYellowRedFill <- scale_fill_manual(values=c(light.blue,yellow,red))
orangeLightBlueFill <- scale_fill_manual(values=c(orange,light.blue))
orangeFill <- scale_fill_manual(values=c(orange))
yellowFill <- scale_fill_manual(values=c(yellow))
blueFill <- scale_fill_manual(values=c(blue))
lightBlueLighterBlueFill  <- scale_fill_manual(values=c(light.blue, lighter.blue))
lightBlueFill <- scale_fill_manual(values=c(light.blue))
lighterBlueFill <- scale_fill_manual(values=c(lighter.blue))
quintileFill <-  scale_fill_manual(values=quintileFillValues)

yellowOrangeColor <- scale_color_manual(values=c(yellow,orange))
orangeYellowColor <- scale_color_manual(values=c(orange,yellow))
redYellowColor <- scale_color_manual(values=c(red,yellow))
yellowRedColor <- scale_color_manual(values=c(yellow,red))
yellowOrangeRedColor <- scale_color_manual(values=c(yellow,orange,red))
lightBlueYellowRedColor <- scale_color_manual(values=c(light.blue,yellow,red))
orangeLightBlueColor <- scale_color_manual(values=c(orange,light.blue))
orangeColor <- scale_color_manual(values=c(orange))
blueColor <- scale_color_manual(values=c(blue))
quintileColor <-  scale_color_manual(values=quintileFillValues)

trmelColor = scale_color_manual(values=c(red,yellow,light.blue))

textQuintileOffset <- scale_color_manual(values=c(white,white,blue,blue,blue))

firstAndLast <- function(vec,year_vec){
  label_df = data.frame(vec,year_vec)
  min.year = min(year_vec,na.rm=T)
  max.year = max(year_vec,na.rm=T)
  label_df$include = NA
  label_df$include[label_df$year_vec==min.year] = 1
  label_df$include[label_df$year_vec==max.year] = 1
  if(is.factor(label_df$vec)){label_df$vec = unfactor(label_df$vec)}
  label_df$vec[which(is.na(label_df$include))] = ""
  return(label_df$vec)
}

round.simple = function(x, digits=0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

round.simple = Vectorize(round.simple)

safeFormat <- function(vec, precision=0, prefix="", suffix=""){
  results <- c()
  for(x in vec){
    #Missing
    if(is.na(x)){
      result <- ""
      
    }else{
      result <- format(round.simple(x, digits = precision),format="d",big.mark=",",nsmall=precision)
    }
    if(result!=""){
      result = paste0(prefix,result,suffix)
    }
    results <- c(results,result)
  }
  return(results)
}

####End setup####
####Loop####
# countries = c("Asia","Africa","Latin America and the Caribbean","Western Asia","Western Europe")
# countries = c("Europe")
for(this.country in countries){
  message(this.country)
  real.country = this.country
  dir.create(paste(wd,this.country,sep="/"))
  setwd(paste(wd,this.country,sep="/"))
  countrydat <- subset(dat,region==this.country)
  if(is.factor(countrydat$year)){
    countrydat$year = unfactor(countrydat$year)
  }
  countrydat$year = as.numeric(countrydat$year)
  regional = countrydat[1,"regional"]
  if(nchar(this.country)>20){
    if(regional==1){
      this.country = "Regional"
    }else{
      this.country = "Subregional"
    }
  }
  max.n = max(countrydat$n,na.rm=T)
  if(max.n>=50){
    minimum.n = 10
  }else if(max.n>=20){
    minimum.n = 5
  }else if(max.n>2){
    minimum.n = 3
  }else{
    minimum.n = 2
  }
  must_sum_to_100s = c("basic_water","limited_water","safely_managed_water","surface_water","unimproved_water"
                       ,"basic_sanitation","limited_sanitation","open_defecation","safely_managed_sanitation","unimproved_sanitation"
  )
  without.n = subset(countrydat,is.na(n) | indicator %in% must_sum_to_100s)
  with.n = subset(countrydat,!is.na(n) & !indicator %in% must_sum_to_100s)
  with.n = subset(with.n,n>=minimum.n)
  countrydat = rbind(without.n,with.n)
  #Chart 1 part a and b
  indicators = c("190_percent","320_percent","GDP_capita_PPP")
  c1data = subset(countrydat,indicator %in% indicators)
  c1data$value = as.numeric(c1data$value)
  c1data <- subset(c1data,!is.na(value))
  if(nrow(c1data)!=0){
    c1a.melt <- subset(c1data, indicator %in% c("190_percent","320_percent"))
    if(nrow(c1a.melt)>0){
      c1a.melt$variable = NA
      c1a.melt$variable[which(c1a.melt$indicator=="190_percent")] = "$1.90/day"
      c1a.melt$variable[which(c1a.melt$indicator=="320_percent")] = "$3.20/day"
      c1a.melt <- subset(c1a.melt,!is.na(value))
      c1a.melt <- c1a.melt[order(c1a.melt$year),]
      c1a.melt$year = as.factor(c1a.melt$year)
      c1a.max <- max(c1a.melt$value,na.rm=TRUE)
      c1a.key.data = data.frame(year=as.numeric(c(NA,NA)),variable=c("$1.90/day","$3.20/day"),value=as.numeric(c(NA,NA)))
      c1a = ggplot(c1a.melt,aes(year,value,fill=variable)) +
        geom_bar(position="dodge",stat="identity",color=blue,show.legend=F,size=1) +
        geom_point(data=c1a.key.data,aes(fill=variable),size=12,color=blue,stroke=1.5,shape=21) +
        orangeYellowFill +
        guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
        simple_style  +
        scale_y_continuous(expand = c(0,0),limits=c(0,max(c1a.max*1.1,1))) +
        # expand_limits(y=c1a.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=35,color=blue,family="Averta Regular")
          ,legend.justification=c(0,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color=blue, size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_blank()
        ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue,family="Averta Regular")
    }
    if(nrow(c1a.melt)==0){c1a.missing<-TRUE}else{c1a.missing<-FALSE}
    c1b.melt <- subset(c1data,indicator == "GDP_capita_PPP")
    if(nrow(c1b.melt)>0){
      c1b.melt$variable = "GDP per capita\n(thousands)"
      c1b.melt <- subset(c1b.melt,!is.na(value))
      c1b.melt <- c1b.melt[order(c1b.melt$year),]
      top4 = c1b.melt$year[c(max(nrow(c1b.melt)-3,1):nrow(c1b.melt))]
      c1b.melt = subset(c1b.melt,year %in% top4)
      c1b.melt$year = as.factor(c1b.melt$year)
      c1b.melt$value = c1b.melt$value/1000
      c1b.max <- max(c1b.melt$value,na.rm=TRUE)
      c1b.key.data = data.frame(year=as.numeric(c(NA)),variable=c("GDP per capita\n(thousands)"),value=as.numeric(c(NA)))
      c1b = ggplot(c1b.melt,aes(year,value,fill=variable)) +
        geom_bar(position="dodge",stat="identity",color=blue,show.legend=F,size=1) +
        geom_point(data=c1b.key.data,aes(fill=variable),size=12,color=blue,stroke=1.5,shape=21) +
        lightBlueFill +
        guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
        simple_style  +
        scale_y_continuous(expand = c(0,0)) +
        expand_limits(y=c1b.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=35,color=blue,family="Averta Regular")
          ,legend.justification=c(0,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color=blue, size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_blank()
          ,legend.key.size = unit(2.2,"lines")
        ) + geom_text(size=9,aes(label=safeFormat(value,precision = 2)),position=position_dodge(1),vjust=-0.3,color=blue,family="Averta Regular")
    }
    
    if(nrow(c1b.melt)==0){c1b.missing<-TRUE}else{c1b.missing<-FALSE}
  }else{
    c1a.missing <- TRUE
    c1b.missing <- TRUE
  }

  #Chart 2
  indicators = c("u5mr")
  c2data = subset(countrydat,indicator %in% indicators)
  c2data$value = as.numeric(c2data$value)
  c2data = subset(c2data, !is.na(value))
  c2data <- c2data[order(c2data$year),]
  top4 = c2data$year[c(max(nrow(c2data)-3,1):nrow(c2data))]
  c2data = subset(c2data,year %in% top4)
  c2data$year = as.factor(c2data$year)
  c2.max <- max(c2data$value,na.rm=TRUE)
  c2 <- ggplot(c2data,aes(year,value,fill="Deaths per 1,000 live births")) +
    geom_bar(stat="identity",width=0.6,color=blue,show.legend=F,size=1) +
    yellowFill +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    # scale_x_continuous(limits = c(2010.7,2015.3)) +
    expand_limits(y=c2.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color=blue,family="Averta Regular")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value,precision=1)),position=position_dodge(1),vjust=-0.3,color=blue,family="Averta Regular")
  #Chart 3 part a and b
  indicators = c("undernourishment_prev","fruit_veg_availability","total_calories_non_staple")
  c3data = subset(countrydat,indicator %in% indicators)
  c3data$value = as.numeric(c3data$value)
  c3data <- subset(c3data,!is.na(year))
  if(nrow(c3data)!=0){
    c3a.data <- subset(c3data,indicator==indicators[1])
    c3a.data$variable = c3a.data$indicator
    c3a.data <- subset(c3a.data,!is.na(value))
    c3a.data <- c3a.data[order(c3a.data$year),]
    c3a.data$year <- factor(c3a.data$year)
    c3b.data <- subset(c3data,indicator==indicators[2])
    c3b.data$variable = c3b.data$indicator
    c3b.data <- subset(c3b.data,!is.na(value))
    c3b.data <- c3b.data[order(c3b.data$year),]
    c3b.years = c(2000,2004,2008,2012,2013)
    c3b.data = subset(c3b.data,year %in% c3b.years)
    c3b.data$year <- factor(c3b.data$year)
    c3c.data <- subset(c3data,indicator==indicators[3])
    c3c.data$variable = c3c.data$indicator
    c3c.data <- subset(c3c.data,!is.na(value))
    c3c.data <- c3c.data[order(c3c.data$year),]
    c3c.data$year <- factor(c3c.data$year)
    c3a.max <- max(c3a.data$value,na.rm=TRUE)
    c3b.max <- max(c3b.data$value,na.rm=TRUE)
    c3c.max <- max(c3c.data$value,na.rm=TRUE)
    if(nrow(c3a.data)!=0){
      c3a.key.data = data.frame(year=as.numeric(c(NA)),variable=c(indicators[1]),value=as.numeric(c(NA)))
      c3a <- ggplot(c3a.data,aes(year,value,fill=variable)) +
        geom_bar(position="dodge",stat="identity",color=blue,show.legend=FALSE,size=1) +
        geom_point(data=c3a.key.data,aes(fill=variable),size=12,color=blue,stroke=1.5,shape=21) +
        scale_fill_manual(
          labels=c(bquote(atop('Undernourishment','(% population)')))
          ,breaks=c(indicators[1])
          ,values=c(yellow)
        ) +
        guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
        simple_style  +
        scale_y_continuous(expand = c(0,0)) +
        expand_limits(y=c3a.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color=blue,family="Averta Regular")
          ,legend.justification=c(0.08,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color=blue, size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color=blue, angle=90, margin=margin(t=20,r=0,b=0,l=0), vjust=0.5,family="Averta Regular")
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue,family="Averta Regular")
      c3a.missing <- FALSE
    }else{
      c3a <- no.data
      c3a.missing <- TRUE
    }
    if(nrow(c3b.data)!=0){
      uniqueYears <- length(unique(c3b.data$year))
      if(uniqueYears>1){
        barWidth = 0.9
      }else{
        barWidth = 0.5
      }
      c3b.key.data = data.frame(year=as.numeric(c(NA)),variable=c(indicators[2]),value=as.numeric(c(NA)))
    c3b <- ggplot(c3b.data,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity",color=blue,width=barWidth,show.legend=FALSE,size=1) +
      geom_point(data=c3b.key.data,aes(fill=variable),size=12,color=blue,stroke=1.5,shape=21) +
      scale_fill_manual(
        labels=c(bquote(atop('Availability of fruit and','vegetables (grams)')))
        ,breaks=c(indicators[2])
        ,values=c(orange)
      ) +
      guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c3b.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color=blue,family="Averta Regular")
        ,legend.justification=c(0.08,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue, angle=90, margin=margin(t=20,r=0,b=0,l=0), vjust=0.5,family="Averta Regular")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue,family="Averta Regular")
      c3b.missing <- FALSE
    }else{
      c3b <- no.data
      c3b.missing <- TRUE
    }
    if(nrow(c3c.data)!=0){
      c3c.key.data = data.frame(year=as.numeric(c(NA)),variable=c(indicators[3]),value=as.numeric(c(NA)))
    c3c <- ggplot(c3c.data,aes(year,value,fill=variable)) +
      geom_bar(position="dodge",stat="identity",color=blue,show.legend=FALSE,size=1) +
      geom_point(data=c3c.key.data,aes(fill=variable),size=12,color=blue,stroke=1.5,shape=21) +
      scale_fill_manual(
        labels=c(bquote(atop('% of total calories','from non-staples')))
        ,breaks=c(indicators[3])
        ,values=c(lighter.blue)
      ) +
      guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c3c.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color=blue,family="Averta Regular")
        ,legend.justification=c(0.08,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue, angle=90, margin=margin(t=20,r=0,b=0,l=0), vjust=0.5,family="Averta Regular")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue,family="Averta Regular")
      c3c.missing <- FALSE
    }else{
      c3c <- no.data
      c3c.missing <- TRUE
    }
  }else{
    c3a <- no.data
    c3b <- no.data
    c3c <- no.data
    c3a.missing <- TRUE
    c3b.missing <- TRUE
    c3c.missing <- TRUE
  }
  #Chart 4
  indicators = c("female_secondary_enroll_net")
  c4data = subset(countrydat,indicator %in% indicators)
  c4data$value = as.numeric(c4data$value)
  c4data = subset(c4data, !is.na(value))
  c4data <- c4data[order(c4data$year),]
  top4 = c4data$year[c(max(nrow(c4data)-3,1):nrow(c4data))]
  c4data = subset(c4data,year %in% top4)
  c4data$year = as.factor(c4data$year)
  c4.max <- max(c4data$value,na.rm=TRUE)
  c4 <- ggplot(c4data,aes(year,value,fill="Blue")) +
    geom_bar(stat="identity",width=0.6,color=blue,size=1) +
    orangeFill +
    guides(fill=FALSE) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    expand_limits(y=c4.max*1.1) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=40,color=blue,family="Averta Regular")
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue,family="Averta Regular")
  if(real.country=="Middle Africa"){c4=no.data}
  #Chart 5
  indicators = c("basic_water","limited_water","safely_managed_water","surface_water","unimproved_water")
  c5names = c("Basic","Limited","Safely managed","Surface water","Unimproved")
  c5data = subset(countrydat,indicator %in% indicators)
  c5data$value = as.numeric(c5data$value)
  c5data = subset(c5data, !is.na(value))
  c5data <- c5data[order(c5data$year),]
  c5data$year = as.factor(c5data$year)
  c5.max <- max(c5data$value,na.rm=TRUE)
  for(j in 1:length(indicators)){
    ind = indicators[j]
    indname = c5names[j]
    c5data$indicator[which(c5data$indicator==ind)] = indname
  }
  c5data$indicator <- factor(c5data$indicator)
  c5data <- c5data[order(is.na(c5data$value),c5data$year,desc(c5data$indicator)),]
  c5data <- ddply(c5data, .(year),
                       transform, pos = cumsum(value) - (0.5 * value)
                   ,valid = sum(!is.na(value),na.rm=TRUE))
  if(nrow(c5data)>0){
    c5data <- subset(c5data,valid>=1)
    c5.key.data = data.frame(
      indicator = c5names,
      year = as.numeric(rep(NA,5)),
      value = as.numeric(rep(NA,5)),
      pos = as.numeric(rep(NA,5)),
      valid = as.numeric(rep(NA,5))
    )
    c5 <- ggplot(c5data,aes(year,value,fill=indicator)) +
      geom_bar(stat="identity",width=0.7,color=blue,size=1,show.legend=F) +
      geom_point(data=c5.key.data,aes(fill=indicator),size=12,color=blue,stroke=1.5,shape=21) +
      scale_fill_manual(
        labels=c5names
        ,values=quintileFillValues
        ,drop = FALSE
      ) +
      guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=30,color=blue,family="Averta Regular")
        ,legend.justification=c(0,0)
        ,legend.direction="horizontal"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=35,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.2,"lines")
      ) + geom_text(data=subset(c5data,value>4.4),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE,family="Averta Regular") +
      scale_color_manual(breaks=c5names,values=c(white,white,blue,blue,blue),drop=FALSE)
  }else{
    c5 = no.data
  }
  if(real.country %in% c("N. America","Northern America")){
    c5 = no.data
  }

  #Chart 6
  indicators = c("basic_sanitation","limited_sanitation","open_defecation","safely_managed_sanitation","unimproved_sanitation")
  c6names = c("Basic","Limited","Open defecation","Safely managed","Unimproved")
  c6data = subset(countrydat,indicator %in% indicators)
  c6data$value = as.numeric(c6data$value)
  c6data = subset(c6data, !is.na(value))
  c6data <- c6data[order(c6data$year),]
  c6data$year = as.factor(c6data$year)
  c6.max <- max(c6data$value,na.rm=TRUE)
  for(j in 1:length(indicators)){
    ind = indicators[j]
    indname = c6names[j]
    c6data$indicator[which(c6data$indicator==ind)] = indname
  }
  c6data$indicator <- factor(c6data$indicator)
  c6data <- c6data[order(is.na(c6data$value),c6data$year,desc(c6data$indicator)),]
  c6data <- ddply(c6data, .(year),
                  transform, pos = cumsum(value) - (0.5 * value)
                  ,valid = sum(!is.na(value),na.rm=TRUE))
  if(nrow(c6data)>0){
    c6data <- subset(c6data,valid>=1)
    c6.key.data = data.frame(
      indicator = c6names,
      year = as.numeric(rep(NA,5)),
      value = as.numeric(rep(NA,5)),
      pos = as.numeric(rep(NA,5)),
      valid = as.numeric(rep(NA,5))
    )
    c6 <- ggplot(c6data,aes(year,value,fill=indicator)) +
      geom_bar(stat="identity",width=0.7,color=blue,size=1,show.legend=F) +
      geom_point(data=c6.key.data,aes(fill=indicator),size=12,color=blue,stroke=1.5,shape=21) +
      scale_fill_manual(
        labels=c6names
        ,values=quintileFillValues
        ,drop = FALSE
      ) +
      guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=30,color=blue,family="Averta Regular")
        ,legend.justification=c(0,0)
        ,legend.direction="horizontal"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=35,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.2,"lines")
      ) + geom_text(data=subset(c6data,value>4.4),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE,family="Averta Regular") +
      scale_color_manual(breaks=c6names,values=c(white,white,blue,blue,blue),drop=FALSE)
  }else{
    c6 = no.data
  }
 
  #Chart 7
  indicators = c("agriculture_expenditure","education_spending","health_spending","social_protection_spending")
  c7names = c("Agriculture","Education","Health","Social protection")
  c7data = subset(countrydat,indicator %in% indicators)
  c7data$value = as.numeric(c7data$value)
  c7data = subset(c7data, !is.na(value))
  c7data <- c7data[order(c7data$year),]
  c7data$year = as.factor(c7data$year)
  c7.max <- max(c7data$value,na.rm=TRUE)
  for(j in 1:length(indicators)){
    ind = indicators[j]
    indname = c7names[j]
    c7data$indicator[which(c7data$indicator==ind)] = indname
  }
  c7data$indicator <- factor(c7data$indicator)
  c7data <- c7data[order(is.na(c7data$value),c7data$year,desc(c7data$indicator)),]
  c7data <- ddply(c7data, .(year),
                  transform, pos = cumsum(value) - (0.5 * value)
                  ,valid = sum(!is.na(value),na.rm=TRUE))
  
  if(nrow(c7data)>0){
    c7data <- subset(c7data,valid>=1)
    c7.key.data = data.frame(
      indicator = c7names,
      year = as.numeric(rep(NA,4)),
      value = as.numeric(rep(NA,4)),
      pos = as.numeric(rep(NA,4)),
      valid = as.numeric(rep(NA,4))
    )
  uniqueYears <- length(unique(c7data$year))
  if(uniqueYears>1){
    barWidth = 0.7
  }else{
    barWidth = 0.4
  }
  c7 <- ggplot(c7data,aes(year,value,fill=indicator)) +
    geom_bar(stat="identity",width=barWidth,color=blue,size=1,show.legend=F) +
    geom_point(data=c7.key.data,aes(fill=indicator),size=12,color=blue,stroke=1.5,shape=21) +
    scale_fill_manual(
      labels=c7names
      ,values=quintileFillValues
      ,drop = FALSE
    ) +
    guides(fill=guide_legend(title=element_blank(),nrow=2,byrow=TRUE)) +
    simple_style  +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      legend.position="top"
      ,legend.text = element_text(size=30,color=blue,family="Averta Regular")
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c7data,value>3),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE,family="Averta Regular") +
    scale_color_manual(breaks=c7names,values=c(white,white,blue,blue),drop=FALSE)
  }else{
    c7 <- no.data
  }
  grouped_line = function(countrydat, ind, disagg, disagg.values, fill=yellowRedFill, color=yellowRedColor, percent=F, legend=F, factor.years=T, subset.years=F){
    cdata = subset(countrydat, (indicator==ind & disaggregation==disagg))
    cdata$value = as.numeric(cdata$value)
    if(percent){
      cdata$value = cdata$value*100
    }
    cdata = subset(cdata, !is.na(value))
    cdata = subset(cdata, disagg.value %in% disagg.values)
    if(nrow(cdata)==0){
      return(no.data)
    }
    if(sum(cdata$value)==0){
      return(no.data)
    }
    if(length(subset.years)>0){
      if(subset.years){
        cdata = subset(cdata, year %in% subset.years)
      }
    }
    
    cdata <- cdata[order(cdata$year),]
    if(factor.years){
      cdata$year = as.factor(cdata$year)
    }else{
      cdata$year = as.numeric(cdata$year)
    }
    cdata$disagg.value = factor(cdata$disagg.value,levels=disagg.values,ordered=T)
    c.max <- max(cdata$value,na.rm=TRUE)
    c.min.year = min(cdata$year,na.rm=T)
    c.max.year = max(cdata$year,na.rm=T)
    c.year.step = round((c.max.year-c.min.year)/4)
    c.year.seq = seq(c.min.year,c.max.year,max(c.year.step,1))
    c.key.data = data.frame(year=as.numeric(rep(NA,length(disagg.values))),disagg.value=disagg.values,value=as.numeric(rep(NA,length(disagg.values))))
    c.key.data$disagg.value = factor(c.key.data$disagg.value,levels=disagg.values)
    c = ggplot(cdata,aes(year,value,group=disagg.value,color=disagg.value)) +
      geom_line(show.legend=F,size=1) +
      geom_point(data=c.key.data,aes(group=disagg.value,fill=disagg.value),size=12,color=blue,stroke=1.5,shape=21,show.legend=legend) +
      fill +
      color +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE),color=F) +
      simple_style  +
      scale_x_continuous(labels=round,breaks=c.year.seq) +
      scale_y_continuous(expand = c(0,0),limits=c(0,max(c.max*1.1,1))) +
      # expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color=blue,family="Averta Regular")
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_text(size=25,color=dark.grey,family="Averta Regular")
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
        ,panel.grid.major.y = element_line(color=dark.grey)
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_blank()
      )
    if(factor.years){
      c = c + geom_text_repel(size=9,aes(group=disagg.value,label=firstAndLast(safeFormat(value,precision=1),unfactor(year))),vjust=-0.3,show.legend=F,family="Averta Regular") 
    }else{
      c = c + geom_text_repel(size=9,aes(group=disagg.value,label=firstAndLast(safeFormat(value,precision=1),year)),vjust=-0.3,show.legend=F,family="Averta Regular") 
    }
    return(c)
  }
  grouped_bar = function(countrydat, ind, disagg, disagg.values, fill=c(yellow,red), percent=F, legend=F, spacing=1,byrow=F,nrow=2,subset.years=F){
    cdata = subset(countrydat, (indicator==ind & disaggregation==disagg))
    cdata$value = as.numeric(cdata$value)
    if(percent){
      cdata$value = cdata$value*100
    }
    cdata = subset(cdata, !is.na(value))
    cdata <- cdata[order(cdata$year),]
    if(subset.years){
      cdata = subset(cdata,year %in% subset.years)
    }
    cdata$year = as.factor(cdata$year)
    cdata$disagg.value = factor(cdata$disagg.value,levels=disagg.values,ordered=T)
    c.max <- max(cdata$value,na.rm=TRUE)
    c.key.data = data.frame(year=as.numeric(rep(NA,length(disagg.values))),disagg.value=disagg.values,value=as.numeric(rep(NA,length(disagg.values))))
    c.key.data$disagg.value = factor(c.key.data$disagg.value,levels=disagg.values,ordered=T)
    c = ggplot(cdata,aes(year,value,group=disagg.value,fill=disagg.value)) +
      geom_bar(position=position_dodge(spacing),stat="identity",color=blue,show.legend=F,size=1) +
      geom_point(data=c.key.data,aes(group=disagg.value,fill=disagg.value),size=12,color=blue,stroke=1.5,shape=21,show.legend=legend) +
      scale_fill_manual(
        labels=disagg.values
        ,values=fill
        ,drop = FALSE
      ) +
      guides(fill=guide_legend(title=element_blank(),bycol=byrow,nrow=nrow)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0),limits=c(0,max(c.max*1.1,1))) +
      # expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color=blue,family="Averta Regular")
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_blank()
      ) + geom_text(size=9,aes(group=disagg.value,label=safeFormat(value,precision=1)),position=position_dodge(spacing),vjust=-0.3,show.legend=F,color=blue,family="Averta Regular") 
    return(c)
  }
  # Charts 8-16
  global_wasting = data.frame(
    region=real.country,
    disagg.value="Global",
    disaggregation="global",
    indicator = "wasting_percent",
    year=c(2017),
    value=c(7.5)
  )
  global_stunting = data.frame(
    region=real.country,
    disagg.value="Global",
    disaggregation="global",
    indicator = "stunting_percent",
    year=c(2000,2005,2010:2017),
    value=c(32.6,29.3,26.1,25.5,24.9,24.3,23.8,23.2,22.7,22.2)
  )
  global_overweight = data.frame(
    region=real.country,
    disagg.value="Global",
    disaggregation="global",
    indicator = "overweight_percent",
    year=c(2000,2005,2010:2017),
    value=c(4.9,5.1,5.3,5.3,5.4,5.4,5.5,5.6,5.6,5.6)
  )
  countrydat = rbindlist(list(countrydat,global_wasting,global_stunting,global_overweight),fill=T)
  countrydat$disagg.value = unfactor(countrydat$disagg.value)
  countrydat$disagg.value[which(
    countrydat$disaggregation=="gender" &
      countrydat$indicator %in% c("wasting_percent","stunting_percent","overweight_percent") &
      countrydat$disagg.value=="Children under 5")] = this.country
  countrydat$disaggregation = unfactor(countrydat$disaggregation)
  countrydat$disaggregation[which(countrydat$disaggregation=="gender" & countrydat$indicator %in% c("wasting_percent","stunting_percent","overweight_percent"))] = "global"
  wasting_dat = subset(countrydat,indicator=="wasting_percent" & disaggregation=="global" & !is.na(value) & !disagg.value %in% c("Boys","Girls"))
  disaggs = c("Global",this.country)
  stunting_dat = subset(countrydat,indicator=="stunting_percent" & disaggregation=="global" & !is.na(value) & !disagg.value %in% c("Boys","Girls"))
  overweight_dat = subset(countrydat,indicator=="overweight_percent" & disaggregation=="global" & !is.na(value) & !disagg.value %in% c("Boys","Girls"))
  show.row = length(unique(wasting_dat$disagg.value))>1 | length(unique(stunting_dat$disagg.value))>1 | length(unique(overweight_dat$disagg.value))>1
  if(show.row){
    # wasting_years = data.table(wasting_dat)[,.(count=nrow(.SD)),by=.(year)]
    # max_wasting_count = max(max(wasting_years$count,na.rm=T),1)
    # wasting_years = max(subset(wasting_years,count==max_wasting_count)$year)
    wasting_years = 2017
    c8 = grouped_bar(countrydat, "wasting_percent","global",disaggs,fill=lightBlueYellowRed,legend=T,byrow=T,nrow=3,subset.years=wasting_years)
  }else{c8=no.data}
  if(length(unique(stunting_dat$disagg.value))>1){
    reg.years = subset(countrydat,indicator=="stunting_percent" & disaggregation=="global" & disagg.value==this.country & !is.na(value))$year
    c9 = grouped_line(countrydat, "stunting_percent","global",disaggs,color=lightBlueYellowRedColor,fill=lightBlueYellowRedFill,factor.years=F,subset.years=reg.years)
  }else{
    c9=no.data
  }
  if(length(unique(overweight_dat$disagg.value))>1){
    reg.years = subset(countrydat,indicator=="overweight_percent" & disaggregation=="global" & disagg.value==this.country & !is.na(value))$year
    c10 = grouped_line(countrydat, "overweight_percent","global",disaggs,color=lightBlueYellowRedColor,fill=lightBlueYellowRedFill,factor.years=F,subset.years=reg.years)
  }else{
    c10=no.data
  }
  wasting_dat = subset(countrydat,indicator=="wasting_percent" & disaggregation=="income" & !is.na(value))
  wasting_missing = nrow(wasting_dat)==0
  if(!wasting_missing){
    wasting_years = data.table(wasting_dat)[,.(count=nrow(.SD)),by=.(year)]
    max_wasting_count = max(max(wasting_years$count,na.rm=T),1)
    wasting_years = max(subset(wasting_years,count==max_wasting_count)$year)
    c11 = grouped_bar(countrydat, "wasting_percent","income",c("Lowest","Second lowest","Middle","Second highest","Highest"),fill=quintileFillValues,legend=T,byrow=T,nrow=3,subset.years=wasting_years)
  }else{c11=no.data}
  
  max.year = max(subset(countrydat,indicator=="stunting_percent" & disaggregation=="income")$year,na.rm=T)
  c12 = grouped_bar(countrydat, "stunting_percent","income",c("Lowest","Second lowest","Middle","Second highest","Highest"),fill=quintileFillValues,subset.years=max.year,legend=wasting_missing,byrow=T,nrow=3)
  max.year = max(subset(countrydat,indicator=="overweight_percent" & disaggregation=="income")$year,na.rm=T)
  c13 = grouped_bar(countrydat, "overweight_percent","income",c("Lowest","Second lowest","Middle","Second highest","Highest"),fill=quintileFillValues,subset.years=max.year,legend=wasting_missing,byrow=T,nrow=3)
  
  wasting_dat = subset(countrydat,indicator=="wasting_percent" & disaggregation=="location" & !is.na(value))
  wasting_missing = nrow(wasting_dat)==0
  if(!wasting_missing){
    wasting_years = data.table(wasting_dat)[,.(count=nrow(.SD)),by=.(year)]
    max_wasting_count = max(max(wasting_years$count,na.rm=T),1)
    wasting_years = max(subset(wasting_years,count==max_wasting_count)$year)
    c14 = grouped_bar(countrydat, "wasting_percent","location",c("Urban","Rural"),legend=T,subset.years=wasting_years)
  }else{c14=no.data}
  max.year = max(subset(countrydat,indicator=="stunting_percent" & disaggregation=="location")$year,na.rm=T)
  c15 = grouped_bar(countrydat, "stunting_percent","location",c("Urban","Rural"),subset.years=max.year,legend=wasting_missing,byrow=T,nrow=3)
  max.year = max(subset(countrydat,indicator=="overweight_percent" & disaggregation=="location")$year,na.rm=T)
  c16 = grouped_bar(countrydat, "overweight_percent","location",c("Urban","Rural"),subset.years=max.year,legend=wasting_missing,byrow=T,nrow=3)
  
  # Chart 17
  wasting = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Wasting alone")$value)/100
  stunting = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Stunting alone")$value)/100
  overweight = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Overweight alone")$value)/100
  wasting_and_stunting = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Wasting and stunting")$value)/100
  stunting_and_overweight = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Stunting and overweight")$value)/100
  all = 1
  
  free = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Free from")$value)/100

  combinations = c(
    A=0
    ,B=0
    ,C=0
    ,D=all
    ,"A&B"=0
    ,"A&C"=0
    ,"A&D"=wasting+wasting_and_stunting
    ,"B&C"=0
    ,"B&D"=stunting+wasting_and_stunting+stunting_and_overweight
    ,"C&D"=overweight+stunting_and_overweight
    ,"A&B&C"=0
    ,"A&B&D"=wasting_and_stunting
    ,"A&C&D"=0
    ,"B&C&D"=stunting_and_overweight
    ,"A&B&C&D"=0
  )
  
  label.vals = combinations
  if(length(wasting)>0){
  label.vals["A&D"] = wasting
  label.vals["B&D"] = stunting
  label.vals["C&D"] = overweight
  label.vals["D"] = free
  }
  label.text = percent(label.vals)
  label.text[which(label.vals<=0.02)] = ""
  
  c17 = tryCatch({euler(combinations,shape="ellipse")},error=function(e){no.data})
  
  # Chart 18
  indicators = c(
    "continued_breastfeeding_2yr",
    "continued_breastfeeding_1yr",
    "minimum_accept_diet",
    "minimum_diet_diversity",
    "minimum_meal",
    "solid_foods",
    "exclusive_breastfeeding",
    "early_initiation"
  )
  c18names = c(
    "Continued breastfeeding at 2 years",
    "Continued breastfeeding at 1 year",
    "Minimum acceptable diet",
    "Minimum dietary diversity",
    "Minimum meal frequency",
    "Introduction to solids, semi-solid foods",
    "Exclusive breastfeeding",
    "Early initiation"
  )
  c18data = subset(countrydat,indicator %in% indicators)
  c18data$value = as.numeric(c18data$value)
  c18data = subset(c18data, !is.na(value))
  c18data = data.table(c18data)
  c18data = c18data[,.SD[which.max(.SD$year)],by=.(indicator,disagg.value)]
  c18data$indicator = unfactor(c18data$indicator)
  c18.max <- max(c18data$value,na.rm=TRUE)
  c18.min = 0
  for(j in 1:length(indicators)){
    ind = indicators[j]
    indname = c18names[j]
    c18data$indicator[which(c18data$indicator==ind)] = indname
  }
  c18data$indicator <- factor(c18data$indicator,levels=rev(c18names))
  c18a.data = subset(c18data,disaggregation=="income")
  c18a.data = subset(c18a.data, disagg.value %in% c("Lowest","Highest"))
  c18a.indicators = unique(c18a.data$indicator)
  c18data = subset(c18data,indicator %in% c18a.indicators)
  c18a.data$disagg.value = factor(c18a.data$disagg.value,levels=c("Lowest","Highest"))
  c18b.data = subset(c18data,disaggregation=="location")
  if(nrow(c18a.data)==0){
    c18a <- no.data
    c18a.missing = T
    if(nrow(c18b.data)>0){
      c18b.missing = F
      c18b = ggplot(c18b.data,aes(x=value,y=indicator)) +
        geom_rect(aes(group=year,xmin=c18.min,xmax=c18.max,ymin=indicator,ymax=indicator),color=grey) +
        geom_point(size=7,aes(group=disagg.value,colour=disagg.value),shape=21,fill="transparent",stroke=2) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
        redYellowColor +
        guides(colour=guide_legend(title=element_blank(),reverse=TRUE,nrow=2,byrow=T)) +
        simple_style  +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color=blue,family="Averta Regular")
          ,legend.justification=c(0,0)
          ,legend.direction="horizontal"
          ,axis.title.y=element_blank()
          ,axis.title.x=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color=blue, size = 1)
          ,axis.text.y = element_text(size=21,color=blue,family="Averta Regular")
          ,axis.text.x = element_text(size=25,color=blue,family="Averta Regular")
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key.size = unit(2.5,"lines")
          ,title = element_text(size=30,color=blue,family="Averta Regular")
        ) + labs(title="Urban/rural (%)")
    }else{
      c18b.missing = T
    }
  }else{
    c18a.missing = F
    c18a = ggplot(c18a.data,aes(x=value,y=indicator)) +
      geom_rect(aes(group=year,xmin=c18.min,xmax=c18.max,ymin=indicator,ymax=indicator),color=grey) +
      geom_point(size=7,aes(group=disagg.value,colour=disagg.value),shape=21,fill="transparent",stroke=2) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      redYellowColor +
      guides(colour=guide_legend(title=element_blank(),reverse=TRUE,nrow=2,byrow=T)) +
      simple_style  +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color=blue,family="Averta Regular")
        ,legend.justification=c(0,0)
        ,legend.direction="horizontal"
        ,axis.title.y=element_blank()
        ,axis.title.x=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1)
        ,axis.text.y = element_text(size=21,color=blue,family="Averta Regular")
        ,axis.text.x = element_text(size=25,color=blue,family="Averta Regular")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.size = unit(2.5,"lines")
        ,title = element_text(size=30,color=blue,family="Averta Regular")
      ) + labs(title="Wealth quintiles (%)")
    if(nrow(c18b.data)>0){
      c18b.missing = F
      c18b = ggplot(c18b.data,aes(x=value,y=indicator)) +
        geom_rect(aes(group=year,xmin=c18.min,xmax=c18.max,ymin=indicator,ymax=indicator),color=grey) +
        geom_point(size=7,aes(group=disagg.value,colour=disagg.value),shape=21,fill="transparent",stroke=2) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
        redYellowColor +
        guides(colour=guide_legend(title=element_blank(),reverse=TRUE,nrow=2,byrow=T)) +
        simple_style  +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color=blue,family="Averta Regular")
          ,legend.justification=c(0,0)
          ,legend.direction="horizontal"
          ,axis.title.y=element_blank()
          ,axis.title.x=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color=blue, size = 1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color=blue,family="Averta Regular")
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key.size = unit(2.5,"lines")
          ,title = element_text(size=30,color=blue,family="Averta Regular")
        ) + labs(title="Urban/rural (%)")
    }else{
      c18b.missing = T
    }
  }
  # Charts 19-27
  c19 = grouped_line(countrydat, "adolescent_underweight","gender",c("Boys","Girls"),percent=T,legend=T,color=orangeLightBlueColor,fill=orangeLightBlueFill,factor.years=F)
  c20 = grouped_line(countrydat, "adolescent_overweight","gender",c("Boys","Girls"),percent=T,color=orangeLightBlueColor,fill=orangeLightBlueFill,factor.years=F)
  c21 = grouped_line(countrydat, "adolescent_obesity","gender",c("Boys","Girls"),percent=T,color=orangeLightBlueColor,fill=orangeLightBlueFill,factor.years=F)
  c22 = grouped_line(countrydat, "adult_diabetes","gender",c("Male","Female"),percent=T,legend=T,color=orangeLightBlueColor,fill=orangeLightBlueFill,factor.years=F)
  c23 = grouped_line(countrydat, "adult_overweight","gender",c("Male","Female"),percent=T,color=orangeLightBlueColor,fill=orangeLightBlueFill,factor.years=F)
  c24 = grouped_line(countrydat, "adult_obesity","gender",c("Male","Female"),percent=T,color=orangeLightBlueColor,fill=orangeLightBlueFill,factor.years=F)
  c25 = grouped_line(countrydat, "adult_blood_pressure","gender",c("Male","Female"),percent=T,legend=T,color=orangeLightBlueColor,fill=orangeLightBlueFill,factor.years=F)
  c26 = grouped_line(countrydat, "adult_anemia","pregnancy",c("All women","Pregnant women","Non-pregnant women"),color=lightBlueYellowRedColor,fill=lightBlueYellowRedFill,legend=T,factor.years=F)
  ind = "adult_sodium"
  spacing = 1
  legend = T
  fill=yellowOrangeRedFill
  c27data = subset(countrydat, (indicator==ind))
  c27data$value = as.numeric(c27data$value)
  c27data = subset(c27data, !is.na(value))
  if(nrow(c27data)>0){
    c27data$disagg.value = this.country
    c27.global = data.frame(year=max(c27data$year),disagg.value="Global",value=5.6)
    disagg.values = c(this.country,"Global")
    c27data = rbindlist(list(c27data,c27.global),fill=T)
    c27data <- c27data[order(c27data$year),]
    c27data$year = as.factor(c27data$year)
    c27data$disagg.value = disagg.values
    c27data$disagg.value = factor(c27data$disagg.value,levels=disagg.values)
    c27.max <- max(c27data$value,na.rm=TRUE)
    c27.key.data = data.frame(year=as.numeric(rep(NA,length(disagg.values))),disagg.value=disagg.values,value=as.numeric(rep(NA,length(disagg.values))))
    c27.key.data$disagg.value = factor(c27.key.data$disagg.value,levels=disagg.values)
    c27 = ggplot(c27data,aes(year,value,group=disagg.value,fill=disagg.value)) +
      geom_bar(position=position_dodge(spacing),stat="identity",color=blue,show.legend=F,size=1) +
      geom_point(data=c27.key.data,aes(group=disagg.value,fill=disagg.value),size=12,color=blue,stroke=1.5,shape=21,show.legend=legend) +
      fill +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0),limits=c(0,max(c27.max*1.1,1))) +
      # expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color=blue,family="Averta Regular")
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_blank()
      ) + geom_text(size=9,aes(group=disagg.value,label=safeFormat(value,precision=1)),position=position_dodge(spacing),vjust=-0.3,show.legend=F,color=blue,family="Averta Regular") 
  }else{
    c27 = no.data
  }
 
  # Chart 28
  food.order = c(
    "Calcium",
    "Fruit",
    "Legumes",
    "Milk",
    "Nuts and seeds",
    "Omega 3",
    "Polyunsaturated fat",
    "Processed meat",
    "Red meat",
    "Salt",
    "Saturated fat",
    "Sugar-sweetened beverages",
    "Trans fat",
    "Vegetables",
    "Whole grain"
  )
  c28.data = subset(countrydat,component == "M")
  if(nrow(c28.data)>0){
    this.region = subregion_list[real.country][[1]]
    if(nchar(this.region)>20){
      this.region = "Regional"
    }
    c28.data$value = as.numeric(c28.data$value)
    c28.data$value[which(c28.data$unit=="%")] = c28.data$value[which(c28.data$unit=="%")]*100
    setnames(c28.data,"rec","recommended")
    c28.data$percent = c28.data$value/c28.data$recommended
    # Outliers
    calc_outlier = function(sd){
      results = c()
      outlier_val = 2.1
      for(i in 1:nrow(sd)){
        row = sd[i,]
        if(row$outlier==1){
          results = c(results,outlier_val)
          outlier_val = outlier_val + 0.1
        }else{
          results = c(results,row$percent)
        }
      }
      return(results)
    }
    setnames(c28.data,"indicator","food")
    setnames(c28.data,"disagg.value","class")
    c28.data = c28.data[order(c28.data$food, c28.data$percent),]
    c28.data$outlier = 0
    c28.data$outlier[which(c28.data$percent>2)] = 1
    c28.data$percent[which(c28.data$percent>2)] = 2.1
    c28.data = data.table(c28.data)
    if(regional==1){
      c28.data = subset(c28.data,class %in% c("National","Global"))
    }
    c28.data[,percent:=calc_outlier(.SD),by=.(food)]
    c28.data$food = factor(c28.data$food,levels=rev(food.order))
    c28.data = c28.data[order(-c28.data$food),]
    if(regional==1){
      c28.data$column = c(rep(1,14),rep(2,16))
    }else{
      c28.data$column = c(rep(1,21),rep(2,24))
    }
    bar.dat = unique(c28.data[,c("food","recommended","column"),with=F])
    bar.dat$class = this.country
    c28.max = min(max(c28.data$percent,na.rm=T),2)
    
    c28.data$class[which(c28.data$class=="National")] = this.country
    
    if(regional==0){
      c28.data$class[which(c28.data$class=="Regional")] = this.region
      c28.data$class = factor(c28.data$class,levels=c(this.country,this.region,"Global"))
    }else{
      c28.data$class = factor(c28.data$class,levels=c(this.country,"Global"))
    }
    
    
    i = 1
    c28.data.sub = subset(c28.data,column==i)
    trmel = data.frame(food="",recommended=0,example=T,class=this.country)
    c28.data.sub = rbindlist(list(c28.data.sub,trmel),fill=T)
    bar.dat.sub = subset(bar.dat,column==i)
    bar.dat.sub = rbindlist(list(bar.dat.sub,trmel),fill=T)
    c28a =
      ggplot(c28.data.sub,aes(x=food,colour=class)) +
      geom_bar(data=bar.dat.sub,aes(y=c28.max),fill="white",color=blue,stat="identity",width=0.4) +
      geom_bar(data=bar.dat.sub,aes(y=1),fill="white",color=blue,stat="identity",width=0.4) +
      geom_point(aes(y=percent),size=8,shape=21,fill="transparent",stroke=2) +
      geom_text(data=subset(c28.data.sub,is.na(example) & class=="Global"),aes(y=1,label=paste0(round.simple(recommended,1),unit)),color=blue,vjust=-1.3,size=10,family="Averta Regular") +
      geom_text(data=subset(c28.data.sub,example==T),aes(y=1,label="Midpoint of TMREL"),color=blue,vjust=-1.3,size=10,family="Averta Regular") +
      geom_text_repel(data=subset(c28.data.sub,outlier==1),aes(y=percent,label=round.simple(value)),color=blue,size=8,family="Averta Regular",vjust=2) +
      annotate("text", x=8, y=0.4, label="0%/0g of TMREL",size=9,color=blue,family="Averta Regular") +
      annotate("text", x=8, y=1.6, label="200% of TMREL",size=9,color=blue,family="Averta Regular") +
      trmelColor + 
      coord_flip() +
      theme_classic() +
      theme(
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=25,color=blue,family="Averta Regular"),
        legend.title=element_blank(),
        legend.position="left",
        legend.text = element_text(size=22,color=blue,family="Averta Regular"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.key.size = unit(2.5,"lines")
      )
    
    i = 2
    c28.data.sub = subset(c28.data,column==i)
    bar.dat.sub = subset(bar.dat,column==i)
    c28b = ggplot(c28.data.sub,aes(x=food,colour=class)) +
      geom_bar(data=bar.dat.sub,aes(y=c28.max),fill="white",color=blue,stat="identity",width=0.4) +
      geom_bar(data=bar.dat.sub,aes(y=1),fill="white",color=blue,stat="identity",width=0.4) +
      geom_point(aes(y=percent),size=8,shape=21,fill="transparent",stroke=2,show.legend=F) +
      geom_text(data=subset(c28.data.sub, class=="Global"),aes(y=1,label=paste0(round.simple(recommended,1),unit)),color=blue,vjust=-1.3,size=10,family="Averta Regular") +
      geom_text_repel(data=subset(c28.data.sub,outlier==1),aes(y=percent,label=round.simple(value)),color=blue,size=7,family="Averta Regular",vjust=2) +
      trmelColor + 
      coord_flip() +
      theme_classic() +
      theme(
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=25,color=blue,family="Averta Regular"),
        legend.title=element_blank(),
        legend.text = element_text(size=22,color=blue,family="Averta Regular"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.key.size = unit(2.5,"lines")
      )
  }else{
    c28a = no.data
    c28b = no.data
  }
  
  # Chart 29
  indicators = c("oda_per_capita")
  c29names = c("Basic nutrition ODA received")
  y.lab = "ODA, US$, per 1,000 people"
  c29data = subset(countrydat,indicator %in% indicators)
  c29data$value = as.numeric(c29data$value)
  c29data$value= c29data$value * 1000
  c29data = subset(c29data, !is.na(value))
  if(sum(c29data$value)>0){
    c29data = c29data[,c("year","indicator","value")]
    c29.oda.max <- max(c29data$value,na.rm=TRUE)
    c29data$indicator = unfactor(c29data$indicator)
    for(j in 1:length(indicators)){
      ind = indicators[j]
      indname = c29names[j]
      c29data$indicator[which(c29data$indicator==ind)] = indname
    }
    
    c29.key.data.1 = data.frame(year=as.numeric(rep(NA,1)),indicator=c29names[1],value=as.numeric(rep(NA,1)))
    c29 = ggplot(c29data,aes(x=year,y=value)) +
      geom_area(alpha=1,show.legend=F,color=blue,fill=yellow) +
      geom_point(data=c29.key.data.1,aes(group=indicator,fill=indicator),size=12,color=blue,stroke=1.5,shape=21) +
      yellowFill +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE),color=guide_legend(title=element_blank(),byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(
        expand = c(0,0),
        limits=c(0,max(c29.oda.max*1.1,1)),
      ) +
      theme(
        legend.position="top"
        ,legend.box = "vertical"
        ,legend.text = element_text(size=25,color=blue,family="Averta Regular")
        ,legend.justification=c(0,0)
        ,legend.box.just = "left"
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_text(size=20,color=blue,family="Averta Regular")
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_text(size=25,color=dark.grey,family="Averta Regular")
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
        ,panel.grid.major.y = element_line(color=dark.grey)
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key.width = unit(1,"cm")
      ) + labs(y = y.lab)
  }else{
    c29 = no.data
  }
  
  #Have both c1a and c1b
  if(!c1a.missing && !c1b.missing){
    Cairo(family="Averta Regular",file="c1a.png",width=400,height=600,units="px",bg="white")
    tryCatch({print(c1a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c1b.png",width=400,height=600,units="px",bg="white")
    tryCatch({print(c1b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c1.png",width=800,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have only c1a
  if(!c1a.missing && c1b.missing){
    Cairo(family="Averta Regular",file="c1a.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c1b.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c1.png",width=800,height=600,units="px",bg="white")
    tryCatch({print(c1a)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have only c1b
  if(c1a.missing && !c1b.missing){
    Cairo(family="Averta Regular",file="c1a.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c1b.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c1.png",width=800,height=600,units="px",bg="white")
    tryCatch({print(c1b)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have neither c1a or c1b
  if(c1a.missing && c1b.missing){
    Cairo(family="Averta Regular",file="c1a.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c1b.png",width=400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c1.png",width=800,height=600,units="px",bg="transparent")
    print(no.data)
    dev.off()
  }
  Cairo(family="Averta Regular",file="c2.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c2)},error=function(e){message(e);print(no.data)})
  dev.off()
  #Have c3a, c3b, and c3c
  if(!c3a.missing && !c3b.missing && !c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c3a and c3b only
  if(!c3a.missing && !c3b.missing && c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c3a and c3c only
  if(!c3a.missing && c3b.missing && !c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c3b and c3c only
  if(c3a.missing && !c3b.missing && !c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #have c3a only
  if(!c3a.missing && c3b.missing && c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have c3b only
  if(c3a.missing && !c3b.missing && c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #have c3c only
  if(c3a.missing && c3b.missing && !c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have none of c3s
  if(c3a.missing && c3b.missing && c3c.missing){
    Cairo(family="Averta Regular",file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(no.data)
    dev.off()
  }
  Cairo(family="Averta Regular",file="c4.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c4)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c5.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c5)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c6.png",width=816,height=714,units="px",bg="white")
  tryCatch({print(c6)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c7.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c7)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c8.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c8)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c9.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c9)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c10.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c10)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c11.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c11)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c12.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c12)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c13.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c13)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c14.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c14)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c15.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c15)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c16.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c16)},error=function(e){message(e);print(no.data)})
  dev.off()
  if(length(free)>0){
    png(filename="c17.png",width=2400,height=750,units="px",bg="white",type="cairo",family="Averta Regular")
    print(plot(c17,
               legend = list(labels=c("Wasting only","Stunting only","Overweight only","No wasting, stunting or overweight"),font="arial",vgap=2,fontsize=25,fontcolor=blue,fontfamily="Averta Regular")
               ,edges = list(col=blue,lwd=3)
               ,quantities = list(labels=label.text,fontsize=20,col=blue,family="Averta Regular")
               ,fills=list(fill=c(yellow,orange,light.blue,grey),alpha=1)
    ))
    dev.off()
  }else{
    Cairo(family="Averta Regular",file="c17.png",width=2400,height=750,units="px",bg="white")
    print(no.data)
    dev.off()
  }
  
  #Have both c18a and c18b
  if(!c18a.missing && !c18b.missing){
    Cairo(family="Averta Regular",file="c18a.png",width=1350,height=600,units="px",bg="white")
    tryCatch({print(c18a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c18b.png",width=1050,height=600,units="px",bg="white")
    tryCatch({print(c18b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(family="Averta Regular",file="c18.png",width=2400,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have only c18a
  if(!c18a.missing && c18b.missing){
    Cairo(family="Averta Regular",file="c18a.png",width=1350,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c18b.png",width=1050,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c18.png",width=2400,height=600,units="px",bg="white")
    tryCatch({print(c18a)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have only c18b
  if(c18a.missing && !c18b.missing){
    Cairo(family="Averta Regular",file="c18a.png",width=1350,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c18b.png",width=1050,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c18.png",width=2400,height=600,units="px",bg="white")
    tryCatch({print(c18b)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have neither c18a or c18b
  if(c18a.missing && c18b.missing){
    Cairo(family="Averta Regular",file="c18a.png",width=1350,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c18b.png",width=1050,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(family="Averta Regular",file="c18.png",width=2400,height=600,units="px",bg="transparent")
    print(no.data)
    dev.off()
  }
  Cairo(family="Averta Regular",file="c19.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c19)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c20.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c20)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c21.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c21)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c22.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c22)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c23.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c23)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c24.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c24)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c25.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c25)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c26.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c26)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c27.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c27)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c28a.png",width=1200,height=960,units="px",bg="white")
  tryCatch({print(c28a)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c28b.png",width=1200,height=960,units="px",bg="white")
  tryCatch({print(c28b)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(family="Averta Regular",file="c29.png",width=1000,height=500,units="px",bg="white")
  tryCatch({print(c29)},error=function(e){message(e);print(no.data)})
  dev.off()
}
####End loop####
