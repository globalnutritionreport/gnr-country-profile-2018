####Setup#####
list.of.packages <- c("ggplot2","reshape2","data.table","scales","varhandle","Cairo","plyr","eulerr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- "~/git/gnr-country-profile-2018"
setwd(wd)

dat <- read.csv("data.csv",na.strings=c("","."," "),as.is=TRUE)

countries <- unique(dat$country)

wd <- "~/git/gnr-country-profile-2018/charts"
setwd(wd)

# unlink(
#   dir(wd, full.names = TRUE)
#   , recursive = TRUE
# )
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

yellow <- "#FABD62"
orange <- "#F68516"
red <- "#D8511B"
blue <- "#3D5163"
light.blue <- "#82C6BA"
lighter.blue <- "#BFDFD4"
grey <- "#BFCCD7"
white <- "#ffffff"

# Not used in new design
purple <- "#71105f"
black <- "#443e42"

quintileFillValues <- c(red, orange, yellow, lighter.blue, light.blue)

yellowOrangeFill <- scale_fill_manual(values=c(yellow,orange))
orangeYellowFill <- scale_fill_manual(values=c(orange,yellow))
yellowOrangeRedFill <- scale_fill_manual(values=c(yellow,orange,red))
orangeFill <- scale_fill_manual(values=c(orange))
yellowFill <- scale_fill_manual(values=c(yellow))
blueFill <- scale_fill_manual(values=c(blue))
lightBlueLighterBlueFill  <- scale_fill_manual(values=c(light.blue, lighter.blue))
lightBlueFill <- scale_fill_manual(values=c(light.blue))
lighterBlueFill <- scale_fill_manual(values=c(lighter.blue))
quintileFill <-  scale_fill_manual(values=quintileFillValues)

yellowOrangeColor <- scale_color_manual(values=c(yellow,orange))
orangeYellowColor <- scale_color_manual(values=c(orange,yellow))
yellowOrangeRedColor <- scale_color_manual(values=c(yellow,orange,red))
orangeColor <- scale_color_manual(values=c(orange))
blueColor <- scale_color_manual(values=c(blue))
quintileColor <-  scale_color_manual(values=quintileFillValues)

textQuintileOffset <- scale_color_manual(values=c(white,white,blue,blue,blue))

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

####End setup####
####Loop####
for(this.country in countries){
  message(this.country)
  # dir.create(paste(wd,this.country,sep="/"))
  setwd(paste(wd,this.country,sep="/"))
  countrydat <- subset(dat,country==this.country)
  #Chart 1 part a and b
  indicators = c("190_percent","310_percent","GDP_capita_PPP")
  c1data = subset(countrydat,indicator %in% indicators)
  c1data$value = as.numeric(c1data$value)
  c1data <- subset(c1data,!is.na(year))
  if(nrow(c1data)!=0){
    c1a.melt <- subset(c1data, indicator %in% c("190_percent","310_percent"))
    c1a.melt$variable = NA
    c1a.melt$variable[which(c1a.melt$indicator=="190_percent")] = "$1.90/day"
    c1a.melt$variable[which(c1a.melt$indicator=="310_percent")] = "$3.10/day"
    c1a.melt <- subset(c1a.melt,!is.na(value))
    c1a.melt <- c1a.melt[order(c1a.melt$year),]
    c1a.melt$year = as.factor(c1a.melt$year)
    c1a.max <- max(c1a.melt$value,na.rm=TRUE)
    c1b.melt <- subset(c1data,indicator == "GDP_capita_PPP")
    c1b.melt$variable = "PPP($) GDP per capita"
    c1b.melt <- subset(c1b.melt,!is.na(value))
    c1b.melt <- c1b.melt[order(c1b.melt$year),]
    c1b.melt$year = as.factor(c1b.melt$year)
    c1b.max <- max(c1b.melt$value,na.rm=TRUE)
    c1a.key.data = data.frame(year=as.numeric(c(NA,NA)),variable=c("$1.90/day","$3.10/day"),value=as.numeric(c(NA,NA)))
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
        ,legend.text = element_text(size=35,color=blue)
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_blank()
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue)
    if(nrow(c1a.melt)==0){c1a.missing<-TRUE}else{c1a.missing<-FALSE}
    c1b.key.data = data.frame(year=as.numeric(c(NA)),variable=c("PPP($) GDP per capita"),value=as.numeric(c(NA)))
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
        ,legend.text = element_text(size=35,color=blue)
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_blank()
        ,legend.key.size = unit(2.2,"lines")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue)
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
      ,legend.text = element_text(size=40,color=blue)
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color=blue,margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue)
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
          labels=c(bquote(atop('Undernourishment' ^ 1,'(% population)')))
          ,breaks=c(indicators[1])
          ,values=c(yellow)
        ) +
        guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
        simple_style  +
        scale_y_continuous(expand = c(0,0)) +
        expand_limits(y=c3a.max*1.1) +
        theme(
          legend.position="top"
          ,legend.text = element_text(size=22,color=blue)
          ,legend.justification=c(0.08,0)
          ,legend.direction="vertical"
          ,axis.title.x=element_blank()
          ,axis.title.y=element_blank()
          ,axis.ticks=element_blank()
          ,axis.line.y = element_blank()
          ,axis.line.x = element_line(color=blue, size = 1.1)
          ,axis.text.y = element_blank()
          ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0))
          ,legend.background = element_rect(fill = "transparent", colour = "transparent")
          ,legend.key = element_rect(fill = "transparent", colour = "transparent")
        ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue)
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
        labels=c(bquote(atop('Availability of fruit and' ^ 1,'vegetables (grams)')))
        ,breaks=c(indicators[2])
        ,values=c(orange)
      ) +
      guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c3b.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color=blue)
        ,legend.justification=c(0.08,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue)
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
        labels=c(bquote(atop('% of total calories' ^ 2,'from non-staples')))
        ,breaks=c(indicators[3])
        ,values=c(lighter.blue)
      ) +
      guides(fill = guide_legend(title=element_blank(),byrow=TRUE,override.aes = list(alpha = 1))) +
      simple_style  +
      scale_y_continuous(expand = c(0,0)) +
      expand_limits(y=c3c.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=22,color=blue)
        ,legend.justification=c(0.08,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ) + geom_text(size=9,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue)
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
      ,legend.text = element_text(size=40,color=blue)
      ,legend.justification=c(0,0)
      ,legend.direction="vertical"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=40,color=blue,margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.5,"lines")
    ) + geom_text(size=13,aes(label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,color=blue)
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
      ,legend.text = element_text(size=30,color=blue)
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color=blue,margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c5data,value>3),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=c5names,values=c(white,white,white,blue,blue),drop=FALSE)
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
      ,legend.text = element_text(size=30,color=blue)
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color=blue,margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c6data,value>3),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=c6names,values=c(white,white,white,blue,blue),drop=FALSE)
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
  c7data <- subset(c7data,valid>=1)
  c7.key.data = data.frame(
    indicator = c7names,
    year = as.numeric(rep(NA,4)),
    value = as.numeric(rep(NA,4)),
    pos = as.numeric(rep(NA,4)),
    valid = as.numeric(rep(NA,4))
  )
  if(nrow(c7data)>0){

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
      ,legend.text = element_text(size=30,color=blue)
      ,legend.justification=c(0,0)
      ,legend.direction="horizontal"
      ,axis.title.x=element_blank()
      ,axis.title.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.line.y = element_blank()
      ,axis.line.x = element_line(color=blue, size = 1.1)
      ,axis.text.y = element_blank()
      ,axis.text.x = element_text(size=35,color=blue,margin=margin(t=20,r=0,b=0,l=0))
      ,legend.background = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key = element_rect(fill = "transparent", colour = "transparent")
      ,legend.key.size = unit(2.2,"lines")
    ) + geom_text(data=subset(c7data,value>0),size=10,aes(y=pos,label=safeFormat(value),color=indicator),show.legend=FALSE) +
    scale_color_manual(breaks=c7names,values=c(white,white,white,blue),drop=FALSE)
  }else{
    c7 <- no.data
  }
  grouped_line = function(countrydat, ind, disagg, disagg.values, fill=orangeYellowFill, color=orangeYellowColor, percent=F, legend=F){
    cdata = subset(countrydat, (indicator==ind & disaggregation==disagg))
    cdata$value = as.numeric(cdata$value)
    if(percent){
      cdata$value = cdata$value*100
    }
    cdata = subset(cdata, !is.na(value))
    cdata <- cdata[order(cdata$year),]
    cdata$year = as.factor(cdata$year)
    cdata$disagg.value = factor(cdata$disagg.value,levels=disagg.values)
    c.max <- max(cdata$value,na.rm=TRUE)
    c.key.data = data.frame(year=as.numeric(rep(NA,length(disagg.values))),disagg.value=disagg.values,value=as.numeric(rep(NA,length(disagg.values))))
    c.key.data$disagg.value = factor(c.key.data$disagg.value,levels=disagg.values)
    c = ggplot(cdata,aes(year,value,group=disagg.value,color=disagg.value)) +
      geom_line(show.legend=F,size=1) +
      geom_point(data=c.key.data,aes(group=disagg.value,fill=disagg.value),size=12,color=blue,stroke=1.5,shape=21,show.legend=legend) +
      fill +
      color +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0),limits=c(0,max(c.max*1.1,1))) +
      # expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color=blue)
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_blank()
      ) + geom_text(size=9,aes(group=disagg.value,label=safeFormat(value)),position=position_dodge(0.5),vjust=-0.3,show.legend=F) 
    return(c)
  }
  grouped_bar = function(countrydat, ind, disagg, disagg.values, fill=orangeYellowFill, percent=F, legend=F){
    cdata = subset(countrydat, (indicator==ind & disaggregation==disagg))
    cdata$value = as.numeric(cdata$value)
    if(percent){
      cdata$value = cdata$value*100
    }
    cdata = subset(cdata, !is.na(value))
    cdata <- cdata[order(cdata$year),]
    cdata$year = as.factor(cdata$year)
    cdata$disagg.value = factor(cdata$disagg.value,levels=disagg.values)
    c.max <- max(cdata$value,na.rm=TRUE)
    c.key.data = data.frame(year=as.numeric(rep(NA,length(disagg.values))),disagg.value=disagg.values,value=as.numeric(rep(NA,length(disagg.values))))
    c.key.data$disagg.value = factor(c.key.data$disagg.value,levels=disagg.values)
    c = ggplot(cdata,aes(year,value,group=disagg.value,fill=disagg.value)) +
      geom_bar(position="dodge",stat="identity",color=blue,show.legend=F,size=1) +
      geom_point(data=c.key.data,aes(group=disagg.value,fill=disagg.value),size=12,color=blue,stroke=1.5,shape=21,show.legend=legend) +
      fill +
      guides(fill=guide_legend(title=element_blank(),byrow=TRUE)) +
      simple_style  +
      scale_y_continuous(expand = c(0,0),limits=c(0,max(c.max*1.1,1))) +
      # expand_limits(y=c1a.max*1.1) +
      theme(
        legend.position="top"
        ,legend.text = element_text(size=35,color=blue)
        ,legend.justification=c(0,0)
        ,legend.direction="vertical"
        ,axis.title.x=element_blank()
        ,axis.title.y=element_blank()
        ,axis.ticks=element_blank()
        ,axis.line.y = element_blank()
        ,axis.line.x = element_line(color=blue, size = 1.1)
        ,axis.text.y = element_blank()
        ,axis.text.x = element_text(size=25,color=blue,margin=margin(t=20,r=0,b=0,l=0))
        ,legend.background = element_rect(fill = "transparent", colour = "transparent")
        ,legend.key = element_blank()
      ) + geom_text(size=9,aes(group=disagg.value,label=safeFormat(value)),position=position_dodge(1),vjust=-0.3,show.legend=F,color=blue) 
    return(c)
  }
  # Charts 8-16
  c8 = grouped_line(countrydat, "stunting_percent","gender",c("Male","Female","Both"),percent=T,color=yellowOrangeRedColor,fill=yellowOrangeRedFill)
  c9 = grouped_bar(countrydat, "wasting_percent","gender",c("Male","Female","Both"),percent=T,fill=yellowOrangeRedFill,legend=T)
  c10 = grouped_line(countrydat, "overweight_percent","gender",c("Male","Female","Both"),percent=T,color=yellowOrangeRedColor,fill=yellowOrangeRedFill)
  c11 = grouped_line(countrydat, "stunting_percent","income",c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"),percent=T,color=quintileColor,fill=quintileFill)
  c12 = grouped_bar(countrydat, "wasting_percent","income",c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"),percent=T,fill=quintileFill,legend=T)
  c13 = grouped_line(countrydat, "overweight_percent","income",c("Poorest","Second poorest","Middle","Second wealthiest","Wealthiest"),percent=T,color=quintileColor,fill=quintileFill)
  c14 = grouped_line(countrydat, "stunting_percent","location",c("Urban","Rural"),percent=T)
  c15 = grouped_bar(countrydat, "wasting_percent","location",c("Urban","Rural"),percent=T,legend=T)
  c16 = grouped_line(countrydat, "overweight_percent","location",c("Urban","Rural"),percent=T)
  
  # Chart 17
  wasting = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Wasting alone")$value)
  stunting = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Stunting alone")$value)
  overweight = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Overweight alone")$value)
  wasting_and_stunting = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Wasting and stunting")$value)
  stunting_and_overweight = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Stunting and overweight")$value)
  all = 1
  
  free = as.numeric(subset(countrydat, indicator=="coexistence" & disagg.value=="Not wasting, stunting, or overweight")$value)

  combinations = c(
    A=0
    ,B=0
    ,C=0
    ,D=all
    ,"A&B"=0
    ,"A&C"=0
    ,"A&D"=wasting
    ,"B&C"=0
    ,"B&D"=stunting
    ,"C&D"=overweight
    ,"A&B&C"=0
    ,"A&B&D"=wasting_and_stunting
    ,"A&C&D"=0
    ,"B&C&D"=stunting_and_overweight
    ,"A&B&C&D"=0
  )
  
  label.vals = combinations
  label.vals["D"] = free
  
  label.text = percent(label.vals)
  
  c17 = tryCatch({euler(combinations,shape="ellipse")},error=function(e){no.data})
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
  #Have c3a, c3b, and c3c
  if(!c3a.missing && !c3b.missing && !c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c3a and c3b only
  if(!c3a.missing && !c3b.missing && c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c3a and c3c only
  if(!c3a.missing && c3b.missing && !c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #Have c3b and c3c only
  if(c3a.missing && !c3b.missing && !c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
  }
  #have c3a only
  if(!c3a.missing && c3b.missing && c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c3a)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have c3b only
  if(c3a.missing && !c3b.missing && c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c3b)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #have c3c only
  if(c3a.missing && c3b.missing && !c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="white")
    tryCatch({print(c3c)},error=function(e){message(e);print(no.data)})
    dev.off()
  }
  #Have none of c3s
  if(c3a.missing && c3b.missing && c3c.missing){
    Cairo(file="c3a.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3b.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3c.png",width=300,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3d.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3e.png",width=450,height=600,units="px",bg="transparent")
    print(cblank)
    dev.off()
    Cairo(file="c3.png",width=900,height=600,units="px",bg="transparent")
    print(no.data)
    dev.off()
  }
  Cairo(file="c4.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c4)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c5.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c5)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c6.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c6)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c7.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c7)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c8.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c8)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c9.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c9)},error=function(e){message(e);print(no.data)})
  dev.off()
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
  Cairo(file="c14.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c14)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c15.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c15)},error=function(e){message(e);print(no.data)})
  dev.off()
  Cairo(file="c16.png",width=800,height=700,units="px",bg="white")
  tryCatch({print(c16)},error=function(e){message(e);print(no.data)})
  dev.off()
  png(filename="c17.png",width=600,height=600,units="px",bg="white",type="cairo")
  print(plot(c17,
       legend = list(labels=c("Wasted","Stunting","Overweight","Free from"),font="arial",fontsize=20)
       # ,edges = list(col=blue)
       ,quantities = list(labels=label.text,fontsize=15)
       ,fills=list(fill=c(yellow,orange,light.blue,grey),alpha=1)
  ))
  dev.off()
}
####End loop####
