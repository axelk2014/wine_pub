# analysis-functions.R script for wineconsumption analysis
# forked from WineConsumption.R
# created 4/11/17

library(dplyr)
library(ggplot2)
library(quantmod)
library(reshape)
library(scales)
library(gridExtra)
library(ggthemes)
library(ggpubr)


a1 <- Sys.time()
setwd("/Users/alex/Documents/datasets/wine")
source("src/functions.R")

setwd("/Users/alex/Documents/datasets/wine")
source("src/data-clean.R")

a2 <- Sys.time()
print("sourcing data-clean.R and functions.R complete")
print(a2-a1)

# customise theme to see Y axis titles
theme_wsj.custom <- theme_set(theme_wsj()) %+replace% theme(axis.title = element_text(colour = "black",size=10, family="Arial")) 


# chart 1
# customise data labels
loessFit <- loess(df$Total.Wine.per.Resident.litres~df$Year, df, span = 0.6)
loessFit <- data.frame(x=loessFit$x,y=loessFit$fitted)
# create dataframe for labels
lbl.df <- df[abs(df$Total.Wine.per.Resident.litres - loessFit$y) > 1.03,]
lbl.df <- lbl.df[!duplicated(lbl.df$Total.Wine.per.Resident.litres),]
lbl.df <- rbind(lbl.df,df[which.max(df$Total.Wine.per.Resident.litres),])

plot.title = "Consumption of Wine per Resident - US (in litres)"
plot.subtitle = "source: https://www.wineinstitute.org/resources/statistics/article86"

p1 <- ggplot(data=df, aes(x=Year, y=Total.Wine.per.Resident.litres)) + geom_bar(stat="identity",fill="red4") +
  annotate(geom="text", x=10,y=11.5, label="Total.Wine.per.Resident",size=4,fontface="bold") +
  geom_text(data=lbl.df, 
            aes(x=Year,y=Total.Wine.per.Resident.litres, label=round(Total.Wine.per.Resident.litres,2)), vjust=ifelse(lbl.df$Year == "1993",-0.5,-0.5), hjust=ifelse(lbl.df$Year == "2016",0.7,0.5), size=3, family="Arial Narrow",colour="black") + 
  scale_y_continuous(expand = c(0,0)) + expand_limits(y=max(df$Total.Wine.per.Resident.litres)+1) +
  scale_x_discrete(breaks=c(1934,1940,1946,1960,1980,1986,1993,2000,2016)) +
  theme_wsj.custom +   
  labs(x="",y="(in litres)") 

# chart 2
# customise data labels
loessFit.gdp <- loess(GDP.per.capita.USD~Year, df.gdp.us, span = 0.6)
loessFit.gdp <- data.frame(x=loessFit.gdp$x,y=loessFit.gdp$fitted)
gdp.insert <- as.data.frame(cbind(Year=seq(1934,length.out = 26),y=NA))
loessFit.gdp <- rbind(gdp.insert,loessFit.gdp)

# create dataframe for labels
lbl.df <- df.gdp.us[abs(df.gdp.us$GDP.per.capita.USD - loessFit.gdp$y) > 2300,]
lbl.df <- lbl.df[!duplicated(lbl.df$GDP.per.capita.USD),]
lbl.df <- rbind(na.omit(lbl.df),df.gdp.us[which.max(df.gdp.us$GDP.per.capita.USD),])

plot.title = "US GDP per capita"
plot.subtitle = "source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD"

p2 <- ggplot(data=df.gdp.us, aes(x=Year, y=GDP.per.capita.USD)) + geom_line(group=1, colour="red4") +
  scale_y_continuous(expand = c(0.05,0), breaks=c(0,20000,40000,60000), label=c(0,20,40,60)) + expand_limits(y=max(df.gdp.us$GDP.per.capita.USD,na.rm=TRUE)+10000) +
  scale_x_discrete(breaks=c(1934,1940,1960,1980,2000,2008,2016)) +
  theme_wsj.custom  +
  geom_text(data=na.omit(lbl.df), 
            aes(x=Year,y=GDP.per.capita.USD, label=dollar(round(GDP.per.capita.USD,2))), vjust=-1, hjust=ifelse(na.omit(lbl.df$Year)==2016, 1,0),size=3, family="Arial Narrow") + 
  annotate(geom="text", x=10,y=67000, label="GDP.per.capita.USD",size=4,fontface="bold") +
  labs(y="(in $1,000)",x="") 

# chart 3
lbl.df <- rbind(df.pop.us[which.min(df.pop.us$US.Population.Total),],df.pop.us[which.max(df.pop.us$US.Population.Total),])

plot.title = "US Total Population"
plot.subtitle = "source: https://data.worldbank.org/indicator/SP.POP.TOTL"

p3 <- ggplot(data=df.pop.us, aes(x=Year, y=US.Population.Total)) + geom_line(group=1,colour="red4") +
  scale_y_continuous(limits=c(0,360000000),expand = c(0,0), breaks=c(0,100000000,200000000,300000000), label=c(0,10,20,30)) + expand_limits(y=max(na.omit(df.pop.us$US.Population.Total))+50000000) +
  scale_x_discrete(breaks=c(1934,1940,1960,1980,2000,2016)) +
  theme_wsj.custom + 
  geom_text(data=lbl.df, 
            aes(x=Year,y=US.Population.Total, label=comma(US.Population.Total)), vjust=ifelse(lbl.df$Year == 1960,0,-0.5), hjust=ifelse(lbl.df$Year == 1960,1.1,1),size=3, family="Arial Narrow") + 
  annotate(geom="text", x=10,y=320000000, label="US.Population.Total",size=4,fontface="bold") +
  labs(y="(in 10,000,000)") 

#grid.arrange(p1, p2, p3,ncol=1)

ggarrange(p1,p2,p3,
          #labels = c( "GDP.per.capita.USD", "US.Population.Total "),,
         heights = c(1.5,0.9,1.2),
         ncol = 1, nrow = 3, align = "v")
