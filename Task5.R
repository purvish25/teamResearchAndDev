source("DataClean.R")
library(dplyr)
library(viridis)
library(hrbrthemes)
library(tidyverse)

suicide_dataset <- read.csv("suicide_dataset.csv")
names(suicide_dataset)[1] <- "country"
names(suicide_dataset)[7] <- "SuicidePer100k"
names(suicide_dataset)[8] <- "CountryYear"
names(suicide_dataset)[9] <- "HDI4Year"
names(suicide_dataset)[10] <- "GDP4Year"
names(suicide_dataset)[11] <- "GDPPerCapita"

suicide_dataset <- suicide_dataset %>%
  filter(year != 2016) 

suicides_df<- suicide_dataset %>%
  group_by(country) %>%
  add_tally() %>%
  filter(n >= 360)

dfAvs<-aggregate(suicides_df$SuicidePer100k ~ suicides_df$country, FUN=sum)
pdf("visulization.pdf") 

#dfsum <- aggregate(suicides_df$SuicidePer100k)

colnames(dfAvs)<-c("country","meansuisidesper100k")

barplot(dfAvs$meansuisidesper100k,
        main="Countries with average deaths per 100k",
        ylab = "Average death per 100k",
        xlab = "countries",
        col = c("lightblue", "red"),
        names.arg = c(dfAvs$country),
        las=2
)
dfAvs <- dfAvs[order(dfAvs$meansuisidesper100k),]
df_low5 <- head(dfAvs,5)

dfYears<-aggregate(suicides_df$SuicidePer100k ~ suicides_df$year, FUN=sum)
colnames(dfYears)<-c("year","totalsuicidesper100k")

barplot(dfYears$totalsuicidesper100k,
        main="Countries with average deaths per 100k",
        ylab = "Average death per 100k",
        xlab = "countries",
        col = c("lightblue", "red"),
        names.arg = c(dfYears$year),
        las=2
)

ggplot(dfAvs
       , aes(x=country, y=meansuisidesper100k, fill = country))+ 
  geom_bar(stat="identity")+ theme_minimal()+ 
  theme(axis.text=element_text(size=6))+
  labs(title="Suicides per 100k (1985 - 2015)"
       , subtitle = "Countries with 30 years of data")+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off() 

