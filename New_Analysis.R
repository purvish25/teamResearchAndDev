library(readr) # to load the csv file
library(tidyverse)
## This part of the program loads the kaggle suicide dataset and cleaning.

## Import Suicide Dataset CSV
suicide_dataset <- read.csv("suicide_dataset.csv") ## 27820 observations
raw_data <- suicide_dataset
## Ignore year 2016 as there are not enough observations.Not all countries have
## suicides reported for the year 2016
## Count rows matching year 2016. 160 rows will be removed as part of this step
### View (df %>%
### filter(year == 2016) %>% 
### summarise(cnt = n()))
suicide_dataset <- suicide_dataset %>%
  filter(year != 2016) ## 27660 observation

## Remove any country with less than 30 years of data. This will result in
## 11076 observations and a total of 30 countries with more than 30 years of data.

suicide_dataset<- suicide_dataset %>%
  group_by(country) %>%
  add_tally() %>%
  filter(n >= 360)

## Rename suicide_dataset columns
names(suicide_dataset)[7] <- "SuicidePer100k"
names(suicide_dataset)[8] <- "CountryYear"
names(suicide_dataset)[9] <- "HDI4Year"
names(suicide_dataset)[10] <- "GDP4Year"
names(suicide_dataset)[11] <- "GDPPerCapita"

## Update 5-14 years data with 05-14 years as it causes issues in plotting
suicide_dataset$age[suicide_dataset$age == "5-14 years"] <- "05-14 years"
suicide_dataset$sex[suicide_dataset$sex == "male"] <- "Male"
suicide_dataset$sex[suicide_dataset$sex == "female"] <- "Female"
#View(suicide_dataset)

## Remove GDP and HDI columns as HDI has very few value and we are not interested
## in GDP data
suicide_dataset <- suicide_dataset %>% select(-HDI4Year, 
                                              -GDP4Year, 
                                              -GDPPerCapita, 
                                              -generation,
                                              -n) #this column is added as part of filtering number of years of data

## Calculate global average number of suicides considering countries with  
## 30 years of data  
global_average <- (sum(as.numeric(suicide_dataset$suicides_no)) / 
                     sum(as.numeric(suicide_dataset$population))) * 100000

## Calculate average suicides in Male
suicide_dataset_male <- filter(suicide_dataset, sex == "Male") # 5538 observations
global_average_male <- (sum(as.numeric(suicide_dataset_male$suicides_no)) / 
                          sum(as.numeric(suicide_dataset_male$population))) * 100000

## Calculate average suicides in Female
suicide_dataset_female <- filter(suicide_dataset, sex == "Female") # 5538 observations
global_average_female <- (sum(as.numeric(suicide_dataset_female$suicides_no)) / 
                            sum(as.numeric(suicide_dataset_female$population))) * 100000

## What is the population for a country by year? This question needs to be answered
## as the popoulation column original dataset is for country-year-sex-age combination
population_by_country <- suicide_dataset %>%
  group_by(country, year) %>%
  summarize(population = sum(as.numeric(population)))

#combined_data <- with(suicide_dataset, aggregate(list(population=population), list(x = tolower(sex)), sum))
#dftest <- aggregate(cbind(suicide_dataset$SuicidePer100k) ~ suicide_dataset$age+suicide_dataset$country, FUN=sum)
datafor30country <- aggregate(cbind(suicide_dataset$suicides_no,suicide_dataset$population) ~ suicide_dataset$age
                              , FUN=sum)
colnames(datafor30country)<-c("age_group","total_suicides","total_population")
datafor30country$per100k <- ((as.numeric(as.character(datafor30country$total_suicides)))*100000)/ as.numeric(as.character(datafor30country$total_population))
datafor30country$hk <- ((as.numeric(as.character(datafor30country$total_population)))/100000)

write.csv(datafor30country,"datafor30country.csv", row.names = TRUE)

#main_data <- read.csv("main_data.csv")
#colnames(main_data)<-c("","05-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years")

#main_matrix <- data.matrix(main_data)
temp <- prop.test(datafor30country$total_suicides,datafor30country$total_population)

fulldata <- aggregate(cbind(raw_data$suicides_no,raw_data$population) ~ raw_data$age, FUN=sum)
colnames(fulldata)<-c("age_group","total_suicides","total_population")
fulldata$per100k <- ((as.numeric(as.character(fulldata$total_suicides)))*100000)/ as.numeric(as.character(fulldata$total_population))
fulldata$hk <- ((as.numeric(as.character(fulldata$total_population)))/100000)

write.csv(fulldata,"fulldata.csv", row.names = TRUE)
prop.test(fulldata$total_suicides,fulldata$total_population)

#chisq.test(dftest$total_suicides)
#main_matrix <- main_matrix[,-1] # delete column 2

