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

################################################################################
## Preparing a dataframe by aggregating columns for chi-squared test 
################################################################################

datafor30country <- aggregate(cbind(suicide_dataset$suicides_no,suicide_dataset$population) ~ suicide_dataset$age
                              , FUN=sum)
colnames(datafor30country)<-c("age_group","total_suicides","total_population")

datafor30country$suicide_rate_per100k <- (datafor30country$total_suicides*100000)/ datafor30country$total_population
datafor30country$total_population_in100k <- (datafor30country$total_population/100000)

prop.test(datafor30country$suicide_rate_per100k,datafor30country$total_population_in100k)

################################################################################
## Preparing a raw dataframe by aggregating columns for chi-squared test
################################################################################

fulldata <- aggregate(cbind(raw_data$suicides_no,raw_data$population) ~ raw_data$age, FUN=sum)
colnames(fulldata)<-c("age_group","total_suicides","total_population")

fulldata$suicide_rate_per100k <- (fulldata$total_suicides*100000)/fulldata$total_population
fulldata$total_population_in100k <- (fulldata$total_population/100000)

prop.test(fulldata$suicide_rate_per100k,fulldata$total_population_in100k)


