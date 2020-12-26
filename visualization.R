## Author: Team 49
## Members: rs20acs@herts.ac.uk, pj18aan@herts.ac.uk, vs18abb@herts.ac.uk, dr18abc@herts.ac.uk, zm20abu@herts.ac.uk
## University of Hertfordshire

## References:  
## [1] https://github.com/justmarkham/dplyr-tutorial/blob/master/dplyr-tutorial-2.Rmd#L141

## Load libraries
library(readr) # to load the csv file
library(tidyverse)
## This part of the program loads the kaggle suicide dataset and cleaning.

## Import Suicide Dataset CSV
suicide_dataset <- read.csv("suicide_dataset.csv") ## 27820 observations

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


############################################################
#Task 1 - Suicide rate per 100k over all age groups
##########################################################

## STEP-1: Calculate global average number of suicides per 100k for all age groups with 30 years of data
suicide_by_age <- suicide_dataset %>%
  group_by(age) %>%
  summarize(n = n(), 
            suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000) %>%
  arrange(suicide_per_100k)

## STEP-2: Rearranging the x axis by ascending order.
suicide_by_age$age <- factor(suicide_by_age$age,
                             levels = rev(suicide_by_age$age[order(-suicide_by_age$suicide_per_100k)]))

## STEP-3: Results (Graph) to show the Suicide rate per 100k over all age groups using bar plot.......
task1 <- ggplot(suicide_by_age, aes(x=age, y=suicide_per_100k, fill = age)) + 
  geom_bar(stat="identity")+ 
  theme_minimal() + 
  theme(axis.text=element_text(size=6)) +
  labs(title="Suicides Per 100k (1985 - 2015)", 
       subtitle = "Age groups for all Countries with 30 years of data",
       x = "Age Groups", 
       y = "Suicides Per 100k") + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

########################################################################################
#Task 2 - Suicide rate/100k over all genders, age groups (Stacked graph to show proportion)
######################################################################################################

## STEP-1: Calculate global average number of suicides per 100k for all gender, age groups with 30 years of data
df_agegroup_gender <- suicide_dataset %>%
  group_by(age, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

## STEP-2: Results (Graph) to show the Suicide rate per 100k over all genders, age groups using Stacked graph....
task2 <- ggplot(df_agegroup_gender, aes(y = suicide_per_100k, x = age, fill = sex, label = round(suicide_per_100k, digits = 1))) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportions of suicides (1985 - 2015)",
       subtitle = "All age groups by gender", 
       x = "Age Groups", 
       y = "Suicides Per 100k",
       fill = "Sex") + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")

################################################################################################################
#Task 3 - Suicide rate/100k over all genders, age groups (Stacked graph to show proportion by absolute numbers)
################################################################################################################

## STEP-1: Results (Graph) to show the Suicide rate per 100k over all genders, age groups using Stacked graph....
task3 <- ggplot(df_agegroup_gender, mapping = aes(x = age, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity") +
  labs(title="Suicides Per 100k (1985 - 2015)",
       subtitle = "All age groups by gender",
       x = "Age Groups", 
       y = "Suicides Per 100k",
       fill = "Sex") + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom")

################################################################################
## Task 5 - Proportions of suicides that are Male & Female, by Country 
################################################################################
## Dataframe for all female demographic
female_age_group_country <- suicide_dataset %>%
  filter(sex == "Female") %>%
  group_by(country, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) 

## Dataframe for all male demographic
male_age_group_country <- suicide_dataset %>%
  filter(sex == "Male") %>%
  group_by(country, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

## Trend plot by all age groups, female 
ggplot(female_age_group_country, aes(x = country, y = suicide_per_100k, col = age, group = 1)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Suicides Per 100k by Country",
       subtitle = "All Age Groups, Female",
       x = "Country", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Trend plot by all age groups, female 
ggplot(male_age_group_country, aes(x = country, y = suicide_per_100k, col = age, group = 1)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Suicides Per 100k by Country",
       subtitle = "All Age Groups, Male",
       x = "Country", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################
## Task 6 - Suicides rates/100k trends for all age groups every 5 years
################################################################################
top_20percent_suicides <- suicide_dataset %>%
  mutate(suicides_per_100k = suicides_no * 100000 / population) %>%
  arrange(desc(suicides_per_100k)) %>%
  head(n = round(nrow(.) * 20 / 100))

top_20percent_suicides$time <- ifelse(top_20percent_suicides$year <= 1990, "1985 - 1990", 
                                      ifelse(top_20percent_suicides$year <= 1995, "1991 - 1995", 
                                             ifelse(top_20percent_suicides$year <= 2000, "1996 - 2000",
                                                    ifelse(top_20percent_suicides$year <= 2005, "2001 - 2005",
                                                           ifelse(top_20percent_suicides$year <= 2010, "2006 - 2010",
                                                                  "2011 - 2015")))))

task6 <- ggplot(top_20percent_suicides, aes(x = age, fill = sex)) + 
  geom_bar() + 
  labs(title = "20% most significant suicides/100k", 
       subtitle = "Volumes every 5 years, Age & Sex",
       x = "Age", 
       y = "Number of Demographics", 
       fill = "Sex") + 
  facet_wrap(~ time) + 
  scale_y_continuous(breaks = seq(0, 300, 25)) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################
## Task 7 - Plot Suicide rate/100k "Trend" for each sex and age group
################################################################################
## Dataframe for all female demographic
female_age_group_trend <- suicide_dataset %>%
  filter(sex == "Female") %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) 

## Dataframe for all male demographic
male_age_group_trend <- suicide_dataset %>%
  filter(sex == "Male") %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) 

## Dataframe for all demographics
age_sex_trend <- suicide_dataset %>%
  group_by(year, age, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) 

## Trend plot by all age groups, female
task7a <- ggplot(female_age_group_trend, aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time",
       subtitle = "All Age Groups, Female",
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2), minor_breaks = F) 

## Trend plot by all age groups, male
task7b <- ggplot(male_age_group_trend, aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time",
       subtitle = "All Age Groups, Male",
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2), minor_breaks = F) 

## Trend plot by all age groups, genders (on same scale)
task7c <- ggplot(age_sex_trend, aes(x = year, y = suicide_per_100k, col = age)) + 
  #facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time",
       subtitle = "All Age Groups & Sex (Same Scale)",
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2), minor_breaks = F) +
  facet_grid(age~sex) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################################################
#Task 4 - Collecting all the plots and putting then in a visualization.pdf file
################################################################################################################

# pdf("visualization.pdf")
# 
# print(task1)
# print(task2)
# print(task3)
# print(task6)
# print(task7a)
# print(task7b)
# print(task7c)
# dev.off()
