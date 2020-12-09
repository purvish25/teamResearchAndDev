# Author: Rahul Soni
# Email: rs20acs@herts.ac.uk
# Course: M.Sc. Artificial Intelligence & Robotics
# University of Hertfordshire

# References:  
# [1] https://github.com/justmarkham/dplyr-tutorial/blob/master/dplyr-tutorial-2.Rmd#L141

# Load libraries
library(readr) # to load the csv file
library(ggplot2)

# This part of the program loads the kaggle suicide dataset and add the region
# and continent based on the country column. 
# This is done using countrycode library that returns continent or region value
# for a given country.
# Further details of the library can be found online on the following link
# https://cran.r-project.org/web/packages/countrycode/countrycode.pdf
suicide_Dataframe <- function(read_path) {
  # Export Suicide Dataset CSV
  suicide_dataset <- read.csv(read_path)
  
  # Add continent column to the suicide dataframe for a given country
  # This adds continents based on Continent as defined in the World Bank 
  # Development Indicators

  
  # Add region column to the suicide dataframe for a given country
  # This adds regions based on 7 Regions as defined in the World Bank 
  # Development Indicators
  # Sometimes continents don't provide enough groups for you to delineate the 
  # data. For example, continents groups North and South America into Americas


  
  # Ignore year 2016 as there are not enough observations.
  # Count rows matching year 2016. 160 rows will be removed as part of this step
  #View (df %>%
  #  filter(year == 2016) %>% 
  #  summarise(cnt = n()))
  suicide_dataset <- suicide_dataset %>%
    filter(year != 2016) 
  #View (suicide_dataset)
  
  # Remove GDP and HDI columns as HDI has very few value and we are not interested
  # in GDP data
 # suicide_dataset <- suicide_dataset %>% select(-HDI4Year, -GDP4Year, -GDPPerCapita, -generation)
  #View (df1)
  
  #Remove any country with less than 10 years of data
  suicides_df<- suicides %>%
    group_by(country) %>%
    add_tally() %>%
    filter(n >= 360)
  
  return (suicide_dataset)
}