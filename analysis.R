source("visualization.R")

# Find how many countries have reported data for every year
records_per_year <- suicide_dataset %>%
                    group_by(year) %>%
                    summarise(cnt = n())

################################################################################
## Task 1 - Suicides rates/100k trends over the years
################################################################################
suicide_dataset %>%
  group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000) %>%

  
  ggplot(aes(x = year, y = suicides_per_100k)) + 
  geom_line(col = "#0072B2", size = 1) + 
  geom_point(col = "#0072B2", size = 2) + 
  geom_hline(yintercept = global_average, linetype = 2, color = "#000000", size = 1) +
  labs(title = "Global Suicides (per 100k)",
       subtitle = "Trend over time, 1985 - 2015.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) 

################################################################################
## Task 2 - Suicides rates/100k trends for all age groups every 5 years
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

ggplot(top_20percent_suicides, aes(x = age, fill = sex)) + 
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
## Task 3 - Plot Suicide rate/100k "Trend" for each age group
################################################################################
age_group_trend <- suicide_dataset %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) 

ggplot(age_group_trend, aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Age", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)  

################################################################################
## Task 5 - Plot Suicide rate/100k "Trend" for top 5 worst countries for every 
## age group
################################################################################
suicide_age_country <- suicide_dataset %>%
  group_by(country, age) %>%
  mutate(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(desc(suicide_per_100k)) %>% 
  head(n = round(nrow(.) * 20 / 100))

View(suicide_age_country)
