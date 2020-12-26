source("visualization.R")
# Group by Age,Sex to 
df_age_sex <- suicide_dataset %>%
  group_by(age, sex) %>% 
  summarize(suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population))) 

df_age_sex <- df_age_sex %>% 
  count(age, sex, suicides) %>%
  spread(key = age,value = suicides) 

df_age_sex <- df_age_sex %>% select(-n)
View(df_age_sex)
################################################################################
pop_sex <- suicide_dataset %>%
  group_by(sex) %>%
  summarize(suicides = sum(as.numeric(suicides_no))) 

data <- merge(df_age_sex, pop_sex, by  = "sex")

data <- data %>% 
  rename(suicides = Total_Suicides)
names(data)[names(data) == "suicides"] <- "Total_Suicides"

View(data)
################################################################################
pop_age <- suicide_dataset %>%
  group_by(age) %>%
  summarize(suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population))) 

pop_age <- pop_age %>% 
  count(age, suicides) %>%
  spread(key = age,value = suicides) 

pop_age <- pop_age %>% select(-n)

View(pop_age)
################################################################################
data = bind_rows(data, pop_age)
data[3,1] = "Total_Suicides"
data[3,8] = sum(data[1,8] + data[2,8])
View(data)
