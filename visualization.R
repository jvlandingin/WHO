library(tidyverse)
library(ggplot2)
library(ggthemes)
library(grid)
library(lubridate)
library(scales)

dat <- read.csv("WHO-COVID-19-global-data.csv")
dat <- rename(dat, Date_reported = ï..Date_reported)
head(dat)

#Total number of cases and deaths since start of 2020 to today (October 21, 2021)
overall <- dat %>% 
  mutate(Date_reported = strptime(as.character(Date_reported), "%m/%d/%Y")) %>%
  filter(Date_reported == "2021-10-21") %>%
  summarize(total_cases_mil = sum(Cumulative_cases)*0.000001, 
            total_deaths_mil = sum(Cumulative_deaths*0.000001),
            death_percentage = round(sum(Cumulative_deaths)/sum(Cumulative_cases),4)*100)
overall

#line graph of the growth of cases and deaths in the world
growth_world <- dat %>%
  mutate(Date_reported = as.Date(Date_reported)) %>%
  group_by(Date_reported) %>%
  summarize(cases = sum(Cumulative_cases)*0.000001,
            deaths = sum(Cumulative_deaths)*0.000001) %>%
  ungroup() %>%
  gather(-Date_reported, key = categ, value = count) %>% 
  arrange(Date_reported) %>%
  ggplot(aes(Date_reported, count, group = categ, color = categ)) +
  geom_line(size = 1) + 
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y"))
growth_world

#line graph of the growth of deaths in the world
deaths_world <- dat %>%
  mutate(Date_reported = as.Date(Date_reported)) %>%
  group_by(Date_reported) %>%
  summarize(cases = sum(Cumulative_cases)*0.000001,
            deaths = sum(Cumulative_deaths)*0.000001) %>%
  ungroup() %>%
  ggplot(aes(Date_reported, deaths)) +
  geom_line(size = 1) + 
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  ylab("deaths (in millions)")
deaths_world

#Growth of covid cases/deaths in the philippines compared to other countries
#get population sizes
pop <- read_csv("population_edit.csv")
pop <- pop %>% select('Country Name', 'Country Code', '2020')
pop <- pop %>% rename(countryName = 'Country Name', code = 'Country Code', population = '2020')
pop

ph_cases <- dat %>%
  filter(WHO_region == "WPRO") %>%
  left_join(tab, by = c(Country_code = "Alpha-2 code")) %>%
  rename(code = "Alpha-3 code") %>%
  left_join(pop, by = c("code" = "code")) %>%
  rename(country = Country.x) %>%
  select(Date_reported, country, Country_code, Cumulative_cases, Cumulative_deaths,population) %>%
  mutate(population = replace_na(population, 15003), 
         cases_percent = Cumulative_cases/population,
         death_percent = Cumulative_deaths/population,
         category = ifelse(str_detect(country, "Philippines"), "Philippines", "Other countries")) %>%
  ggplot(aes(Date_reported, cases_percent, group = country, color = category)) +
  geom_line()
ph_cases

unique(dat$WHO_region)
