library(tidyverse)
library(lubridate)
library(scales)
library(ggpubr)
library(ggpmisc)

dat <- read.csv("WHO-COVID-19-global-data.csv")
dat <- rename(dat, Date_reported = ï..Date_reported)
head(dat)
head(tab)

pop <- read_csv("population_edit.csv")
pop <- pop %>% select('Country Name', 'Country Code', '2020')
pop <- pop %>% rename(countryName = 'Country Name', code = 'Country Code', population = '2020')
pop

head(pop)

newdat <- dat %>%
  mutate(Date_reported = as.Date(strptime(as.character(Date_reported), "%m/%d/%Y"))) %>%
  left_join(tab, by = c(Country_code = "code_2")) %>%
  left_join(pop, by = c("code_3" = "code")) %>%
  filter(!is.na(population))   %>%#remove countries that were small
  select(-Country.y, -Numeric, -countryName, -code_3) %>%
  rename(Country = Country.x) %>%
  mutate(cases_percent = Cumulative_cases/population, 
         deaths_per_100k = Cumulative_deaths/population*100000,
         new_cases_per_100k = (New_cases/population)*100000,
         new_deaths_per_100k = (New_cases/population)*100000)
  
head(newdat)



major_countries <- newdat %>%
  filter(Country_code %in% c("US", "CN", 
                        "GB", "RU", "DE",
                        "PH", "NZ")) 

#percent of cases in the population throughout time from major countries
major_countries%>%
  ggplot(aes(Date_reported, cases_percent, group = Country, color = Country)) +
  geom_line(size = 2)
#percent of deaths in the population throughout time from major countries
major_countries%>%
  ggplot(aes(Date_reported, deaths_per_100k, group = Country, color = Country)) +
  geom_line(size = 2)



#average of cases in the population of countries throughout time
cases_avg <- newdat %>% 
  group_by(Date_reported) %>%
  summarize(avg_cases_percent = mean(cases_percent))

cases_avg %>%
  ggplot(aes(Date_reported, avg_cases_percent)) +
  geom_line()

#average of deaths in the population of countries throughout time
deaths_avg <- newdat %>% 
  group_by(Date_reported) %>%
  summarize(avg_deaths_per_100k = mean(deaths_per_100k))

deaths_avg %>%
  ggplot(aes(Date_reported, avg_deaths_per_100k)) +
  geom_line()


#cases in countries throughout time
newdat %>%
  ggplot(aes(Date_reported, cases_percent, group = Country)) +
  geom_line()

#deaths in countries throughout time
newdat %>%
  ggplot(aes(Date_reported, deaths_per_100k, group = Country)) +
  geom_line()

#Visualization
fig_a <- cases_avg %>%
  ggplot(aes(Date_reported, avg_cases_percent, color = "Average")) +
  geom_line(size = 1) +
  geom_line(data = major_countries, aes(Date_reported, cases_percent, group = Country, color = Country),
            size = 1) +
  geom_line(data= newdat, aes(Date_reported, cases_percent, group = Country), alpha = 0.2, color = "gray") +
  xlab("Date Reported") +
  ylab("Percent of cases in the population") +
  geom_text(data = major_countries, mapping = aes(x = as.Date("2021-07-01"), y = 0.018, label = "Philippines"),
            show.legend = F, color = "black", size = 3) + 
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  ggtitle("Growth of Covid-19 Cases") +
  theme_minimal() + 
  theme(legend.title = element_blank(), aspect.ratio = 7/19) +
  coord_cartesian(xlim = c(as.Date("2020-04-01"), as.Date("2021-10-21")))
ggsave(plot = fig_a, filename = "fig_a.png")
#Conclusion: An initial look at the graph shows that Philippines does not look to have many cases as compared
#to other countries. As it looks to be below average. However, further research showed that the Philippines
#actually has one of the lowest testing rates across all countries ranking at 144 according to www.worldometers.info/coronavirus/

fig_b <- deaths_avg %>% 
  ggplot(aes(Date_reported, avg_deaths_per_100k, color = "Average")) +
  geom_line(size = 1) + 
  geom_line(data = major_countries, aes(Date_reported, deaths_per_100k, group = Country, color = Country), 
                                        size = 1) +
  geom_line(data= newdat %>% filter(Country != "Peru"), aes(Date_reported, deaths_per_100k, group = Country), alpha = 0.2, color = "gray") +
  xlab("Date Reported") +
  ylab("Deaths per 100k people") +
  geom_text(data = major_countries, mapping = aes(x = as.Date("2021-07-01"), y = 38, label = "Philippines"),
            show.legend = F, color = "black", size = 3) + 
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  ggtitle("Growth of Covid-19 Deaths") +
  theme_minimal() + 
  theme(legend.title = element_blank(), aspect.ratio = 7/19) +
  coord_cartesian(xlim = c(as.Date("2020-04-01"), as.Date("2021-10-21")))
ggsave(plot = fig_b, filename = "fig_b.png")

fig_ab <- ggarrange(fig_a, fig_b, nrow = 2, ncol = 1, common.legend = T, legend = "bottom")
ggsave(plot = fig_ab, filename = "fig_ab.png")
?ggsave
?ggarrange
#ranking of Philippines in number of cases
tab_a <- newdat %>% 
  filter(Date_reported == "2021-10-21") %>%
  arrange(desc(cases_percent)) %>%
  mutate(rank = rank(desc(cases_percent))) %>%
  filter(Country == "Philippines") %>%
  select(Date_reported, Country, Cumulative_cases, cases_percent, rank)

#ranking of Philippines in number of deaths
newdat %>% 
  filter(Date_reported == "2021-10-21") %>%
  arrange(desc(deaths_per_100k)) %>%
  mutate(rank = rank(desc(deaths_per_100k))) %>%
  filter(Country == "Philippines") %>%
  select(Date_reported, Country, Cumulative_deaths, deaths_per_100k, rank)


#Redo visualizations but divide cases and deaths by testing rate per country to take it into account
#Here we assume the average testing rate of a country as constant throughout time
#We also assume that the distribution of cases/deaths in the tested individuals are the same for untested individuals
newdat_testrate <- newdat %>%
  left_join(testDat, by = c(Country_code = "codes")) %>%
  select(-Country.y) %>%
  rename(Country = Country.x) %>% 
  mutate(cases_per_test = cases_percent/testing_rate,
         deaths_per_test = (deaths_per_100k/100000)/testing_rate,
         mort_per_case = deaths_per_test/cases_per_test)


major_countries_test <- newdat_testrate %>%
  filter(Country_code %in% c("US", "CN", 
                             "GB", "RU", "DE",
                             "PH", "NZ")) 

#percent of cases in the population throughout time from major countries
major_countries_test%>%
  ggplot(aes(Date_reported, cases_per_test, group = Country, color = Country)) +
  geom_line(size = 2) +
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#percent of deaths in the population throughout time from major countries
major_countries_test%>%
  ggplot(aes(Date_reported, deaths_per_test, group = Country, color = Country)) +
  geom_line(size = 2) +
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#average of cases in the population of countries throughout time
casestest_avg <- newdat_testrate %>% 
  filter(!is.na(cases_per_test)) %>%
  group_by(Date_reported) %>%
  summarize(avg_cases_per_test = mean(cases_per_test))

casestest_avg %>%
  ggplot(aes(Date_reported, avg_cases_per_test)) +
  geom_line()

#average of deaths in the population of countries throughout time
deathstest_avg <- newdat_testrate %>% 
  filter(!is.na(cases_per_test)) %>%
  group_by(Date_reported) %>%
  summarize(avg_deaths_per_test = mean(deaths_per_test), 
            avg_cases_per_test = mean(cases_per_test),
            avg_mort_per_case = mean(mort_per_case))

deathstest_avg %>%
  ggplot(aes(Date_reported, avg_deaths_per_test)) +
  geom_line()

#Visualization
fig_c <- casestest_avg %>%
  ggplot(aes(Date_reported, avg_cases_per_test, color = "Average")) +
  geom_line(size = 1) +
  geom_line(data = major_countries_test, aes(Date_reported, cases_per_test, group = Country, color = Country),
            size = 1) + 
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  xlab("Date Reported") +
  ylab("Positive Rate per Test") +
  geom_text(data = major_countries, mapping = aes(x = as.Date("2021-09-01"), y = 0.08, label = "Philippines"),
            show.legend = F, color = "black", size = 3) + 
  ggtitle("Positive Rate of Covid-19 Tests") +
  theme_minimal() + 
  theme(legend.title = element_blank(), aspect.ratio = 7/19) 

fig_d <- deathstest_avg %>% 
  ggplot(aes(Date_reported, avg_deaths_per_test, color = "Average")) +
  geom_line(size = 1) + 
  geom_line(data = major_countries_test, aes(Date_reported, deaths_per_test, group = Country, color = Country), 
            size = 1) +
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  xlab("Date Reported") +
  ylab("Mortality Rate per Test") +
  geom_text(data = major_countries, mapping = aes(x = as.Date("2021-09-01"), y = 0.0018, label = "Philippines"),
            show.legend = F, color = "black", size = 3) + 
  ggtitle("Growth of Covid-19 Deaths") +
  theme_minimal() + 
  theme(legend.title = element_blank(), aspect.ratio = 7/19) 

fig_e <- deathstest_avg %>% 
  filter(Date_reported > "2020-05-01") %>%
  ggplot(aes(Date_reported, avg_mort_per_case, color = "Average")) +
  geom_line(size = 1) + 
  geom_line(data = major_countries_test %>% 
              filter(Date_reported > "2020-05-01"), aes(Date_reported, mort_per_case, group = Country, color = Country), 
            size = 1) +
  scale_x_date(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  xlab("Date Reported") +
  ylab("Mortality Rate per Test") +
  geom_text(data = major_countries, mapping = aes(x = as.Date("2021-09-01"), y = 0.0018, label = "Philippines"),
            show.legend = F, color = "black", size = 3) + 
  ggtitle("Growth of Covid-19 Deaths") +
  theme_minimal() + 
  theme(legend.title = element_blank(), aspect.ratio = 7/19)

fig_cd <- ggarrange(fig_c, fig_d, nrow = 2, ncol = 1, common.legend = T, legend = "bottom")
ggsave(plot = fig_cd, filename = "fig_cd.png")
#ranking of Philippines in number of cases
newdat_testrate %>% 
  filter(Date_reported == "2021-10-21") %>%
  arrange(desc(cases_per_test)) %>%
  mutate(rank = rank(desc(cases_per_test))) %>%
  filter(Country == "Philippines") %>%
  select(Date_reported, Country, Cumulative_cases, cases_per_test, rank)

#ranking of Philippines in number of deaths
newdat_testrate %>% 
  filter(Date_reported == "2021-10-21") %>%
  arrange(desc(deaths_per_test)) %>%
  mutate(rank = rank(desc(deaths_per_test))) %>%
  filter(Country == "Philippines") %>%
  select(Date_reported, Country, Cumulative_deaths, deaths_per_test, rank)

#rank is in 195 countries. We could not consider countries that we could not get testing rates from

  