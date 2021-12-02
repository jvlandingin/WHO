library(tidyverse)
library(lubridate)


dat <- read.csv("WHO-COVID-19-global-data.csv")
dat <- rename(dat, Date_reported = ï..Date_reported)
head(dat)

first_vaccine_dates <- read.csv("owid-covid-data.csv") 

first_vaccine_dates <- first_vaccine_dates %>%
  select(iso_code, location, date, total_vaccinations) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(total_vaccinations)) %>%
  group_by(iso_code) %>%
  summarize(first_vaccine = min(date))

head(first_vaccine_dates)

vacdat <- dat %>%
  mutate(Date_reported = as.Date(strptime(as.character(Date_reported), "%m/%d/%Y"))) %>%
  left_join(tab, by = c(Country_code = "code_2")) %>%
  select(-Country.y, -Numeric) %>%
  rename(Country = Country.x) %>%
  left_join(first_vaccine_dates, by = c(code_3 = "iso_code")) %>%
  mutate(period = as.numeric(trunc.Date(difftime(Date_reported,first_vaccine, units = "weeks")))) %>%
  group_by(Country, period) %>%
  summarize(New_cases = sum(New_cases))
vacdat
  
vacdat %>% 
  ggplot(aes(period, New_cases, color = Country, group = Country)) +
  geom_line() +
  geom_vline(xintercept=0, alpha = 0.4) +
  theme(legend.position = "none")

head(vacdat)
head(newdat_testrate)
library(zoo)
test <- vacdat %>% 
  left_join(newdat_testrate %>% select(Country, testing_rate), by = "Country") %>%
  select(Country, period, New_cases, testing_rate) %>%
  distinct(Country, period, New_cases, testing_rate) %>%
  mutate(previous_cases = rollsumr(New_cases, k = 4, fill = NA) - New_cases, 
         infection_rate = New_cases/previous_cases,
         infection_rate = ifelse(str_detect(infection_rate, "\\d+"), infection_rate, 0),
         infection_rate = replace_na(infection_rate, 0),
         std_infection_rate = ifelse(is.nan((infection_rate - mean(infection_rate))/sd(infection_rate)),
                                     0, (infection_rate - mean(infection_rate))/sd(infection_rate)))
  
data.frame(test)
head(test)
test %>% #filter(period %in% (-10:1)) %>%
  ggplot(aes(period, std_infection_rate, color = Country, group = Country)) +
  geom_line() +
  theme(legend.position = "none") +
  geom_vline(xintercept=0, alpha = 0.4)

#Average infection rate of the Philippines throughout time
fig_f <- test %>%
  filter(period > -70, std_infection_rate < 8 & std_infection_rate > -5) %>%
  ungroup() %>%
  group_by(period) %>%
  summarize(avg_infection_rate = mean(std_infection_rate)) %>%
  ggplot(aes(period, avg_infection_rate, color = "Average")) +
  geom_line(size = 1) +
  geom_vline(xintercept=0, alpha = 1, linetype = 1, color = "blue") +
  geom_text(aes(x = -5, y = 0.5, label = "Vaccination Date", angle = 90), show.legend = F, color = "blue") +
  xlab("Period(Weeks)") +
  ylab("Infection Rate (Standardized)") +
  geom_line(data = test %>% filter(Country == "Philippines", std_infection_rate < 2.5), 
            aes(period, std_infection_rate, color = "Philippines"), size = 1) +
  ggtitle("Infection Rate per Week") + 
  theme_minimal() + 
  theme(legend.title = element_blank())

ggsave(plot = fig_f, filename = "fig_f.png")


#Mortality rate of the Philippines throughout time
mortdat <- dat %>%
  mutate(Date_reported = as.Date(strptime(as.character(Date_reported), "%m/%d/%Y"))) %>%
  left_join(tab, by = c(Country_code = "code_2")) %>%
  left_join(pop, by = c("code_3" = "code")) %>%
  filter(!is.na(population))%>%
  select(-Country.y, -Numeric) %>%
  rename(Country = Country.x) %>%
  left_join(first_vaccine_dates, by = c(code_3 = "iso_code")) %>%
  select(Date_reported, Country, New_cases, New_deaths, population, first_vaccine) %>%
  mutate(period = as.numeric(trunc.Date(difftime(Date_reported,first_vaccine, units = "weeks")))) %>%
  group_by(Country, period) %>%
  summarize(New_cases = sum(New_cases), New_deaths = sum(New_deaths), population = mean(population))
mortdat

z <- mortdat %>%
  mutate(previous_cases = rollsumr(New_cases, k = 4, fill = NA) - New_cases, 
         mortality_rate = (New_deaths/population)*100000,
         mortality_rate = ifelse(str_detect(mortality_rate, "\\d+"), mortality_rate, 0),
         mortality_rate = replace_na(mortality_rate, 0),
         std_mortality_rate = (mortality_rate - mean(mortality_rate))/sd(mortality_rate)) %>%
  ungroup()
view(z)

z %>% 
  ggplot(aes(period, mortality_rate, color = Country, group = Country)) +
  geom_line() +
  theme(legend.position = "none") +
  geom_vline(xintercept=0, alpha = 1, linetype = 1, color = "blue") +
  geom_text(aes(x = -5, y = 10, label = "Vaccination Date", angle = 90), show.legend = F, color = "blue") +
  ylab("Mortality Rate per 100,000 people") +
  xlab("Period (Weeks)")

fig_g <- z %>%
  filter(period > -70, str_detect(std_mortality_rate, "\\d")) %>%
  group_by(period) %>%
  summarize(std_mortality_rate = mean(std_mortality_rate)) %>%
  ggplot(aes(period, std_mortality_rate, color = "Average")) +
  geom_line(size = 1) + 
  geom_vline(xintercept=0, alpha = 1, linetype = 1, color = "blue") +
  geom_text(aes(x = -3, y = 2, label = "Vaccination Date", angle = 90), show.legend = F, color = "blue")+
  ylab("Mortality Rate (Standardized)") +
  xlab("Period (Weeks)") +
  geom_line(data = z %>% filter(Country == "Philippines"),
            aes(period, std_mortality_rate, color = "Philippines"), size =1) + 
  theme_minimal() + 
  theme(legend.title = element_blank(), aspect.ratio = 7/19) +
  ggtitle("Mortality Rate per Week")
ggsave(plot = fig_g, filename = "fig_g.png")
  