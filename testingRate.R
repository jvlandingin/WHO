library(tidyverse)
library(stringr)
library(rvest)

h2 <- read_html("https://www.worldometers.info/coronavirus/")
covid_tests <- h2 %>% html_table()
covid_tests <- covid_tests[[1]]
covid_tests <- covid_tests %>% filter(!is.na(`#`))

covid_tests <- covid_tests %>%
  select(`Country,Other`, TotalTests, Population) %>%
  rename(Country = `Country,Other`) %>%
  mutate(TotalTests = as.numeric(gsub(",","",TotalTests)),
         Population = as.numeric(gsub(",","",Population)),
         testing_rate = TotalTests/Population)

covid_test$code_2[no_codes] <- c("US", "GB", "RU", "IR", "PH", "NL", "VN",
                                 "AE", "BO", "PS", "VE", "DO", "KR", "MD", 
                                 "MK", "NA", NA, "CD", "SY", "SD", "LA", 
                                 "TZ", "BS", "CG", "TW", NA, "BN", "CF", 
                                 "GM", "NE", "VC", "SX", "KM","MF", "TC", 
                                 "VG", NA, NA,"BL",  "KY", NA, "FK", "PM", 
                                 "VA", NA, "MH", "SH", "FM")


# missing_codes <- c("US", "GB", "RU", "IR", "PH", "NL", "VN",
#                    "AE", "BO", "PS", "VE", "DO", "KR", "MD", 
#                    "MK", "NA", NA, "CD", "SY", "SD", "LA", 
#                    "TZ", "BS", "CG", "TW", NA, "BN", "CF", 
#                    "GM", "NE", "VC", "SX", "KM","MF", "TC", 
#                    "VG", NA, NA,"BL",  "KY", NA, "FK", "PM", 
#                    "VA", NA, "MH", "SH", "FM")

newcodes <- covid_test$code_2

newcodes[no_codes] <- c("US", "GB", "RU", "IR", "PH", "NL", "VN",
                                 "AE", "BO", "PS", "VE", "DO", "KR", "MD", 
                                 "MK", "NA", NA, "CD", "SY", "SD", "LA", 
                                 "TZ", "BS", "CG", "TW", NA, "BN", "CF", 
                                 "GM", "NE", "VC", "SX", "KM","MF", "TC", 
                                 "VG", NA, NA,"BL",  "KY", NA, "FK", "PM", 
                                 "VA", NA, "MH", "SH", "FM")



covid_test <- covid_tests %>%
  left_join(tab, by = "Country") %>%
  mutate(codes = newcodes)
covid_test

testDat <- covid_test %>%
  select(Country, codes, testing_rate)
testDat
