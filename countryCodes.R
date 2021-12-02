library(tidyverse)
library(stringr)
library(rvest)

h1 <- read_html("https://www.iban.com/country-codes")
tab <- h1 %>% html_nodes(".flat-services")

tab <- tab %>% html_table
tab <- tab[[1]]

head(tab)
tab <- tab %>% rename(code_3 = "Alpha-3 code", code_2 = "Alpha-2 code")

