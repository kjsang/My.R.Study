install.packages("rvest")
pacman::p_load(tidyverse, rvest)
pacman::p_load(curl, httr, RSelenium)
pacman::p_load(jsonlite, stringr, readxl)
library(tidyverse)



read_xls("rawdata_gentri.xls") -> rawdata
rawdata %>% 
  select(date, press, title, keywords, URL) %>%
  filter(press == "중앙일보")

source <- "https://news.joins.com/article/23948609"
source %>% 
  html_nodes(css = "#article_body") %>% 
  html_attr(
