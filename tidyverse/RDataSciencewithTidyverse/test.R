library(tidyverse)
library(readxl)
library(rvest)
data <- read_xls("rawdata_gentri.xls") %>% 
  as.tibble
glimpse(data)
data %>%
  filter(press == "중앙일보") %>%
  mutate("본문",
         URL %>%
           read_html() %>%
           html_nodes("div#article_body") %>%
           as.character() -> data1
as.tibble(data) %>% 
  select(본문) %>% 
?trimws
