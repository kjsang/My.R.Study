library(tidyverse)
library(magrittr)

read.csv("trash_gangnam.csv", encoding = "AUC-KR") %>% 
  as_tibble() -> raw_data
raw_data %>%
  mutate_if(is.character, as.factor) %>%
  rename(
    trash_case = 쓰레기통.종류,
    load_name = 도로명.가로.명,
    location = 설치지점
  ) %>%
  mutate(trash_case = str_replace_all(trash_case, " ", "")) -> data
data %>% 
  select(trash_case, load_name, location) %>% 
  group_by(load_name, trash_case) %>% 
  count() %>% 
  arrange(desc(n))
