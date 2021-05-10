library(tidyverse)
readxl::read_xlsx("R.xlsx") %>% 
  group_by(게재여부) %>% 
  count(소속) %>% 
  arrange(desc(n))
