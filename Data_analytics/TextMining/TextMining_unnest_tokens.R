#### Text Mining ####

# TTF(Tidy Text Format)

# unrest_tokens function
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)


text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

library(dplyr)
text_df <- tibble(line = 1:4, text = text) # 티블 사용해야함

text_df
#> # A tibble: 4 x 2
#>    line text                                  
#>   <int> <chr>                                 
#> 1     1 Because I could not stop for Death -  
#> 2     2 He kindly stopped for me -            
#> 3     3 The Carriage held but just Ourselves -
#> 4     4 and Immortality


if(!require(tidytext)) install.packages("tidytext")
library(tidytext)

text_df %>%
  unnest_tokens(word, text)

#> # A tibble: 20 x 2
#>     line word   
#>    <int> <chr>  
#>  1     1 because
#>  2     1 i      
#>  3     1 could  
#>  4     1 not    
#>  5     1 stop   
#>  6     1 for    
#>  7     1 death  
#>  8     2 he     
#>  9     2 kindly 
#> 10     2 stopped
#> # … with 10 more rows


# 제인오스틴 작품 TTF 변환
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

original_books


library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

# 불용어 제거
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 

library(ggplot2)

# 빈도수 그래프
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# 구텐베르크R 패키지 망가짐 ..ㅠ 
