# sentiments analysis #

library(tidytext)
get_sentiments("afinn")

#> # A tibble: 2,477 x 2
#>    word       value
#>    <chr>      <dbl>
#>  1 abandon       -2
#>  2 abandoned     -2
#>  3 abandons      -2
#>  4 abducted      -2
#>  5 abduction     -2
#>  6 abductions    -2
#>  7 abhor         -3
#>  8 abhorred      -3
#>  9 abhorrent     -3
#> 10 abhors        -3
#> # … with 2,467 more rowsCopy

get_sentiments("bing")
#> # A tibble: 6,786 x 2
#>    word        sentiment
#>    <chr>       <chr>    
#>  1 2-faces     negative 
#>  2 abnormal    negative 
#>  3 abolish     negative 
#>  4 abominable  negative 
#>  5 abominably  negative 
#>  6 abominate   negative 
#>  7 abomination negative 
#>  8 abort       negative 
#>  9 aborted     negative 
#> 10 aborts      negative 
#> # … with 6,776 more rowsCopy

get_sentiments("nrc")
#> # A tibble: 13,901 x 2
#>    word        sentiment
#>    <chr>       <chr>    
#>  1 abacus      trust    
#>  2 abandon     fear     
#>  3 abandon     negative 
#>  4 abandon     sadness  
#>  5 abandoned   anger    
#>  6 abandoned   fear     
#>  7 abandoned   negative 
#>  8 abandoned   sadness  
#>  9 abandonment anger    
#> 10 abandonment fear     
#> # … with 13,891 more rows
#
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#> # A tibble: 303 x 2
#>    word        n
#>    <chr>   <int>
#>  1 good      359
#>  2 young     192
#>  3 friend    166
#>  4 hope      143
#>  5 happy     125
#>  6 love      117
#>  7 deal       92
#>  8 found      92
#>  9 present    89
#> 10 kind       82
#> # … with 293 more rows

library(tidyr)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# 2.3. 정서사전 비교

pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice
#> # A tibble: 122,204 x 4
#>    book              linenumber chapter word     
#>    <fct>                  <int>   <int> <chr>    
#>  1 Pride & Prejudice          1       0 pride    
#>  2 Pride & Prejudice          1       0 and      
#>  3 Pride & Prejudice          1       0 prejudice
#>  4 Pride & Prejudice          3       0 by       
#>  5 Pride & Prejudice          3       0 jane     
#>  6 Pride & Prejudice          3       0 austen   
#>  7 Pride & Prejudice          7       1 chapter  
#>  8 Pride & Prejudice          7       1 1        
#>  9 Pride & Prejudice         10       1 it       
#> 10 Pride & Prejudice         10       1 is       
#> # … with 122,194 more rows

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# 소설 일부에 대한 정서용어집 적용
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)Copy
#> # A tibble: 2 x 2
#>   sentiment     n
#>   <chr>     <int>
#> 1 negative   3324
#> 2 positive   2312Copy

get_sentiments("bing") %>% 
  count(sentiment)
#> # A tibble: 2 x 2
#>   sentiment     n
#>   <chr>     <int>
#> 1 negative   4781
#> 2 positive   2005

# 긍정부정 빈도
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts
#> # A tibble: 2,585 x 3
#>    word     sentiment     n
#>    <chr>    <chr>     <int>
#>  1 miss     negative   1855
#>  2 well     positive   1523
#>  3 good     positive   1380
#>  4 great    positive    981
#>  5 like     positive    725
#>  6 better   positive    639
#>  7 enough   positive    613
#>  8 happy    positive    534
#>  9 love     positive    495
#> 10 pleasure positive    462
#> # … with 2,575 more rows

# 시각화

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# 고려할 점
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

custom_stop_words
#> # A tibble: 1,150 x 2
#>    word        lexicon
#>    <chr>       <chr>  
#>  1 miss        custom 
#>  2 a           SMART  
#>  3 a's         SMART  
#>  4 able        SMART  
#>  5 about       SMART  
#>  6 above       SMART  
#>  7 according   SMART  
#>  8 accordingly SMART  
#>  9 across      SMART  
#> 10 actually    SMART  
#> # … with 1,140 more rows


# 워드클라우드
library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
  
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
                   

p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

p_and_p_sentences$sentence[2]

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())


# 응용...
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()