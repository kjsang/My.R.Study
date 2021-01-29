install.packages("N2H4")
library(N2H4)
library(dplyr) 
tar <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
getAllComment(tar) %>%
	select(userName, contents)

# install.packages("tidytext")
library(tidytext)
tar <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
getAllComment(tar) %>%
  select(userName, contents) %>%
  unnest_tokens(ws, contents, "words")



install.packages("remot")
install.packages("rJava")
library(rJava)

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--np-multiarch"))
library(KoNLP)
SimplePos09("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.")

install.packages("RmecabKo")
RmecabKo::install_mecab("c:/Rlib/mecab")
library(RmecabKo)

# unnest_tokens(
#   tbl = 텍스트 데이터,
#   input = 목표 텍스트 열,
#   output = 결과열의 이름,
#   token = "words", <- 여기에 형태소 분석 함수를 적용
#   ...
# )

library(KoNLP)
# 뉴스 댓글을 가져와서
getAllComment(
  "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
) %>%
  # 사용자 아이디와 댓글 컬럼만 선택하고
  select(userName, contents) %>%
  # 댓글 컬럼을 형태소 단위로 쪼개
  # pos라는 컬럼으로 출력
  unnest_tokens(pos, contents,
                token = SimplePos09) %>% # 쌍따옴표 쓰지 말자
  # 사용자 별로 그룹 지어서
  group_by(userName) %>%
  # pos 결과물의 순서 보장을 위해 순서 값을 추가
  mutate(pos_order = 1:n()) %>% 
  ungroup()

# # A tibble: 4,026 x 3
# # Groups:   userName [224]
# userName pos         pos_order
# <chr>    <chr>           <int>
#   1 mooz**** 조/n                1
# 2 mooz**** :/s                 2
# 3 mooz**** 대따/n              3
# 4 mooz**** 국/n                4
# 5 mooz**** :/s                 5
# 6 mooz**** 밀/p+ㄴ들/e         6
# 7 mooz**** 한테/n              7
# 8 mooz**** 걸렸/n              8
# 9 mooz**** 다/m                9
# 10 cons**** 그래/i+,/s          1
# # ... with 4,016 more rows

library(stringr)
pos_res %>%
  # 우선 `filter()` 와 `str_detect()` 함수를
  # 활용하여 명사(n)만 추출
  filter(str_detect(pos, "/n")) %>%
  # 형태소 정보를 제거
  mutate(pos_done = str_remove(pos, "/.*$")) ->
  n_done
n_done

# 쌍따옴표 쓰면 안 된다.

pos_res %>%
  filter(
    str_detect(pos, "/p")
  ) %>%
  mutate(
    pos_done =
      str_replace_all(pos,
                      "/.*$", "다")
  ) ->
  p_done
bind_rows(n_done, p_done) %>%
  arrange(pos_order) %>%
  filter(nchar(pos_done) > 1) %>%
  select(userName, pos_done) ->
  pos_done
pos_done

getAllComment(
  "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313"
) %>%
  select(userName, contents) %>% 
  mutate(contents = str_remove_all(contents, " ")) 
# all 이 없으면 첫 번째 것만 없어진다.


# install.packages("remotes")
remotes::install_github("mrchypark/multilinguer", force = T)
library(multilinguer)
install.packages("reticulate")
reticulate::install_miniconda()

remotes::install_github("haven-jeon/KoSpacing")
library(KoSpacing)



set_env()
spacing("롯데마트가판매하고있는흑마늘양념치킨이논란이되고있다.")

library(tidyverse)
library(RmecabKo)
library(KoNLP)
spacing("롯데마트가판매하고있는흑마늘양념치킨이논란이되고있다.") %>%
  SimplePos09


#### 지표분석 ####

library(N2H4)
library(dplyr)
library(tidytext)
library(KoNLP)
library(stringr)
library(wordcloud)

# 워드클라우드
"https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313" %>%
  getAllComment() %>%
  select(userName, contents) %>%
  filter(str_length(contents)<250) %>% # 엄청 긴 거
  unnest_tokens(pos, contents,
              token = SimplePos09) %>%
  filter(str_detect(pos, "/n|v(v|a)")) %>% 
  mutate(pos_done = str_remove_all(pos, "/.*$")) %>% 
  filter(str_length(pos_done)>1) %>%
  count(pos_done, sort = T) %>%
  filter(pos_done !=  "^ㅋ") %>% 
  with (
    wordcloud(words = pos_done, freq = n, min.freq = 1,
              max.words=50, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    )

# 유저별 빈도를 R아보자 (단순빈도)
"https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313" %>%
  getAllComment() %>%
  select(userName, contents) %>%
  filter(str_length(contents)<250) %>%
  unnest_tokens(pos, contents,
                token = SimplePos09) %>%
  filter(str_detect(pos, "/n")) %>%
  mutate(pos_done = str_remove(pos, "/.*$")) %>% 
  filter(str_length(pos_done)>1) -> pos_d

pos_d %>% 
  select(-pos) %>% 
  group_by(userName) %>% 
  count(pos_done, sort=T) %>% 
  filter(pos_done == "우")

# 동시출현빈도계산
library(KoSpacing)
set_env()
library(widyr)
library(KoNLP)
"https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313" %>%
  getAllComment() %>%
  unnest_tokens(sent, contents,
                token = "sentences") %>% 
  filter(str_length(sent)<198) %>% #에러가 떠서
  mutate(id = as.numeric(1:n())) %>% 
  unnest_tokens(pos, sent,
                token = SimplePos09) %>% 
  select(id, pos) %>% 
  filter(str_detect(pos, "/n|v(v|a)")) %>% 
  mutate(pos =
           str_remove_all(pos, "/.*$")) %>% 
  filter(str_length(pos)>1) %>% 
  pairwise_count(pos, id,
                 sort = T, upper = F) -> pw
pw

# 바차트 그려보기
# 바차트 그리는게 무슨 의미? 
# 판사가 아침마다 조간신문 정독...
# 모든 기사를 네트워크를 그리면 사건들의 이슈가 보인다.
pw %>%
  filter(item1 == "우리")
library(forcats)
library(ggplot2)
# bar plot
pw %>%
  filter(item1 %in% c("장관")) %>%
  top_n(15) %>%
  mutate(item2 = fct_reorder(item2, n, .desc = TRUE)) %>%
  ggplot(aes(x = item2, y = n, fill = item1)) +
  geom_bar(stat = "identity")

# 네트워크 시각화
library(igraph)
pw %>%
  filter(n > 3) %>%
  graph_from_data_frame() ->
  pw_graph
pw_graph

library(ggraph)
set.seed(2018)
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
ggraph(pw_graph) +
  geom_edge_link(
    aes(edge_alpha = n),
    show.legend = FALSE,
    arrow = a,
    end_cap = circle(.07, 'inches')
  ) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
# 단순빈도로만 했기 때문에 내용이 별거 없음


# tf-idf
"https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313" %>%
  getAllComment() %>%
  select(userName, contents) %>%
  mutate(id = as.numeric(1:n())) %>%
  select(id, contents) %>%
  filter(str_length(contents)<250) %>% # 엄청 긴 거
  unnest_tokens(pos, contents,
                token = SimplePos09) %>%
  filter(str_detect(pos, "/n|v(v|a)")) %>%
  mutate(pos = str_remove_all(pos, "/.*$")) %>% 
  filter(str_length(pos)>1) %>%
  group_by(id) %>% 
  count(pos) %>% 
  bind_tf_idf(pos, id, n) %>% 
  filter(n>1) %>% 
  ungroup() %>% 
  arrange(desc(tf_idf))


library(N2H4)
library(dplyr)
library(tidytext)
library(KoNLP)
library(stringr)
library(RmecabKo) # token = pos
"https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=005&aid=0001236313" %>%
  getAllComment() %>% 
  select(userName, contents) -> tar 

tar %>% 
  mutate(id = as.numeric(1:n())) %>%
  filter(str_length(contents)<250) %>% 
  unnest_tokens(pos, contents,
                token = SimplePos09) %>% 
  filter(str_detect(pos, "/n|/v(v|a)")) %>% 
  mutate(pos = str_replace_all(pos, "/.*$", "")) %>%
  filter(str_detect(pos, "/n", negate = T)) %>% 
  mutate(pos = str_replace_all(pos, "/.*$", "다")) %>%
  arrange(id) %>% 
  filter(str_length(pos)>1) %>% 
  group_by(id) %>% 
  count(pos, sort = T) %>% 
  bind_tf_idf(pos, id, n) %>% 
  filter(n>1) %>% 
  arrange(desc(tf_idf))
# ... 쓸모가 없다.



# 감성분석
remotes::install_github("mrchypark/KnuSentiLexR")
library(KnuSentiLexR)
rta %>%
  unnest_tokens(sent, contents, token = "sentences") %>% # 띄어쓰기는 words simplepos09
  filter(nchar(sent) < 20) %>%
  select(sent) %>% 
  mutate(score = senti_score(sent),
         magni = senti_magnitude(sent)) %>%
  filter(score != 0)
 # 정확도가 처참하다... 기사 제대로 써라가 +1
 # 한글감성분석 API가 구글에 있지만, 성능이 엄청 안 좋다.
 # 어케할까...