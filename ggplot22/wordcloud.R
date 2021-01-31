#install.packages('tidytext')
#install.packages('KoNLP')
#install.packages('wordcloud2')
library('wordcloud2')
library('KoNLP')
library('tidytext')
useNIADic() # 특정 사전을 사용하겠습니다.

m <- read_lines('moon.txt')
m %>% head
glimpse(m)

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text) # 띄어쓰기 단위로 토큰화? 안된다 ㅋ

SimplePos09('아버지 가방에 들어가신다.')

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) # 형태소로 슥슥 바꿔주자

# grep 사용하는 방법
ex <- c("apple", "orange", "banana", "Apple", "pineapple")
grep("apple", ex) # 벡터 찾아냄
grepl("apple", ex) # T,F로 구분

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos))

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos)) # gsub은 바꾸기

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos), length=str_length(word)) # 글자를 세줌

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos)) %>%
  filter(str_length(word)>=2) # 두 글자 이상만 뽑아주기

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos)) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort=TRUE) # 낱말별 빈도 추출

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos)) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort=TRUE) %>%
  wordcloud2() # 워드클라우드 그려주기

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos)) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort=TRUE) %>%
  filter(n>=2) %>% # 숫자가 2개 이상인 것만 걸러내주기기
  wordcloud2()
# 단, KoNLP 패키지가 완벽하지 않아서 정확한 데이터가 나오지 않는다.

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos)) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort=TRUE)

m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(pos, text, token=SimplePos09) %>%
  filter(grepl("/n", pos)) %>%
  mutate(word=gsub("/.*$", "", pos)) %>%
  filter(str_length(word)>=2) %>%
  count(word, sort=TRUE) %>%
  write.csv('word_count.csv') # 일단은 저장하고~
# 엑셀에서 직접 노가다로 수작업하자. 어쩔 수 없다..
# 그 다음~ (수작업 안 했음)

wl <- read.csv('word_list.csv')
m %>% 
  tibble(line=1:222, text=.) %>%
  unnest_tokens(word, text) -> raw_text # 어절단위로 끊은 것

raw_text$word %>% head 
grep('우리', raw_text$word) 
grep('우리', raw_text$word) %>% length() # 세어보면? 정확하게 나온다.

for (i in 1:length(wl$noun)){
  wl[i, 2] <- grep(wl[i, 1], raw_text$word) %>% length
} # for문으로 짜면 된다.
wl %>% 
  arrange(-V2) %>% 
  head()

wl %>% 
  arrange(-V2) %>% 
  filter(V2>=2) %>%
  wordcloud2(fontFamily='Noto Sans CJK KR Bold', 
             color='skyblue', 
             minRotation=0, maxRotation=0) # 회전하지 마!

wl %>% 
  filter(V2>=2) %>% # 정렬을 안 하면 그림이 산만하다다
  wordcloud2(fontFamily='Noto Sans CJK KR Bold', 
             color='skyblue', 
             minRotation=0, maxRotation=0)
# 그림으로 그리고싶으면 사이트 이용하기

wl %>% 
  arrange(-V2) %>%
  head(28) -> m_count
m_count # 상위 28개 뽑아내고

raw_text %>%
  filter(word %in% m_count$noun) %>% # 명사를 포함하고 있는 애들을 골라내라.
  select(2, 1) -> m_df
m_df # 제일 많이 나온 낱말이 몇 번째 줄에 나왔는지 알 수 있다.

mg <- graph_from_data_frame(m_df)
V(mg)$type <- bipartite_mapping(mg)$type
mm <- as_incidence_matrix(mg) %*% t(as_incidence_matrix(mg))
diag(mm) <- 0 # 자기 자신이 높게 나오기 때문에 자기 자신에 0을 넣어주기
mg <- graph_from_adjacency_matrix(mm) # 전에 했던 것처럼 해주고

mg %>% as_tbl_graph(directed=FALSE) %>% # 사회관계망 그래프 그려주기
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name)) # 아이유 피쳐링할 때 썼던 코드 슥

mg %>% # 심심하니까 좀 더 만들자
  as_tbl_graph(directed=FALSE) %>%
  mutate(eigen=centrality_eigen()) %>% # 고유중심성 더하고
  ggraph() +
  geom_edge_link(color='gray50', alpha=.2) + # 엣지링크(곡선하고싶으면 커브)
  geom_node_point(aes(size=eigen), alpha=.2) + # 점찍고
  geom_node_text(aes(label=name, size=eigen)) + # 텍스트
  theme_graph() + 
  theme(legend.position='none') # 레전드 빼라

mg %>% as_tbl_graph(directed=FALSE) %>%
  mutate(eigen=centrality_eigen()) %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name)),
                 color='gray75', alpha=.2) +
  geom_node_text(aes(label=name, size=eigen)) +
  scale_size(range=c(3, 9)) + # 스케일 사이즈 좀 더 키우기
  theme_graph() +
  theme(legend.position='none')
