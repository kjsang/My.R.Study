# 사회관계망 분석


#install.packages('igraph')
#install.packages('ggraph')
#install.packages('tidygraph')
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)

feat <- read.csv('featuring_iu.csv') %>% as_tibble
feat # 아이유가 피쳐링 주고받은 사람들 목록이다.

fg <- as_tbl_graph(feat, directed=FALSE)
fg # 그래프 형태로 변환해준다. 방향이 있고 없고에 따라 T, F

ggraph(fg) + # 그래프 형태는 어떻게 생겼나?
  geom_node_point() + # 점 찍고
  geom_edge_link() # 링크를 연결해라
# > Using `stress` as default layout 여러가지 모양 중 stress 모양으로 만듬

feat %>% 
  as_tbl_graph() %>%
  ggraph(layout='kk') + # 레이아웃 종류
  geom_node_text(aes(label=name)) + # 점 대신 텍스트
  geom_edge_link(aes(start_cap = label_rect(node1.name), # 이름만큼 여백
                     end_cap = label_rect(node2.name))) # 이름만큼 여백

feat2 <- read.csv('featuring_2019.csv') %>% as_tibble
feat2 # 멜론 탑100 피쳐링 결과

feat2 %>% # 그대로 쓰니까? 예쁜모양이 안 나왔다 ㅋㅋㅋㅋㅋ
  as_tbl_graph(directed=FALSE) %>%
  ggraph() + 
  geom_node_text(aes(label=name)) +
  geom_edge_link(aes(start_cap = label_rect(node1.name), 
                     end_cap = label_rect(node2.name)))

feat2 %>% # 망한 걸 바로잡기
  as_tbl_graph(directed=FALSE) %>%
  activate(nodes) %>% # 노드 활성화시키고
  mutate(neighbors = centrality_degree(), # 중심성 추가
         group = group_infomap()) # 그룹

feat2 %>% # 실제로 그려보자
  as_tbl_graph(directed=FALSE) %>%
  activate(nodes) %>%
  mutate(neighbors = centrality_degree(),
         group = group_infomap()) %>%
  ggraph() + # 그래프를 그리는데,
  geom_edge_link(alpha=.2) + # 링크 알파값 지정
  geom_node_point(aes(color=factor(group))) + # 그룹이 펙터라고 말해준 것(색)
  geom_node_text(aes(label=name), size=3) # 텍스트 넣자

feat2 %>%
  as_tbl_graph(directed=FALSE) %>%
  activate(nodes) %>%
  mutate(neighbors = centrality_degree(),
         group = group_infomap()) %>%
  ggraph() +
  geom_edge_link(alpha=.2) +
  geom_node_point(aes(color=factor(group))) +
  geom_node_text(aes(label=name), size=3, repel=TRUE)

feat2 %>%
  as_tbl_graph(directed=FALSE) %>%
  activate(nodes) %>%
  mutate(neighbors = centrality_degree(),
         group = group_infomap()) %>%
  ggraph() +
  geom_node_point(aes(color=factor(group))) +
  geom_node_text(aes(label=name), size=3, repel=TRUE) +
  geom_edge_link(alpha=.2) +
  theme(legend.position='none') # 라벨 없애주자

feat2 %>%
  as_tbl_graph(directed=FALSE) %>%
  activate(nodes) %>%
  mutate(neighbors = centrality_degree(),
         group = group_infomap()) %>%
  ggraph("stress") +
  geom_node_point(aes(color=factor(group))) +
  geom_node_text(aes(label=name), size=3, repel=TRUE) +
  geom_edge_link(alpha=.2) +
  theme_graph(nicely) +
  theme(legend.position='none')


k <- read.csv('kovo_school.csv') %>% as_tibble
k # 야구선수들이 어느 대학 고등학교 나왔나?

k %>% gather(이름, 학교, -선수) %>% # 데이터를 롱폼으로 변형
  select(1, 3) %>% # 맨 앞에 이름과 학교만 골라내자
  graph_from_data_frame() -> kg # 그래프 데이터프레임으로 만들어주자
kg 
plot(kg)

V(kg)$type <- bipartite_mapping(kg)$type # False는 개인, True는 학교 (방향구분)
km <- as_incidence_matrix(kg) # 연결함수로 그려라
km <- km %*% t(km)
diag(km) <- 0 # 요 의미를 모른다고 하심

km <- as_incidence_matrix(kg)

km %>% head(1)
km <- km %*% t(km)
km %>% head(1)

diag(km) <- 0
km %>% head(1)

km %>% as_tbl_graph(directed=FALSE) %>%
  mutate(group=group_infomap()) %>%
  ggraph() +
  geom_edge_link(color='gray85') +
  geom_node_point(aes(color=factor(group)))


#install.packages('networkD3')
library(networkD3)
simpleNetwork(feat)
simpleNetwork(feat2)