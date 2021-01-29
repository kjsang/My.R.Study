#install.packages('tidyverse')
library(tidyverse)

# 파일 불러오기
kbo <- read.csv('kbo.csv')
kbo %>% 
  as_tibble() %>%    
  mutate(타율=안타/타수) %>% 
  select(이름, 구단, 타율) %>% 
  group_by(구단) %>% 
  arrange(-타율)

kbo %>% 
  as_tibble() %>% 
  group_by(구단) %>% 
  summarise(안타=sum(안타), 타수=sum(타수), 타율=안타/타수) %>%
  arrange(-타율)

kbo %>% 
  mutate(타율=안타/타수, 
                 출루율=(안타+볼넷+사구)/(타수+볼넷+사구+희생플라이), 
                 장타력=총루타/타수,
                 ops=출루율+장타력) %>% 
  arrange(-ops) %>% 
  head(10) %>% # ops 순서를 정리하되, 상위 10개만 뽑아달라
  select(이름, ops) %>% 
  ggplot(aes(x=이름, y=ops)) + # 각 열 중 x, y 가져와라
  geom_bar(stat='identity') # 막대그래프 가져와서 그려라

kbo %>% 
  mutate(타율=안타/타수, 
           출루율=(안타+볼넷+사구)/(타수+볼넷+사구+희생플라이), 
           장타력=총루타/타수,
           ops=출루율+장타력) %>% 
  arrange(-ops) %>% 
  head(10) %>%
  select(이름, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops)) + # 내림차순순
  geom_bar(stat='identity') +
  coord_flip() # 가로세로 바꿔서

# 그래프에 색 칠하기
kbo %>% 
  mutate(타율=안타/타수, 
           출루율=(안타+볼넷+사구)/(타수+볼넷+사구+희생플라이), 
           장타력=총루타/타수,
           ops=출루율+장타력) -> kbo2

kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  coord_flip() # 뉘어서 

kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_fill_manual(values=c('red', 'orange', 'yellow', 'green', 'blue', 'navy', 'purple'))

# 팔레트 쓰기
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_fill_brewer(palette = 'Spectral') # 팔레트

# 텍스트 쓰기
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=format(ops, digits=3))) + #텍스트 써라
  coord_flip() +
  scale_fill_brewer(palette = 'Spectral')

####### 텍스트 조정 ##### 이거!
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=ops-.1, label=format(ops, digits=3))) +
  coord_flip() +
  scale_fill_brewer(palette = 'Spectral')

# x와 y 변수값 없애줘
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=ops-.07, label=format(ops, digits=3))) +
  coord_flip() +
  labs(title='2019 프로야구 OPS 10걸', x="", y="") +
  scale_fill_brewer(palette = 'Spectral') +
  theme(legend.position = 'none') # 레전드 없애줘

# 색깔 마음에 안 들어?
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=ops-.07, label=format(ops, digits=3))) +
  coord_flip() +
  labs(title='2019 프로야구 OPS 10걸', x="", y="") +
  scale_fill_viridis_d(direction=-1, option = "magma") + # 그라데이션 팔레트 (direction=-1도 됨)
  theme(legend.position = 'none')
# 그라데이션 팔레트는 option = 'viridis', magma, plasma, inferno, cividis 등등
# 미적 감각이 필요한 부분이다..
# 그래서 준비했다. 테마를 사람들이 이미 만들어놨다.


# install.packages('ggthemes')
library(ggthemes)

# 테마 활용해보기
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=ops-.07, label=format(ops, digits=3))) +
  coord_flip() +
  labs(title='2019 프로야구 OPS 10걸', x="", y="") +
  scale_fill_brewer(palette = 'Spectral') +
  theme_economist() + # 테마테마테테마
  theme(legend.position = 'none') #명령어 쓸 때 맨 뒤에써야함(커스텀 내용들)

# 뭐가 달라졌을까요?
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=ops-.07, label=format(ops, digits=3))) +
  coord_flip() +
  labs(title='2019 프로야구 OPS 10걸', x="", y="") +
  scale_fill_economist() +
  theme_economist() +
  theme(legend.position = 'none')

# 폰트를 바꿀 수도 있다.
install.packages('showtext')
library('showtext') 
# 구글 폰트를 사용하는 패키지이다.
font_add_google('Nanum Pen Script', 'nanumpen')
showtext_auto() # 써줘야 작동한다.

# 뭐가 달라졌을까요?
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=ops-.1, label=format(ops, digits=3)), family = 'nanumpen', size = 7) +
  coord_flip() +
  labs(title='2019 프로야구 OPS 10걸', x="", y="") +
  scale_fill_economist() +
  theme_economist() +
  theme(legend.position = 'none',
        text=element_text(family='nanumpen'))

# 뭐가 달라졌을까요?
kbo2 %>% arrange(-ops) %>% 
  head(10) %>%
  select(이름, 구단, ops) %>%
  ggplot(aes(x=reorder(이름, ops), y=ops, fill=구단)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=ops-.1, label=format(ops, digits=3)), family = 'nanumpen', size = 7) +
  coord_flip() +
  labs(title='2019 프로야구 OPS 10걸', x="", y="") +
  scale_fill_economist() +
  theme_economist() +
  theme(legend.position = 'none',
        text=element_text(family='nanumpen', size=15),
        axis.text.y=element_text(hjust=1)) # 테마에서는 또 x,y축 바뀐다... 어이없음


# 와플차트
``
install.packages('waffle')
library(waffle)

w <- c(43, 11, 46)
w %>% 
  waffle(rows = 5, colors = c('#e0b151', '#1d93bc', '#2dbbdd'))

w <- c(가=43, 나=11, 다=46)
w %>% 
  waffle(rows = 5, colors = c('#e0b151', '#1d93bc', '#2dbbdd'),
         legend_pos = "bottom")


# 롤리팝 차트