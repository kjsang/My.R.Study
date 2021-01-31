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

library(tidyverse)
library(ggthemes)

# 롤리팝 차트
kovo <- read.csv('kovo.csv') %>%
  as_tibble

kovo %>% 
  ggplot(aes(x=구단, y=퀵오픈)) +
  geom_segment(aes(xend=구단, yend=0)) + # 끝점과 시작점 바꿔주기
  geom_point(size=5) # 롤리롤리 팝팝

kovo %>% 
  ggplot(aes(x=reorder(구단, 퀵오픈), y=퀵오픈, color = 남녀부)) + # 구단을 하되, 퀵오픈 순서대로 해라, 색깔
  geom_segment(aes(xend=구단, yend=0)) +
  geom_point(size=5)

kovo %>% 
  ggplot(aes(x=reorder(구단, 퀵오픈), y=퀵오픈, color = 남녀부)) + # 구단을 하되, 퀵오픈 순서대로 해라, 색깔
  geom_segment(aes(xend=구단, yend=0)) +
  geom_point(size=5) +
  facet_grid(~남녀부) # 남녀부별

kovo %>% 
  ggplot(aes(x=reorder(구단, 퀵오픈), y=퀵오픈, color = 남녀부)) + # 구단을 하되, 퀵오픈 순서대로 해라, 색깔
  geom_segment(aes(xend=구단, yend=0)) +
  geom_point(size=5) +
  facet_grid(~남녀부, scales = "free_x") # 남녀부별 / 남녀 빈 칸 없애기

kovo %>% 
  ggplot(aes(x=reorder(구단, 퀵오픈), y=퀵오픈, color = 남녀부)) + # 구단을 하되, 퀵오픈 순서대로 해라, 색깔
  geom_segment(aes(xend=구단, yend=0)) +
  geom_point(size=5) +
  coord_flip() +
  facet_grid(~남녀부, scales = "free_x") # 남녀부별 / 남녀 빈 칸 없애기

kovo %>% 
  ggplot(aes(x=reorder(구단, 퀵오픈), y=퀵오픈, color = 남녀부)) + # 구단을 하되, 퀵오픈 순서대로 해라, 색깔
  geom_segment(aes(xend=구단, yend=0)) +
  geom_point(size=5) +
  coord_flip() +
  facet_grid(남녀부~., scales = "free_y") # 물결 위치에 따라 나타나는 모양이 다름(뒤에 점 붙여줘야함)

kovo %>% 
  ggplot(aes(x=reorder(구단, 퀵오픈), y=퀵오픈, color = 남녀부)) +
  geom_segment(aes(xend=구단, yend=0)) +
  geom_point(size=5) +
  scale_color_manual(values=c('darkblue', 'darkred')) + # 색깔 지정해주기기
  coord_flip() +
  labs(x="") +
  facet_grid(남녀부~., scales = "free_y")
# 많이 그린다. 점만 남겨두기도 하고... 

kovo %>% # 점만 남겨둔 모양 (세그먼트 지워주면 됨)
  ggplot(aes(x=reorder(구단, 퀵오픈), y=퀵오픈, color = 남녀부)) +
  geom_point(size=5) +
  scale_color_manual(values=c('darkblue', 'darkred')) + # 색깔 지정해주기기
  coord_flip() +
  labs(x="") +
  facet_grid(남녀부~., scales = "free_y")

# 덤벨차트

fifa <- read.csv('fifa.csv') %>%
  as_tibble

fifa %>% 
  filter(confederation=='AFC') %>%
  select(country_full, rank) %>% # 나라이름과 랭킹
  group_by(country_full) %>% # 나라이름 묶기
  summarise(최저=max(rank), 최고=min(rank)) %>% #맥스 민 구하기
  ggplot(aes(x=country_full, y=최고)) + # 최고
  geom_point()

fifa %>% 
  filter(confederation=='AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>% 
  summarise(최저=max(rank), 최고=min(rank)) %>%
  ggplot(aes(x=reorder(country_full, 최고), y=최고)) + # 최고로 순서정렬
  geom_point()

fifa %>% # 순위는 낮을 수록 좋으니 리버스필요
  filter(confederation=='AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>% 
  summarise(최저=max(rank), 최고=min(rank)) %>%
  ggplot(aes(x=reorder(country_full, 최고), y=최고)) + # 최고로 순서정렬
  scale_y_reverse() + # 리버스
  geom_point()

fifa %>% # 순위는 낮을 수록 좋으니 리버스필요
  filter(confederation == 'AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>%
  summarise(최저 = max(rank),  최고 = min(rank)) %>%
  ggplot(aes(x = reorder(country_full, -최고), y = 최고)) + # 최고로 순서정렬 / 리버스2 (최고에 - 붙여준다)
  scale_y_reverse() + # 리버스
  geom_point() +
  coord_flip() # x,y축 바꿔주고!

fifa %>% # 최저점도 함께 보여주자
  filter(confederation == 'AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>%
  summarise(최저 = max(rank),  최고 = min(rank)) %>%
  ggplot(aes(x = reorder(country_full, -최고), y = 최고)) +
  scale_y_reverse() +
  geom_point() + # 비워두면 ggplot 코드 상의 점을 찍는 것
  geom_point(aes(y = 최저)) + # 최저점 하나 더 찍어주기
  coord_flip()

fifa %>% # 최저점도 함께 보여주자
  filter(confederation == 'AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>%
  summarise(최저 = max(rank),  최고 = min(rank)) %>%
  ggplot(aes(x = reorder(country_full, -최고), y = 최고)) +
  scale_y_reverse() +
  geom_point() + 
  geom_point(aes(y = 최저)) +
  geom_segment(aes(xend=reorder(country_full, -최고), yend=최저)) + # 최고점, 최저점을 이어주자
  coord_flip()

fifa %>% # 화살표를 그려보자
  filter(confederation == 'AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>%
  summarise(최저 = max(rank),  최고 = min(rank)) %>%
  ggplot(aes(x = reorder(country_full, -최고), y = 최고)) +
  scale_y_reverse() +
  geom_point() + 
  geom_point(aes(y = 최저)) +
  geom_segment(aes(xend=reorder(country_full, -최고), yend=최저), arrow=arrow(length = unit(0.2, "cm"))) + # arrow로 화살표를 그려주었다.
  coord_flip

fifa %>% # 화살표를 반대로 그려줘야하겠죠?
  filter(confederation == 'AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>%
  summarise(최저 = max(rank),  최고 = min(rank)) %>%
  ggplot(aes(x = reorder(country_full, -최고), y = 최고)) +
  scale_y_reverse() +
  geom_point() + 
  geom_point(aes(y = 최저)) +
  geom_segment(aes(xend=reorder(country_full, -최고), y=최저, yend = 최고), arrow=arrow(length = unit(0.2, "cm"))) + # y 최저, yend를 최고로 설정
  coord_flip()

fifa %>% # 화살표 모양 이쁘게 해줘야겠죠? 디테일 살려주자!
  filter(confederation == 'AFC') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>%
  summarise(최저 = max(rank),  최고 = min(rank)) %>%
  ggplot(aes(x = reorder(country_full, -최고), y = 최고)) +
  scale_y_reverse() +
  geom_point() + 
  geom_point(aes(y = 최저)) +
  geom_segment(aes(xend=reorder(country_full, -최고), y=최저-1.5, yend = 최고+1.5), arrow=arrow(length = unit(0.2, "cm")), color='gray50') + # y, yend 위치를 살짝 조정해준다. 색깔도 함께 입혀보자.
  coord_flip()

# 기왕 데이터가 있으니 한국 데이터를 살펴보자.
fifa %>%
  filter(country_abrv=='KOR') %>%
  select(country_full, rank) %>%
  group_by(country_full) %>%
  summarise(최저=max(rank), 최고=min(rank))

fifa %>% # 우리나라가 제일 잘했을 때는 언제?
  filter(country_abrv=='KOR' & rank==17) %>% 
  select(rank_date)

fifa %>% # 우리나라 랭킹 추이?
  filter(country_abrv=='KOR') %>%
  ggplot(aes(x=rank_date, y=rank, group=country_abrv)) + # 나라이름을 기준으로 애네를 묶어라 라고 코드를 입력해준 것임
  geom_line() +
  scale_y_reverse()

fifa %>% # x축 깔끔하게 보여주기
  filter(country_abrv=='KOR') %>%
  ggplot(aes(x=as.Date(rank_date), y=rank, group=country_abrv)) + # 데이트가 날짜라는 것을 알려주자
  geom_line() +
  scale_y_reverse()


# 슬로프 차트
# 이번에는 배구
kal <- read.csv('kal.csv') %>% as_tibble

kal %>%
  gather(공격, 점유율, -세터) %>% # 롱폼으로 공격유형을 모아주자 (세터에 해당하는 애들은 건드리지 마라)
  ggplot(aes(x=세터, y=점유율, group=공격)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label= format(점유율, digits = 1))) # 소수점 정리

kal %>% # 텍스트를 한 쪽만 가져오고 싶다.
  gather(공격, 점유율, -세터)%>%
  ggplot(aes(x=세터, y=점유율, group=공격)) +
  geom_point() +
  geom_line() +
  geom_text(data=. %>% filter(세터=='유광우'), # 유광우만 텍스트 불러와라
            aes(label=format(점유율, digits=1)))

kal %>% # 공격종류도 불러오고 싶어
  gather(공격, 점유율, -세터) %>%
  ggplot(aes(x=세터, y=점유율, group=공격)) +
  geom_point() +
  geom_line() +
  geom_text(data=. %>% filter(세터=='유광우'), 
            aes(label= paste(공격, format(점유율, digits=1)))) # 공격종류 추가

kal %>% # 양쪽에 정보 입력
  gather(공격, 점유율, -세터) %>%
  ggplot(aes(x=세터, y=점유율, group=공격)) +
  geom_point() +
  geom_line() +
  geom_text(data=. %>% filter(세터=='유광우'), 
            aes(label=paste(공격, format(점유율*100, digits=2))), hjust=1.1) +
  geom_text(data=. %>% filter(세터=='한선수'), # 뒤에도 정보 추가해주기
            aes(label=format(점유율*100, digits=1), hjust=-0.2)) # 디짓 1

kal %>%
  gather(공격, 점유율, -세터) %>%
  ggplot(aes(x=세터, y=점유율, group=공격, color = 공격)) +
  geom_point() +
  geom_line() +
  geom_text(data=. %>% filter(세터=='유광우'), 
            aes(label=paste(공격, format(점유율*100, digits=2))), hjust=1.1) +
  geom_text(data=. %>% filter(세터=='한선수'), # 뒤에도 정보 추가해주기
            aes(label=format(점유율*100, digits=1), hjust=-0.2)) # 디짓 1


# 으악~ 레이스차트 등장

# install.packages('gganimate')
# install.packages('gapminder')
# install.packages("gifski")
library(gifski)
library(gganimate)
library(gapminder)

gdp <- read.csv("https://raw.github.com/datasets/gdp/master/data/gdp.csv")
words <- scan(
  text="world income only total dividend asia euro america africa oecd",
  what= character())
pattern <- paste0("(",words,")",collapse="|")
gdp  <- subset(gdp, !grepl(pattern, Country.Name , ignore.case = TRUE))
colnames(gdp) <- gsub("Country.Name", "country", colnames(gdp))
colnames(gdp) <- gsub("Country.Code", "code", colnames(gdp))
colnames(gdp) <- gsub("Value", "value", colnames(gdp))
colnames(gdp) <- gsub("Year", "year", colnames(gdp))

gdp$value <- round(gdp$value/1e9)

gap <- gdp %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-value) * 1,
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",value)) %>%
  filter(rank <=10) %>%
  ungroup()

p <- ggplot(gap, aes(rank, group = country, 
                     fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='{closest_state}', x = "", y = "GDP in billion USD",
       caption = "Sources: World Bank | Plot generated by Nitish K. Mishra @nitishimtech") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, 200, fps = 10, duration = 40, width = 800, height = 600, renderer = gifski_renderer("gganim.gif"))


height <- read_csv('http://ncdrisc.org/downloads/height/NCD_RisC_eLife_2016_height_age18_countries.csv')
names(height)[1] <- 'country' # 소스를 바꾸는게 아니라 데이터를 바꾸기
names(height)[4] <- 'year'
names(height)[5] <- 'value'
height <- height %>% filter(Sex=='Men')
gap <- height %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-value) * 1,
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",value)) %>%
  filter(rank <=10) %>%
  ungroup()
p <- ggplot(gap, aes(rank, group = country, 
                     fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='{closest_state}', x = "", y = "GDP in billion USD",
       caption = "Sources: World Bank | Plot generated by Nitish K. Mishra @nitishimtech") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, 200, fps = 10, duration = 40, width = 700, height = 500, renderer = gifski_renderer("gganim.gif"))
