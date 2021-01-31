# 와 지도다!
# https://cloud.google.com/maps-platform/#get-started
# 매월 200달러는 무료로 사용할 수 있습니다.
  
#install.packages('ggmap')
library(ggmap)

## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
## Please cite ggmap if you use it! See citation("ggmap") for details.

register_google(key='##_발급_받으신_키_##')
register_google(key='AIzaSyBSZEwVsmRiYLiCpSWW-ZSwFCvJgG7IhNU')

install.packages('rvest')

library('rvest') # 인터넷에서 정보를 긁어오는 패키지임

# 아래의 코드를 복사 붙여넣기 해주세요 (키가 등록이 되었는지 확인하는 방법)
html.airports <- read_html('https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic')
df <- html_table(html_nodes(html.airports, 'table')[[1]], fill = TRUE)

colnames(df)[6] <- 'total'
df$total <- gsub('\\[[0-9]+\\]', '', df$total)
df$total <- gsub(',', '', df$total)
df$total <- as.numeric(df$total)

gc <- geocode(df$Airport) # 특정한 위치를 가지고 구글에서 좌표를 받아오는 함수
df <- cbind(df, gc)

world <- map_data('world')
ggplot(df, aes(x=lon, y=lat)) + # 위키피디아의 승객이 많은 공항 자료를 긁어와서 그램을 그린 것이다.
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill='gray75', color='gray70') + 
  geom_point(color='dark red', alpha=.25, aes(size=total)) +
  geom_point(color='dark red', alpha=.75, shape=1, aes(size=total)) +
  theme(legend.position='none')

# 본격적으로 ggmap 다루기 시작
geocode('seoul') # 특정값을  넣으면 좌표를 뽑아준다.
geocode('서울특별시 종로구 청계천로 1') # 오류가 나면
geocode(enc2utf8('서울특별시 종로구 청계천로 1')) # 이걸로 해보자
ggmap(get_map(location='south korea', 
              zoom=7)) # 한국의 지도가 나온다.

ggmap(get_map(location='south korea', # 위치
              zoom=7, # 배율
              maptype='roadmap', # 맵 타입: 도로
              color='bw')) # 색깔

map <- get_map(location='south korea', 
               zoom=7, 
               maptype='roadmap', 
               color='bw')
ggmap(map)

library(tidyverse)

star <- read.csv('starbucks.csv') %>% as_tibble
star # 스타벅스

star %>% # 어느 시군구에 스타벅스가 많나?
  group_by(code) %>% 
  summarise(count=n()) %>%
  arrange(-count)

ggmap(map) + 
  geom_point(data=star, aes(x=long, y=lat)) # x위도 y경도

ggmap(map) + 
  geom_point(data=star, aes(x=long, y=lat), color='#006633') # 스타벅스 색깔

ggmap(map) + 
  stat_density_2d(data=star, # 밀집도를 2d로 그려보자
                  aes(x=long, y=lat, fill=..level.., alpha=..level..),
                  # 밀도레벨이 높을 수록 진하게
                  geom='polygon', # 도형모형으로 그려라
                  size=7, bins=28) + # 모양을 이렇게 해라
  scale_fill_gradient(low='transparent', high='#006633') # 그라데이션을 넣자 (반투명)

seoul <- read.csv('seoul.csv') %>% as_tibble
seoul # 쉐이프형태(rst) 파일이 있는데, 에러가 난다.. 서울 데이터만 뽑아옴


ggplot() + # 서울의 지도를 그려보자 (살짝 에러가 있긴 함)
  geom_polygon(
    data = seoul,
    aes(x = long, y = lat, group = group),
    fill = 'white',
    color = 'black'
  )

ggplot() + # 서울에 스타벅스 위치 찍어보기
  geom_polygon(
    data = seoul,
    aes(x = long, y = lat, group = group),
    fill = 'white',
    color = 'black'
  ) +
  geom_point(data = star %>% filter(sido == '서울'),
             aes(x = long, y = lat),
             color = '#006633')

star %>% 
  filter(sido=='서울') %>% # 서울을 가져오고
  group_by(code) %>% # 시군구를 그룹짓고
  summarise(count=n()) %>% # 몇 개인지 세어보고
  right_join(seoul) %>% # 테이블을 합쳐라 (위 자료를 원래 서울값에 합쳐라)
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=count), # 색칠할 때 카운트로
               color='black') +
  scale_fill_viridis_c(direction=-1) # 연속된 숫자로 사용(viridis_c viridis_b 등)

# 조인함수 사용법
# https://kuduz.tistory.com/1154

star %>% # 합치는 방법법
  filter(sido=='서울') %>%
  group_by(code) %>%
  summarise(count=n()) %>%
  right_join(seoul) -> star2
star2

ggmap(get_map('seoul', zoom=11, color='bw')) + # 서울을 줌11로 당겨서 불러오고
  geom_polygon(data=star2, aes(x=long, y=lat, group=group, fill=count), 
               color='black', alpha=.8) + # 레이어를 더해라
  scale_fill_viridis_c(direction=-1) # 색을 칠해라

star %>% # 지도 위에 점을 찍자
  filter(sido=='서울') %>%
  group_by(code) %>%
  summarise(count=n()) %>%
  right_join(seoul) %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=count), 
               color='black') +
  scale_fill_viridis_c(direction=-1) +
  geom_point(data=star%>%filter(sido=='서울'), aes(x=long, y=lat), 
             color='white', size=1)

star %>% # 못생긴 점을 살짝 이쁘게 만들어주자
  filter(sido=='서울') %>%
  group_by(code) %>%
  summarise(count=n()) %>%
  right_join(seoul) %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=count), 
               color='black') +
  scale_fill_viridis_c(direction=-1) +
  geom_point(data=star%>%filter(sido=='서울'), aes(x=long, y=lat), 
             color='white', size=2, alpha=.7) + # 흰색 찍고(투명도는 알파)
  geom_point(data=star%>%filter(sido=='서울'), aes(x=long, y=lat), 
             color='#006633', size=1) # 스벅점 찍고

seoul %>% # 요즘 점 하나로 퉁치는게 유행
  group_by(code) %>%
  summarise(m_long=mean(long), m_lat=mean(lat)) %>% # 위도 경도 평균값
  inner_join(seoul) -> seoul2
seoul2

star %>% 
  filter(sido=='서울') %>%
  group_by(code) %>%
  summarise(count=n()) %>%
  right_join(seoul2) %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), fill='white', color='black') +
  # 지도를 먼저 그리고
  geom_point(aes(x=m_long, y=m_lat, size=count), color='#006633', alpha=.5)
  # 점을 중간에 찍자



# 단순화 해보자 (네모네모빔을 쏴주자)
#install.packages('plotly')
#install.packages('widgetframe')
library(plotly)
library(widgetframe)

star %>% 
  filter(sido=='서울') %>%
  group_by(code) %>%
  summarise(count=n()) %>%
  right_join(seoul2) %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), fill='white', color='black') +
  geom_point(aes(x=m_long, y=m_lat, size=count), color='#006633', alpha=.5) -> p

# 아까 했던 방법
ggplotly(p) %>%
  highlight(selected=attrs_selected(line=list(color='black'))) %>%
  frameWidget()

# 격자에다가 넣어주자
grid <- read.csv('grid.csv') %>% as_tibble
ggplot() + 
  geom_tile(data=grid, aes(x=col, y=row), fill='gray50', color='white') + 
  scale_y_reverse()

# 가로가 너무 기니까 세로로 만들어주자
ggplot() + 
  geom_tile(data=grid, aes(x=col, y=row), fill='gray50', color='white') + 
  scale_y_reverse() +
  coord_fixed() # 가로세로 바꾸기


ggplot() +  # 텍스트 넣어주기
  geom_tile(data=grid, aes(x=col, y=row), fill='gray50', color='white') + 
  scale_y_reverse() +
  coord_fixed() +
  geom_text(data=grid, aes(x=col, y=row, label=name), size=3) # 텍스트 넣어주기


sido <- read.csv('sido.csv') %>% as_tibble
ggplot() + 
  geom_tile(data=grid, aes(x=col, y=row), fill='gray50', color='white') + 
  scale_y_reverse() +
  coord_fixed() + 
  geom_text(data=grid, aes(x=col, y=row, label=name), size=3) +
  geom_path(data=sido, aes(x=x+0.5, y=y+0.5, group=group), size=1) # 분할해주기

star %>%
  group_by(code) %>%
  summarise(count=n()) %>%
  left_join(grid, .) -> grid

ggplot() + 
  geom_tile(data=grid, aes(x=col, y=row, fill=count), color='white') + # 색 입히기
  scale_y_reverse() +
  coord_fixed() + 
  geom_text(data=grid, aes(x=col, y=row, label=name), size=3) +
  geom_path(data=sido, aes(x=x+0.5, y=y+0.5, group=group), size=1) 

ggplot() + 
  geom_tile(data=grid, aes(x=col, y=row, fill=count), color='white') + 
  scale_y_reverse() +
  coord_fixed() + 
  geom_text(data=grid, aes(x=col, y=row, label=name), size=3) +
  geom_path(data=sido, aes(x=x+0.5, y=y+0.5, group=group), size=1) +
  scale_fill_viridis_c(direction=-1) # 색 입히기 2
