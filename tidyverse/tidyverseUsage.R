if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)


# 1. dplyr : tidyR의 시작

# 데이터 불러오기
data(mpg)

# 데이터 살펴보기
glimpse(mpg) # head보다 자세하게 데이터 보여줌
head(mpg) # 구식 ㅋㅋ
View(mpg) # 엑셀처럼 변수 보여줌

# 연속변수 처리
summary(mpg$cty) # 쿠식 ㅋㅋ
filter(mpg, cty > 17)

# 범주형 변수 처리
filter(mpg, cyl %in% 4)
filter(mpg, cyl %in% c(4, 6))

# 문자열 필터
filter(mpg, class == "suv")
filter(mpg, manufacturer == "hyundai" & hwy < 25)
filter(mpg, year > 2000 & class %in% c("subcompact", "suv"))  #2000년 이후, subcompact이거나 suv인 차
filter(mpg, drv %in% "r" | class %in% "suv")


# 2. agrittr, 읽기 쉬운 코드의 시작이자 dplyr의 완성형
mpg %>% 
  filter(year > 2000 & class %in% c("subcompact"))

# select()
mpg %>% 
  glimpse #변수 확인하기
mpg %>% 
  select(model, year, class)

mpg %>% 
  select(-hwy, -fl, -class)
mpg %>% 
  select(manufacturer:cty)
mpg %>% 
  select(-hwy:-class)

# 섞어서도 사용 가능
mpg %>% select(model, year, class, -hwy, -fl)


# 3. filter()와 select()의 혼합 (데이터 전처리에서 많이 쓰임)
mpg %>% 
  filter(cty > 20) %>% 
  select(model, year, cty:class)

# 4. group_by()와 summarise()
mpg %>% 
  group_by(model) # 차종에 따른 연비는 어떻게 다를까? (그룹별 묶음)

mpg %>% 
  group_by(model) %>% 
  summarise(hwy_mean = mean(hwy))
mpg %>%
  group_by(model) %>%
  summarise(hwy_sum = sum(hwy))

# 5. arrange를 이용한 데이터 정렬
mpg %>% 
  group_by(model) %>% 
  summarise(hwy_mean = mean(hwy), hwy_sum = sum(hwy))

mpg %>% 
  group_by(model) %>% 
  summarise(hwy_mean = mean(hwy), hwy_sum = sum(hwy)) %>% 
  arrange(hwy_mean)

mpg %>% 
  group_by(model) %>% 
  summarise(hwy_mean = mean(hwy), hwy_sum = sum(hwy)) %>% 
  arrange(desc(hwy_mean))

# 6. 결측치 처리
nycflights13::flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(month) %>% 
  summarise(delay_mean = mean(dep_delay))
nycflights13::flights %>% 
  group_by(month) %>% 
  summarise(delay_mean = mean(dep_delay, na.rm = TRUE))

# mutate를 통한 변수(열) 만들기
mpg %>% 
  mutate(hwy_per_cty = hwy / cty)