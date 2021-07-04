library(tidyverse)
###################################
####### 오퍼레이터 배우기 #########
###################################

### 기초 ####

# 1. 기초
1:10 # 벡터 생성
9 %/% 2 # 몫
9 %% 2 # 나머지

# 2. 기초2
# %*% 행렬의 곱
A <- matrix(1:12, ncol = 4)
B <- t(A)
A %*% B

# 3. 기초3
# %o% 벡터외적
1:3 %o% 1:3 # 외적
?"%o%"
# %x% kronecker 곱 (행렬)
M <- matrix(1:6, ncol = 2)
kronecker(4, M)
kronecker(diag(1, 3), M) # 3 by 3 행렬에 크로네커 곱
?kronecker
# example1
# ask for dimnames

fred <- matrix(1:12, 3, 4, 
               dimnames = list(LETTERS[1:3], 
                               LETTERS[4:7]))
bill <- c("happy" = 100, 
          "sad" = 1000)
kronecker(fred, bill, 
          make.dimnames = TRUE)

bill <- outer(bill, c("cat" = 3, "dog" = 4))
kronecker(fred, bill, make.dimnames = TRUE)

#### 초급 ####
1:10 %in% c(2,4,6) # 들어...있니?
# :: # 패키지 함수를 불러올 때
# %>% # 마그리트 연산자! 무조건 알아야 함
iris %>%
  as_tibble %>% 
  dplyr::filter(Species == "setosa")

#### 중급 ####
library(magrittr)
# %<>% # 마그리트를 자동저장
mtcars %<>%
  filter(mpg > 15)
# %$% # 데이터
mtcars %>% names # 변수명
plot(mtcars$mpg, mtcars$hp)
with(mtcars, plot(mpg, hp))
mtcars %$% plot(mpg, hp) # 위 두 개랑 같다

rnorm(200) %>% 
  matrix(ncol = 2) %T>% # 한 칸 뛰어넘기
  plot %>% 
  colSums()


#### 고급 ####
# +, %+replace%
e1 + e2 # 중첩형
e1 %+replace% e2 # 전부 대체
?"%+replace%"
add_el <- theme_grey() +
  theme(text = element_text(family = "Times"))
add_el$text

rep_el <- theme_grey() %+replace%
  theme(text = element_text(family = "Times")) # 나머지는 모두 NULL
rep_el$text

# %<-% # 파이썬에서 사용 하는 것처럼
install.packages("zeallot")
library(zeallot)
?"%<-%"
# simple model and summary
# basic usage
c(a, b) %<-% list(0, 1)

a  # 0
b  # 1

# unpack and assign nested values
c(c(e, f), c(g, h)) %<-% list(list(2, 3), list(3, 4))

e  # 2
f  # 3
g  # 4
h  # 5

# <<- # asign에 하나 더 꺾쇠가 있는 것
# 현재 작성된 환경을 건너뛰어 변수 할당
x <- 3 # 상위환경
myf <- function(){
  x <- 3
  x <<- 2 # 상위 환경에 2를 넣어줘!
  y <- x * 2
  return(y)
}
myf() # 6이 반환되며, x는 2가 되어있다

#::: # 트리플 콜론 오퍼레이터
# 패키지 함수 중 패키지 안에서 사용되는 함수를 밖으로 꺼내쓰는 연산자
?":::"
base::log
base::"+"

## Beware --  use ':::' at your own risk! (see "Details")
stats:::coef.default

# 내가 만드는 오퍼레이터
'%+%' <- function(a, b){
  paste(a,b)
}
"my" %+% "first" %+% "operator"

as_tibble(iris)
as_tibble(iris)

tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y
)

tibble(x=1:5,
       y=1,
       z=x^2+y)

tb <- tibble( # 특수문자도 변수명으로 입력 가능
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)

tb <- tibble(';' ="smile",
             ' ' = "space",
             '2000' = "number")

tb

tb <- data.frame(';' ="smile",
                 ' ' = "space",
                 '2000' = "number")

tb <- data.frame( # 특수문자도 변수명으로 입력 가능, 데이터프레임은 아님
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

names(table1)
names(table1)
table2

table2


example <- table2 %>% 
  pivot_wider(names_from =type,
              values_from = count)

table2 %>%
  pivot_wider(names_from = type, # 와이드 형식으로 만들기
              values_from = count) -> example
example %>% 
  pivot_longer(c(cases, population),
               names_to = "type",
               values_to = "count")

example

table4a %>% 
  pivot_longer(c('1999', '2000'),
               names_to="year",
               values_to = "cases")


table4a %>% # 타이디 형식으로 만들기
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year", 
               values_to = "cases")


