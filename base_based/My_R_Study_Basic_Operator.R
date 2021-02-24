#operator 기초

# 1:10 # 벡터생성

1:10


# 9 %% 2 # 나머지
9%%2
?ncol
A <- matrix(1:28, ncol = 4)
A
B <- t(A)
B


A %*% B # 행렬의 곱을 나타냄

C <- matrix(1:4, ncol = 2)
D <- matrix(c(1,0,0,1), ncol=2)
C
D
C %*% D

1:3 %o% 2:3 # 벡터외적
# > 1:3 %o% 2:3
# [,1] [,2]
# [1,]    2    3
# [2,]    4    6
# [3,]    6    9

#%x% kronecker products / 몰라도 된다..
M <- matrix(1:6, ncol = 2)
kronecker(4, M)

#초급

# :10 %in% c(2,4,6) 왼쪽에 오른쪽이 들어있니?
%in%
hero <- c("Superman", "Batman", "Spiderman", "Ironman")
marble.hero <- c("Spiderman", "Ironman")
hero %in% marble.hero

  :: #패키지의 함수를 불러올 때 (~에 속한)
?Filter()  # stat의 필터인지, dplyr의 필터인지 알 수 없음
dplyr::filter() #명시적으로 어느 패키지의 함수를 사용하는지 알려주는 것


%>% # 왼쪽에 있는 애를 오른쪽 함수의 첫번째 인자로 받는다.
x <- 1:10
y <- x*2
plot(x,y)

x %>% plot(y)

install.packages("tidyverse")
library(tidyverse)
x %>% plot(y)

dim(cars)

cars %>% dim()
