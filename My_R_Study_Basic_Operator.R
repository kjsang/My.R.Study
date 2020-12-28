#operator 기초

# 1:10 # 벡터생성
# 9%%2 # 몫
# 9 %% 2 # 나머지
A <- matrix(1:20, ncol = 4)
B <- t(A)
A %*% B # 행렬의 곱을 나타냄
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
:: #패키지의 함수를 불러올 때
?filter()  # stat의 필터인지, dplyr의 필터인지 알 수 없음
dplyr::filter() #명시적으로 어느 패키지의 함수를 사용하는지 알려주는 것

%>% # 왼쪽에 있는 애를 오른쪽 함수의 첫번째 인자로 받는다.
x <- 1:10
y <- x*2
plot(x,y)

install.packages("tidyverse")
library(tidyverse)
x %>% plot(y)
mtcars %>% dim()