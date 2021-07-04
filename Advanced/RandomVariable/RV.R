#### 확률변수 만들기 ####

magic_box <- function(){
  random_num0to1 <- runif(1)
  
  if (random_num0to1 < 0.5) {
    result <- 0
  } else {
    result <- 1
  }
  
  return(result)
}



#### 함수 실행해보기 ####

magic_box()


#### 짧은 식으로 만들기 ####
magic_box <- function(){
  ifelse(runif(1) < 0.5, 0, 1) 
}


#### 예제1 ####
### 0이 2개, 1이 1개 들어있는 상자의 확률변수는? ###
magic_box <- function(){
  random_num0to1 <- runif(1)
  
  if (random_num0to1 < (2/3)) {
    result <- 0
  } else {
    result <- 1
  }
  
  return(result)
}

#### 예제2 ####
### 1이 1개, 2가 1개 들어있는 상자의 확률변수는? ###
# 이 함수를 실행시키기 위해서는
# magic_box()가 로딩되어 있어야 합니다.
random_1or2 <- function(){
  
  magic_box() + 1
  
}





x <- c(1,2,3,4,5)
x |> mean
install.packages("tidyverse")
yeslibrary(tidyverse)
x %>% mean
