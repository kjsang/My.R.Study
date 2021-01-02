# R로 하는 프로그래밍의 기본
# R은 데이터분석 도구이면서 프로그램 언어이다.
# 프로그래밍이란?: 컴퓨터가 문제를 해결할 수 있도록 절차를 서술해 놓은 것
# 프로그램을 만드는 과정을 프로그래밍이라고 한다.

####if문####

# 조건문: 참이면 실행, 거짓이면 다른 실행
# 참, 거짓 여부에 따라 다른 처리가 필요할 때 사용한다.

# if(logical expression) {  # 조건문
#   statements              # 참이면 실행
# } else {
#   alternatie statements   # 거짓이면 실행
# }

# 예시1
a <- 10
if (a>5) {
  print(a)    # 참이면 실행
} else {
  print(a/10)
  print(a*10) # 거짓이면 실행
}

#예시2
a <- 10
b <- 20
if(a>5 & b>5) {    # and
  print (a+b)
}
if(a>15 | b>15) { b  # or
  print (a*b)
}

#예시3 ifelse()
a <- 10
b <- 20
ifelse(a>b, c <- a, c <- b) #(조건, 참, 거짓)


####반복문####
for(i in 1:10) {
  print(i)
}

# 구구단 만들기
for (i in 1:10) {
  cat("2*",i,"=",2*i,"\n")
}


# 짝수인지 확인
for(i in 1:20) {
  if(i%%2==0) {
    print(i)
  }
}

# 반복문과 if 응용
v1 <- 101:200
for(i in 1:length(v1)) {
  if(v1[i]%%2==0) {
    print(v1[i]*2)
  } else {
    print(v1[i]+2)
  }
}

# 1부터 100까지 합 구하기 (수열)
sum <- 0
for(i in 1:200) {
  sum <-  sum + i
}
print(sum)

# 반복문 while
# 특정 조건이 만족될 때까지 반복하는 것
i <- 1
while(i <= 10) {
  print(i)
  i <- i+1
}

####사용자 정의함수####

# 다른 누군가 만들어 놓은 함수를 사용하는 법은 패키지 활용
# 내가 필요한 함수가 없다면 만들어 사용할 수 있음

# 함수가 필요한 이유
# 반복잡업할 때 사용: 한 번의 호출로 실행 가능


mymax <- function(x, y) {
  # x와 y라는 매개변수
  num.max <- x
  if (y > x) {
    num.max <- y
  }
  return(num.max) # 함수의 실행결과 값
}

mymax(10, 15)
mymax(20, 15)

# 함수의 초기값 (Default Value) 지정

mydiv <- function (x, y = 2) {
  result <- x / y
  return (result)
}

mydiv (x = 10, y = 3)
mydiv (10, 3)
mydiv (10)  # y의 함수값은 기본값 2이기 때문에 값은 5

# input() 여러 개 가능
# return() 한 개 가능 / list()로 여러 개 자료로 묶어줄 수 있음


myfunc <- function(x, y) {
  val.sum <- x + y
  val.mul <- x * y
  return(list(sum = val.sum, mul = val.mul))
}

myfunc(10, 10)

result <- myfunc(5, 8)
result
result$sum
result$mul

# 함수를 불러와서 사용하는 방법
rm(list = ls())
# 저장할 함수
mydiv <- function (x, y = 2) {
  result <- x / y
  return (result)
}
dump("mydiv", file = "mydiv.R") # 객체저장함수

rm(list = ls()) # 삭제하고


source("mydiv.R") # 불러오기
a <- mydiv(20, 4)
b <- mydiv(30, 4)
a + b
mydiv(mydiv(20, 2), 5) # 제대로 실행 되죠?


####apply 계열 함수####

# R을 사용할 때에는 for while을 사용하지 않는다.
#상당한 시간이 걸리기 때문

# apply 계열 함수를 사용한다.
# 반복문 안 처리대상 데이터가 매트릭스, 데이터프레임, 리스트 등일때 사용

for (i in 1:4) {
  print(mean(iris[, i]))
}

apply(iris[, 1:4], 2, mean) # row 방향 작업
apply(iris[, 1:4], 1, mean) # col 방향 작업
# 사용자 지정 함수도 적용 가능

# lapply는 list 포멧으로 나옴
# apply는 결과에 따라 메트릭스, 데이터프레임 등으로 나옴

lapply(iris[, 1:4], mean)
# 리스트 형태의 값을 작업할 때 사용한다.

abc <- list(mtcars[, 1],
            mtcars[, 2],
            mtcars[, 3],
            mtcars[, 4])
lapply(abc, mean)
lapply(abc, mean)[[1]]

# sapply는 실행결과를 벡터, 리스트로 선택

sapply(iris[, 1:4], mean)               # 결과가 vector
sapply(iris[, 1:4], mean, simplify = F) # 결과가 list
apply(iris[, 1:4], 2, mean)             # 결과가 vector  

# 사용자 정의함수를 적용해보자
myfunc <-  function(x) {
  a <- max(x)
  b <- min(x)
  return(a-b)                           # 범위를 return
}
sapply(iris[,1:4], myfunc)

rm(list=ls())

#### 게임 만들기 ####

# 프로그래밍 예제
# 숫자 맞추기 게임
# 컴퓨터가 1:100 사이의 임의의 값 지정 (랜덤)

n <- readline(prompt = "숫자를 입력하세요: ")  # 커서 깜빡임
cat("입력한 숫자는", n, "입니다. \n")        # 산출할 때 cat 활용

num <- round(runif(1) * 100, digits = 0)     # runif는 임의의 값 생성
guess <- -1
cat("Guess a number between 0 and 100. \n")

while (guess != num){
  guess <- readline(prompt = "Guess number :") # 문자입력
  guess <- as.integer(guess)                 # 숫자로 바꿔줌
  if (guess == num) {
    cat("Congraturations,", num, "is right. \n")
  } else if (guess > num) {
    cat("It is smaller! \n")
  } else if (guess < num) {
    cat("It is bigger! \n")
  }
}

#### sink ####

# sink 파일에 출력
# text.txt 파일에 1:100 숫자 출력하기
sink("test.txt", append=F) # 뒤에 붙여서 출력할 것인가?
for (i in 1:100) {
  print(i)
}
sink()