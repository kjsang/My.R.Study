# Dog's honey tip: 주석처리 단축키
# ctrl + shift + c

####R Studio 단축키####

# 파이프연산자
ctrl + shfit + m


# 할당연산자 (asign)
# alt + -

# 커서 여러개 만들기
ctrl + alt + 방향키
plot( )
plot( )
plot( )
plot( )
plot( )

# 줄 복사
shift + alt + 방향키
안녕하세요 shortcut 배우기
안녕하세요 shortcut 배우기
안녕하세요 shortcut 배우기
안녕하세요 shortcut 배우기

# 함께 바꾸기
ctrl + alt + k
안녕하세요 shortcut 배우기
shortcut 배우기 안녕 요하세
안녕하세요 shortcut 배우기
하세요 shortcut 배우기 안녕

# 커맨드처리 (주석처리)
ctrl + shift + c

# 지저분한 코드 정리하기 (예쁘게 만들어주기)
mydata <- function(var1) {
  a <- c(1:10)
  b <- a / 3
  plot(a, b)
}

ctrl + shift + a
# mydata <- function(var1) {
#   a <- c(1:10)
#   b <- a / 3
#   plot(a, b)
# }

# 콘솔창 정리
ctrl + l

# shortcut 만들기
tools > modify keyboard shortcut > Insert Snippet

# 패널 간 이동
ctrl+1 스크립트
ctrl+2 콘솔창   




####산점도####
data("mtcars")
wt <- mtcars$wt
mpg <- mtcars$mpg

#plot()은 산점도를 그리는 명령어 
#두개의 변수로 그리게 된다.
plot(wt,mpg,
     main = "Car Weight-mpg",
     sub = "test",
     xlab = "Car Weight",
     ylab = "Miles Per Gallon",
     col = "red",
     cex = 1, # 점의 크기
     pch = 19, #point의 종류
     type = "p") # 점 모양 그래프(기본값) (p,l,b,c,o,h,s,S,n 등 설정 가능)
# 산점도를 보면 상관관계를 시각적으로 볼 수 있다.
# 변수 간 상관관계 확인 가능

####산점도 응용####
data("mtcars")
vars <- c("mpg", "disp", "drat", "wt")    # 대상 변수
target <- mtcars[,vars]


#pairs()은 여러 변수들 간 상관관계를 한번에 나타낸다.
pairs(target,           # 대상 데이터
      main = "multi plots")

#실전예제
data(iris)
iris.2 <- iris[,3:4]
point <- as.numeric(iris$Species)
color <- c("red", "green", "blue")
plot(iris.2,
     main="Iris plot",
     pch=c(point),
     col=color[point])

# 같은 품종이면 모여있는 것을 발견할 수 있다.
# 꽃잎의 길이와 폭은 양의 상관관계에 있다.
# 꽃잎의 폭과 넓이정보만 있으면 품종을 구별할 수 있다.



####상관분석####

# 두 개의 변수, 양적 자료에 대한 분석방법
# 산점도와 함께 이용
# 수치적으로 변수의 상관관계 표현

# 상관계수는 -1<=r<=1
# r>0 음의 상관관계
# r<0 양의 상관관계
# 1이나 -1에 가까울 수록 상관성 높음
# 0에 가까울 수록 상관성이 떨어짐

plot(iris$Petal.Length~iris$Petal.Width,
     data = iris) # 산점도
res = lm(iris$Petal.Length~iris$Petal.Width, 
         data = iris) # 회귀식
abline(res) # 회귀선 그리기
cor(iris$Petal.Width, iris$Petal.Length) # 상관성 분석 시행


# 여러 변수들 간 상관계수를 동시에 구하고 싶다면?
cor(iris[,-5])


####선그래프####

# 두 개의 변수 중 하나가 시간을 나타내는 값일 때 사용
# 시계열 분석에서 쓰임

# 월별 지각생 통계
month <-  1:12
late <-  c(5,8,7,9,4,6,12,13,8,6,6,4)
plot(month,                   # x data
     late,                    # y data
     main="Late student",     # 그래프 제목
     type="o",                # 그래프 종류(알파벳 l,b,o)
     lty=1,                   # 선의 종류 (1:6)
     lwd=2,                   # 선의 굵기
     xlab="Month",            # x축 레이블
     ylab="Late cnt")         # y축 레이블

#선이 여러개인 그래프 그리기
#하나의 선그래프를 그린 후에 그 위에 다른 선그래프를 겹쳐 그리는 방식

month <-  1:12
late1 <- late
late2 <- c(4,6,5,8,7,8,10,11,6,5,7,3)

plot(month,                   # x data
     late1,                    # y data
     main="Late student",     # 그래프 제목
     type="o",                # 그래프 종류(알파벳 l,b,o)
     lty=1,                   # 선의 종류 (1:6)
     lwd=1,                   # 선의 굵기
     col="blue",              # 선의 색상
     xlab="Month",            # x축 레이블
     ylab="Late cnt"          # y축 레이블
     )
lines(month,
      late2,
      type="b",
      col="red")

rm(list=ls())
#######데이터 분석 연습#######

str(iris)
# 'data.frame':	150 obs. of  5 variables:    # 150개 측정치, 5개 변수를 가진 DF
#   $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...  # 품종이 3가지라는 것 확인

str(iris)              # 아래와 같이 이루어짐
class(iris)            # 자료구조 확인
head(iris)             # 앞에서 몇개만 잘라서 보는 것
dim(iris)              # 행열의 수
table(iris$Species)    # 각 관측치 도수 분포
summary(c(iris[,1:3]))
summary(iris)
sd(iris[,1])        

# 각 데이터에 대해 그룹별 분포를 확인하고자 할 때
par(mfrow =c(1,1))
boxplot(Sepal.Length~Species,
        data=iris,
        main = "Sepal.Length")
boxplot(Sepal.Width~Species,
        data=iris,
        main = "Sepal.Width")
boxplot(Petal.Length~Species,
        data=iris,
        main = "Petal.Length")
boxplot(Petal.Width~Species,
        data=iris,
        main = "Petal.Width")
#setosa는 Sepal.Width가 비교적 크다.


# 각 열데이터에 대해 그룹별 분포를 산점도 통해 확인할 수 있다.
point <- as.numeric(iris$Species)
color <- c("green", "red", "blue")
pairs(iris[,1:4],
      pch=c(point),
      col=color[iris[,5]]
      )



