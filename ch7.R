####데이터시각화####

# 데이터 분석 후 눈으로 볼 수 있는 형태의 결과물로 제시하는 것
# 데이터를 그림 형태로 보여주면 의미파악이 쉬움

####나무지도(tree map)####

# 사각형 안 면적 및 색상에 정보를 표현한다.
# 면적이 크다 = 값이 크다
# 색상이 진하다 = 값이 크다

# 대륙별 여러 나라의 인구와 소득 등을 나타낼 수 있음
install.packages("treemap")
library(treemap)
data(GNI2014)                            # 데이터 불러오기
str(GNI2014)                             # 데이터 정보
head(GNI2014)
treemap(GNI2014,                         # 대상 데이터
        index = c("continent", "iso3"),  # 계층인덱스 (매개변수) 대륙 국가
        vSize = "population",            # 타일의 상대적 크기
        vColor = "GNI",                  # 타일의 컬러 (1인당 소득)
        type="value",                    # 타일 컬러링 방법 (VColor 값에따라)
        bg.labels = "yellow")            # 레이블 배경색 (상위계층)

# 대륙별 평균소득은?
GNI2014$GNI.total <- GNI2014$population*GNI2014$GNI
options("scipen" = 100)
head(GNI2014)

# 국가별 국민 총소득을 대륙별 합계 GNI.A
GNI.A <- aggregate(GNI2014[,4:6],        # 그룹별 집계 내기
                   by=list(GNI2014$continent), sum) # 집계의 기준 (합계)

# 대륙별 합계를 대륙 인구수로 나눠 GNI.percap 저장
GNI.A$GNI.percap <- 
  GNI.A$GNI.total/GNI.A$population

head(GNI.A)
treemap(GNI.A,
        index = c("Group.1"),
        vSize = "population",            # 타일의 상대적 크기
        vColor = "GNI.percap",           # 1인당 평균 소득
        type="value",                    # 타일 컬러링 방법 (VColor 값에따라)
        bg.labels = "yellow")  


####버블차트####

# 버블차트는 이차원 좌표 상 버블을 표시
# 원의 크기를 가지고 어떤 숫자를 표현
# 원이 클 수록 숫자가 크다

rm(list=ls())

install.packages("MASS")
library(MASS)
data(UScrime)
head(UScrime)
radius <- sqrt(UScrime$Pop)                 # 원의 반지름(값이 커서 줄임)
symbols(UScrime$U2,UScrime$y,               # x,y 좌표값
        circles = radius,                   # 원의 반지름 값
        inches = 0.4,                       # 원의 크기 조절값
        fg = "white",                       # 원의 테두리 색 (없는거나 마찬가지)
        bg = "lightgray",                   # 원의 바탕색
        lwd = 1.5,                          # 원의 테두리선 두께
        xlab = "unemployment 35-39 males",  
        ylab = "crime rate",
        main = "UScrime Data"
        )
text(UScrime$U2,                            # 텍스트 출력 (x좌표)
     UScrime$y,                             # 텍스트 출력 (y좌표)
     1:nrow(UScrime),                       # 출력할 텍스트
     cex = 0.8,                             # 폰트 크기
     col = "brown")                         # 폰트 색상

# 실업률과 범죄율 사이의 상관관계
# 원의 넓이는 인구수(pop)
# 주의 번호가 1:nrow(UScrime)
# 인구수가 많으면서 실업률이 높으면 그럴 수도... (상관관계 x)


####다중 상자그림####

# boxplot
# 색상을 입혀서 의미를 줘보도록 하겠음
# 2017년 서울의 월별기온분포 데이터 활용

ds <- read.csv("ch7_data.csv")
head(ds)
summary(ds)
summary(ds$avg_temp)

boxplot(ds$avg_temp,
        col = "yellow",
        ylim = c(-20,40),        # y축을 제한함
        xlab = "서울 1년 기온",
        ylab = "기온")

# 월별 월평균 기온 boxplot 작성
# 월평균 기온이 높을수록 빨강, 낮을수록 노랑

# 월별 평균기온 계산
month.avg <- aggregate(ds$avg_temp,
                       by=list(ds$month), median)[2]   # 평균값이 [2]임
# 평균기온의 순위 계산
month.order <- rank(-month.avg) # -는 내림차순   

boxplot(avg_temp~month, data = ds,
        col=heat.colors(12)[month.order],    # 열온도 함수, 12개 색 가져오기
        ylim=c(-20,40),
        main="서울 월별 기온분포(2017)",
        xlab = "월",
        ylab = "기온")

# 1월은 기온변동폭이 넓다.
# 8월은 빨간색이나 기온차가 꽤 있다.
# 11월은 환절기 기온 변동폭이 크다.


#### aggregate 함수 ####
aggregate(data, by='기준이 되는 칼럼', function)

# cyl 컬럼을 기준으로 나머지 컬럼의 평균값 구하기
aggregate(mtcars, 
          list(cylStandard = mtcars$cyl),       # by = 안 해도 된다.
          mean)

# cyl 기준 mpg 평균 구하기
aggregate(mtcars$mpg,
          list(cylStandard = mtcars$cyl),       # by = 안 해도 된다.
          mean)

# disp 칼럼 120 이상만 출력
aggregate(mtcars, 
          list(cylStandard = mtcars$cyl,        # 조건 다수: list
               dispHigh = mtcars[,'disp'] >120),mean)

# formula 사용
aggregate("적용col"~"기준col", data, function)
# x ~ y 는 y를 기준으로 x를 구한다는 뜻

# 1) one ~ one: cyl 기준 wt 평균 구하기
aggregate(wt~cyl, data = mtcars, mean)

# 2) one ~ many  # 그냥 + 해준다.
aggregate(wt~carb+gear, data = mtcars, mean)

# 3) many ~ one  # cbind로 묶어준다.
aggregate(cbind(disp, wt) ~ gear, data = mtcars, mean)

# 4) many ~ many 
aggregate(cbind(disp,wt)~ carb + gear, data= mtcars, mean)



####모자이크 플롯####

# 면적을 통해 변수의 관계 확인
# 2원 3원 교차표의 시각화
# 교차표에 따라 빈도를 넓이로 표현

# 매트릭스 형태로 데이터가 존재하는 경우

data(UCBAdmissions)
table(UCBAdmissions)
## Data aggregated over departments
apply(UCBAdmissions, c(1, 2), sum)
mosaicplot(apply(UCBAdmissions, c(1, 2), sum),
           main = "Student admissions at UC Berkeley",
           color = TRUE)
## Data for individual departments
opar <- par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
for(i in 1:6)
mosaicplot(UCBAdmissions[,,i],
           xlab = "Admit", ylab = "Sex",
           main = paste("Department", LETTERS[i]),
           color = TRUE)
letters

par(mfrow = c(1, 1))
rm(list = ls())
data(Titanic)
mosaicplot(Titanic, color = c("red", "green"), # 사망자, 생존자
           off = 5)   # 박스 간 간격을 조정하는 매개변수

# 승무원의 비율이 높음
# 일등칸 남성 사망자 많음, 여성 대부분 생존
# 이등칸 남성 대부분 사망, 여성 생존 높음
# 남자 승무원 대부분 사망


#### ggplot ####
# 배우는데 시간이 많이 걸린다. 
# 각잡고 이것만 배워야 한다.

head(iris)

# 1) Scatterplot
plot(x=iris$Sepal.Length, y=iris$Sepal.Width, 
     xlab="Sepal Length", ylab="Sepal Width",  main="Sepal Length-Width")


install.packages("ggplot2")
library(ggplot2)
qplot(x = Sepal.Length, y = Sepal.Width, data = iris, 
      xlab="Sepal Length", ylab="Sepal Width", 
      main="Sepal Length-Width", color=Species, shape=Species)

scatter <- ggplot(data=iris,
                  aes(x = Sepal.Length, y = Sepal.Width)) 
scatter +
  geom_point(aes(color = Species, shape = Species)) +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

scatter + geom_point(aes(color = Petal.Width, shape = Species), size = 2, alpha = I(1/2)) +
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = mean(Sepal.Width)), color = "red", linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red") +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")


# 2) Box plot

boxplot(Sepal.Length~Species,data=iris, 
        xlab="Species", ylab="Sepal Length", main="Iris Boxplot")

library(ggplot2)
box <- ggplot(data=iris, aes(x=Species, y=Sepal.Length))
box + geom_boxplot(aes(fill=Species)) + 
  ylab("Sepal Length") + ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 


# 3) Histogram
hist(iris$Sepal.Width, freq=NULL, density=NULL, breaks=12,
     xlab="Sepal Width", ylab="Frequency", main="Histogram of Sepal Width")

library(ggplot2)
histogram <- ggplot(data=iris, aes(x=Sepal.Width))
histogram + geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Width") +  ylab("Frequency") + ggtitle("Histogram of Sepal Width")


# 4) Bar Plot
set.seed(1234)
iris1 <- iris[sample(1:nrow(iris), 110), ]
hline <- data.frame(Species=c("setosa", "versicolor", "virginica"),
                    hline=as.vector(table(iris$Species)))
hline
barplot(table(iris1$Species), col="black",
        xlab="Species", ylab="Count",
        main="Bar plot of Sepal Length")

library(ggplot2)
bar <- ggplot(data=iris1, aes(x=Species))
bar + geom_bar() + 
  xlab("Species") +  ylab("Count") + ggtitle("Bar plot of Sepal Length") +
  geom_errorbar(data=hline,
                aes(y=hline, ymin=hline, ymax=hline),
                col="red", linetype="dashed")

# 등등... 
# https://www.publichealth.columbia.edu/sites/default/files/media/fdawg_ggplot2.html
# 참고