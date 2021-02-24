#도수분포표 작성방법

rm(list=ls()) #함수 다 지우기

sample.data <- c("y","y","y","y","N","N","N")
table(sample.data) #도수분포표
table(sample.data)/length(sample.data) #비율 출력

#데이터에서 바로 도수분포표를 작성하고 싶을 때
table(iris$Species)

#막대그래프를 작성하고 싶다.

MPA <- c("HUFS", "HUFS", "HUFS",
         "KU","KU","KU","KU","KU","KU",
         "KHU","KHU")
# 또는,
#MPA <- c(rep("HUFS",3),
#         rep("KU",6),
#         rep("KHU",2))

MA <-  c("HUFS","HUFS","HUFS","HUFS","HUFS","HUFS",
         "KU","KU",
         "KHU")

sample.data2 <- c(MPA, MA)
sample.data2

plot.sample.data2 <- table(sample.data2)
plot.sample.data2

barplot(plot.sample.data2,
        main = "Master Degree Univ.")

rm(list=ls())

#데이터로 해보겠습니다.
data(mtcars)
carb <- mtcars$carb 
#또는 
#carb <- mtcars[,"carb"]

table(carb)
barplot(table(carb),
        main="Barplot of Carburetors",
        xlab="of carburetors",
        ylab = "frequency")

#barplot의 구조 +색상
barplot(table(carb),
        main="Barplot of Carburetors",
        xlab="of carburetors",
        ylab = "frequency",
        col="red")

#한 화면에 그래프를 여러개 그리고 싶다.
par(mfrow=c(1,3)) #1x3 윈도우 생성
barplot(table(mtcars$carb),
        main="Barplot of Carburetors",
        xlab="of carburetors",
        ylab = "frequency",
        col="red")
barplot(table(mtcars$cyl),
        main="Barplot of Carburetors",
        xlab="of carburetors",
        ylab = "frequency",
        col="blue")
barplot(table(mtcars$gear),
        main="Barplot of Carburetors",
        xlab="of carburetors",
        ylab = "frequency",
        col="green")

#연습1

# 1. infert 데이터셋의 education 컬럼 값을 잘라내어 edu 에 저장한뒤 edu 의 값을 보이시오. 
head(infert)
edu <- infert$education
edu

# 2. edu 에 있는 값들을 중복을 제거하고 보이시오. 
unique(edu)

# 3. edu 에 있는 값들에 대해 도수 분포표를 작성하여 보이시오. 
t.edu <- table(edu)
t.edu

# 4. edu 에 있는 값들에 대해 막대 그래프를 작성하여 보이시오.
barplot(t.edu)




# 도수분포표 : 빈도수(table), 비율(prop.table), 합계구하기(addmargins) 누적비율 등 제시하고 그래프 그리기 

freq <- read.csv("0301.frequency.csv",
                 header = TRUE,
                 na.strings = ".")
str(freq)

freq$grade <- factor(freq$grade,
                     levels = c(1:5),
                     labels = c("F","D","C","B","A"))

str(freq)

###################################
# 돗수분포표 만들기(일변량)

# attach(시작) ~ detach(끝) : 데이터 프레임 활성화 
# freq라는 데이터 프레임을 고정적으로 인식하고 그 안의 변수만을 가지고 작업하고 싶을 때, 이 함수를 사용 하여 작업량 단축
# 항상 attach() detach() 함께 입력하고 데이터 작업 수행 - 에러메시지 뜨면 detach 후 다시 입력 


attach(freq)

# table을 이용한 빈도수 확인, 그래프 만들 때에는 원데이터를 사용하는 것이 아니라 필요한 부분을 table로 인식해야 한다. 

# grade.n <- table(freq$grade) # attach 없을때

grade.n <- table(grade)

table(grade) 

#상대빈도(%) : 데이블을 만들어 놓고, 테이블에서 빈도를 입력하여 결정한다. 

grade.p <- prop.table(grade.n)
grade.p

#빈도 + 상대빈도 : 빈도와 비율을 합치고자 할때 사용 bind

grade.t <- cbind(grade.n,grade.p) # cbind 옆으로 나열하라 (옆에 열추가)
grade.t


grade.t1 <- rbind(grade.n,grade.p) #  rbind 아래로 나열하라 (아래 행추가)
grade.t1

#빈도 + 상대빈도 + 합계


grade.a <- addmargins(grade.t, 1) # cbind에서는 기본 세팅으로 1을 하면 좋을 것 같다. 
# grade.a <- addmargins(grade.t, margin =1) 그냥 1로해도 나온다. 옵션을 알 때 유용하겠군
grade.a

#margin=1 : 열 합계
#margin=2 : 행 합계
#margin=NULL : 전체 합계

rm(grade.t1)

detach(freq) # 항상 마무리 