# 의사결정나무 분석

install.packages("rpart")
install.packages("rattle")
install.packages("FSelector")

library(rattle)
library(rpart.plot)
library(FSelector)

# 분류는 데이터 분석에서 빈번히 활용
# 가장 직관적인 분석방법은 의사결정나무

# Decision Tree
# 기계학습
# 의사결정규칙을 나무형태로 분류해 나가는 분석 기법
# 장점1: 직관적, 이해가 쉬움
# 장점2: 수치형, 범주형 모두 사용 가능
# 장점3: 대규모 데이터 빠르게 연산 가능

# 분석방법
# 1. 데이터 준비
# 2. 의사결정나무만들기
# 3. 가지치기
# 4. 예측 및 모델 평가

# 1. 데이터 준비 (흉부외과 관찰 데이터)
# AHD 칼럼에 심장병 여부 기록 (n = 303)
hr<-read.csv("Heart.csv", header = T)
head(hr)

a<-information.gain(AHD~., hr[,-1])


# 2. 의사결정나무만들기
tree <- rpart(AHD~.,data=hr[,-1], control=rpart.control(minsplit=2))
fancyRpartPlot(tree)

# 데이터 분석
# 지중해성 빈혈이 심할수록 심장질환 가능성 증가
# 가슴통증은 심장질환 판별 시 가장 영향력 있는 변수


또다른 방법

install.packages("tree")
install.packages("ISLR")

library(tree)
library(ISLR)

data(Carseats)
#?Carseats
str(Carseats)

Carseats$Sales = as.factor(ifelse(Carseats$Sales <= 8, "Low", "High"))
str(Carseats)


seat_tree = tree(Sales ~ ., data = Carseats)
# seat_tree = tree(Sales ~ ., data = Carseats, 
#                  control = tree.control(nobs = nrow(Carseats), minsize = 10))
summary(seat_tree)
plot(seat_tree)
text(seat_tree, pretty = 0)
title(main = "Unpruned Classification Tree")

dim(Carseats)


set.seed(2)
seat_idx = sample(1:nrow(Carseats), 200)
seat_trn = Carseats[seat_idx,]
seat_tst = Carseats[-seat_idx,]

seat_tree = tree(Sales ~ ., data = seat_trn)
summary(seat_tree)
summary(seat_tree)$used
plot(seat_tree)
text(seat_tree, pretty = 0)
title(main = "Unpruned Classification Tree")



seat_trn_pred = predict(seat_tree, seat_trn, type = "class")
seat_tst_pred = predict(seat_tree, seat_tst, type = "class")
#predict(seat_tree, seat_trn, type = "vector")
#predict(seat_tree, seat_tst, type = "vector")

# train confusion
table(predicted = seat_trn_pred, actual = seat_trn$Sales)

# test confusion
table(predicted = seat_tst_pred, actual = seat_tst$Sales)

accuracy = function(actual, predicted) {
  mean(actual == predicted)  
}
