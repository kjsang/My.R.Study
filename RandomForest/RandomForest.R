install.packages("mlbench")
library(mlbench)
data("BreastCancer")
str(BreastCancer)


bc <- BreastCancer[-1]
bc <- cbind(lapply(bc[-10], function(x) as.numeric(as.character(x))), bc[10])
str(bc)

set.seed(567)
train <- sample(nrow(bc), 0.7*nrow(bc))
bc.train <- bc[train,]
bc.test <- bc[-train,]
table(bc.train$Class)
table(bc.test$Class)

install.packages("randomForest")
library(randomForest)
set.seed(123)
bc.forest <- randomForest(Class ~ ., data = bc.train,
                          na.action = na.roughfix, # 결측값이 수치형일 경우 결측값을 중위수로, 명목형의 경우 빈도수 가장 많은 범주값으로 대체
                          importance = T) # 예측변수의 중요도 평가 위함
bc.forest

# Call:
#   randomForest(formula = Class ~ ., data = bc.train, importance = T,      na.action = na.roughfix) 
# Type of random forest: classification
# Number of trees: 500 # 함수에 의해 생성한 나무개수 (기본) mtree =
# No. of variables tried at each split: 3 # 9개 중 노드분할을 위한 예측변수. 분류분석의 경우 스퀘어값을 선정 mtry = 
# 
# OOB estimate of  error rate: 3.07% 사용되지 않은 데이터(OOB)를 테스트데이터처럼 돌려서 나온 오차값 (추정치) 별도의 테스트데이터 없을 때 활용 가능
# Confusion matrix:
#   benign malignant class.error
# benign       315         9  0.02777778
# malignant      6       159  0.03636364

bc.forest.pred <- predict(bc.forest, newdata = bc.test,
                          type = "prob") # 비율로 출력
head(bc.forest.pred)
bc.forest.pred <- predict(bc.forest, newdata = bc.test,
                          type = "response") # 범주로 출력
head(bc.forest.pred)

table(bc.test$Class, bc.forest.pred,
      dnn=c("Actual", "Predicted"))
mean(bc.test$Class==bc.forest.pred, na.rm=T)

library(cluster)
clusplot(x=na.omit(bc.test[,-10]), clus=na.omit(bc.forest.pred),
                   color = T, shade = T, labels = 4, lines = 0,
                   main = "Random Forest")
bc.forest.predAll <- predict(bc.forest,
                             newdata = bc.test,
                             predict.all = T) # 개별케이스들이 어떻게 분류되었는지 분류결과 확인 가능
str(bc.forest.predAll)

bc.forest.predAll$individual[6,] # 6번째 케이스에서 판정결과
table(bc.forest.predAll$individual[6,]) # 판정숫자 비교
na.omit(bc.forest.predAll$aggregate)[6] # 양성으로 판정되어있음

apply(bc.forest.predAll$individual[21:25,], 1, table)
na.omit(bc.forest.predAll$individual)[21:25]

varImpPlot(bc.forest, pch=21, color="black", bg="red",
           main="Variable Importance for Breast Cancer")
importance(bc.forest) # 노드의 순도계선에 기여하는 정도
# 각 집단별 변수의 의 중요도
# MeanDecreaseAccuracy 정확도 기여
# MeanDecreaseGini 순도 기여
importance(bc.forest, tape = 1)
importance(bc.forest, tape = 2)

install.packages("gplots")
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(bc.forest)[,1:2]),
          col = brewer.pal(9, "Blues"),
          dend="none", trace = "none", key = F,
          margins = c(10, 7), cexRow = 1.5, cexCol = 1.2,
          colRow = c("green4", "maroon"),
          main = "Variable Importance\nfor Breast Cancer Classification")
