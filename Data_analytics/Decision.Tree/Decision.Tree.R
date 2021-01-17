###################
## Decision Tree ##
###################

# 1. 패키지 적재
install.packages("mlbench")
library(mlbench)

111
# 2. 데이터 확인
data(BreastCancer)
str(BreastCancer)
# 699 관측값, 11개 변수
# 유방암 의심환자 세포조직 검사 및 판정결과
# ID: 환자 아이디
# 세포조직의 특성 8가지
btc <- BreastCancer

table(btc$Class)  
# 양성 358 악성 241
mean(btc$Class=="benign") # 양성비율 65.5%
mean(btc$Class=="malignant") # 악성비율 34.4%
sum(!complete.cases(btc)) # 결측값 갯수


# 분석에 불필요한 아이디 없앰
bc <- BreastCancer[-1]
# 펙터에서 숫자변수로 변환
bc <- cbind(lapply(bc[-10],
                   function(x) as.numeric(as.character(x))),
            bc[10])
str(bc)


# 3. 데이터 분할
set.seed(1)
train <- sample(nrow(bc), 0.7*nrow(bc))
bc.train <- bc[train,]
bc.test <- bc[-train,]


# 4. 의사결정나무 패키지 적재
# install.packages("rpart")
library(rpart)


# 5. 의사결정나무 분석
bc.dtree <- rpart(formula = Class ~.,
                  data = bc.train,
                  method = "class", #범주형변수의 분류나무 #회귀나무는 아노바
                  parms = list(split="information"))
bc.dtree
# node), split, n, loss, yval, (yprob)
# 노드의 번호), 분할점(의사결정기준), 분할점에 의해 만들어지는 케이스갯수, 잘못불류된 갯수,
# 분류판정과 결과변수 비율
# Cell.size< 3.5 329  21 benign (0.93617021 0.06382979)


# 6. 나무그리기
install.packages("rpart.plot")
library(rpart.plot)
prp(bc.dtree, # 데이터
    type = 2, # 2는 분기점 레이블이 바로 아래에 표기
    extra=104, # 각 노드의 범주별 비율과 케이스의 비율 알 수 있음
    fallen.leaves = T, # 그래프 하단에 최종노드 정렬
    roundint = F, # 지정 안 하면 정수로 반올림 된다 (꼭 해주자)
    main = "Decision Tree from Wisconsin Breast Cancer Dataset")

bc.dtree.pred <- predict(bc.dtree,
                         newdata = bc.test,
                         type = "prob") # 집단출력은 class
head(bc.dtree.pred)

bc.dtree.pred <- predict(bc.dtree,
                         newdata = bc.test,
                         type = "class") 
head(bc.dtree.pred)

table(bc.test$Class, bc.dtree.pred, dnn = c("Actual", "Predicted"))
mean(bc.test$Class==bc.dtree.pred)

# 새로운 데이터에 대해서는 작동 어려움
# 교차검정을 할 필요 있음
# 교차검정하는 과정을 가지치기라고 할 수 있다.
bc.dtree$cptable
printcp(bc.dtree)
# cptable 에는 다양한 나무 크기에 대한 예측오차, 나무크기 등 정보
# nsplit: 
# cp: 큰 나무에 대한 패널티 (적정나무크기 결정)
# cp 인수에 따로 인수를 지정하면 됨 (0은 최대한도까지 커짐)
#    cp가 클 수록 나무 작아지고, 지나치게 크면 과소적합문제 발생
#    기본은 0.01
# 교차타당오차(xerror): 10번 자체검증, 가작 작은 오차의 표준편차 +-1 모델 선정
plotcp(bc.dtree)  # cp 한계표준오차 상한값
# 적정 나무크기 조정할 때, 나무크기 4인 모델을 선택하게 됨
# 최종노드가 4개라는 뜻


bc.dtree.pruned <- rpart(
  formula = Class ~ .,
  data = bc.train,
  method = "class",
  cp= 0.01775148, # 3번째꺼 선택
  parms = list(split = "information")
)

bc.dtree.pruned <- prune(bc.dtree, cp = 0.01775148)
bc.dtree.pruned
bc.dtree.pruned$cptable
plotcp(bc.dtree.pruned)


# 그래프로 아름답게 그려볼까?
 cols <- ifelse(bc.dtree.pruned$frame$yval==1,
                "green4", "darkred") # 악성 양성 다르게
 prp(bc.dtree.pruned,
     type = 2,
     extra = 104,
     fallen.leaves = T,
     roundint = F,
     branch.lty = 3, # 가지를 점선으로
     col = cols, # 악성 양성 색깔 다르게 하자
     border.col = cols, # 악성 양성 색깔
     shadow.col = "gray", # 그림자처리
     split.cex = 1.2, # 분할박스 관련 인수, 텍스트크기
     split.suffix = "?", # 분할박스 끝 문자, 텍스트에 물음표가 들어가있다
     split.box.col = "lightgray", # 음영처리
     split.border.col = "darkgray", # 경계선 색깔
     split.round = 0.5, # 박스의 모서리 둥글게 둥글게
     main = "Pruned Decision Tree from Wisconsin BC Dataset")
 

library(rattle)
fancyRpartPlot(bc.dtree.pruned, sub = NULL)


bc.dtree.pred <- predict(bc.dtree.pruned,
                         newdata = bc.test,
                         type = "class") 
table(bc.test$Class, bc.dtree.pred, dnn = c("Actual", "Predicted"))
mean(bc.test$Class==bc.dtree.pred)
# 예측정확도는 이전과 동일
# 단순한 모델, 정확한 예측 굿