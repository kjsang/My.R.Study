# 데이터마이닝
# 데이터마이닝 도구 학습을 통해 다양한 정보 수집가능
# 예측(회귀분석), 분류, 군집화 등

str(cars)
plot(dist~speed, data = cars)
model <- lm(dist~speed, cars)
  # dist: 종속변수
  # speed: 독립변수
model

# 회귀식의 형태는
# y - Wx + b
coef(model) [1] #b
coef(model) [2] #W

# 회귀식은 y = 3.932x -17.579
# dist = 3.932*speed - 17.579

#Q. 속도가 각각 30,35,40일 때 예상 제동거리는?
speed <- c(30, 35, 40)
dist <- 3.932*speed - 17.579
print(dist)

# 모델의 예측값과 실제 값 사이 어느정도 차이가 날까?
speed <- cars[,1]
prediction.dist <- 3.932*speed - 17.579
prediction.dist
compare <- cbind(prediction.dist, cars[,2], abs(prediction.dist-cars[,2]))
compare

#실제로 그려보기
plot(dist~speed, data=cars)
abline(coef(model))


####중선형회귀####

# 독립변수가 2개 이상인 경우
# 특정 직군의 연봉을 3가지 변수(교육년수, 여성비율, 평판)
# 가지고 예측해보기

install.packages("car")
library(car)
head(Prestige)
new.data <- Prestige[c(1:4)]
plot(new.data, pch = 16,
     col="blue",
     main = "Matrix Scatterplot")

# 연봉예측모델

model.income <- lm(income ~ education + prestige + women,
                   data = new.data)
summary(model.income)

회귀식은
predict.income = 177.199*Prestige$education + 141.435*Prestige$prestige -50.896*Prestige$women -253.850

# 어떤 직군의 평균 교육연수가 9.5년, 여성비율 20%, 평판도 80이라면 예상연봉은?

# Call:
#   lm(formula = income ~ education + prestige + women, data = new.data)
# 
# Residuals:
#   Min      1Q  Median      3Q 
# -7715.3  -929.7  -231.2   689.7 
# Max 
# 14391.8 
# 
# Coefficients:
#   Estimate Std. Error
# (Intercept) -253.850   1086.157
# education    177.199    187.632
# prestige     141.435     29.910
# women        -50.896      8.556
# t value Pr(>|t|)    
# (Intercept)  -0.234    0.816    
# education     0.944    0.347    
# prestige      4.729 7.58e-06 ***
#   women        -5.948 4.19e-08 ***  income 설명하는데 얼마나 중요한가?
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’
# 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2575 on 98 degrees of freedom
# Multiple R-squared:  0.6432,	Adjusted R-squared:  0.6323  모델의 설명력
# F-statistic: 58.89 on 3 and 98 DF,  p-value: < 2.2e-16 모델의 유의확률


# 독립변수가 많을 때 골라내는 작업
library(MASS)
new.data2 <- Prestige[,c(1:5)]
head(new.data2)
model.income2 <- lm(income ~ education+prestige+women+census,
                    data = new.data2)
step <- stepAIC(model.income2, direction = "both")

# Start:  AIC=1607.93
# income ~ education + prestige + women + census 첫번째
# 
# Df Sum of Sq       RSS
# - census     1    639658 649654265
# - education  1   5558323 654572930
# <none>                   649014607
# - prestige   1 143207106 792221712
# - women      1 212639294 861653901
# AIC
# - census    1606.0
# - education 1606.8
# <none>      1607.9
# - prestige  1626.3
# - women     1634.8
# 
# Step:  AIC=1606.03
# income ~ education + prestige + women 두번째
# 
# Df Sum of Sq       RSS
# - education  1   5912400 655566665
# <none>                   649654265
# + census     1    639658 649014607
# - prestige   1 148234959 797889223
# - women      1 234562232 884216497
# AIC
# - education 1605.0
# <none>      1606.0
# + census    1607.9
# - prestige  1625.0
# - women     1635.5
# 
# Step:  AIC=1604.96
# income ~ prestige + women 최종
# 
# Df Sum of Sq        RSS
# <none>                    655566665
# + education  1   5912400  649654265
# + census     1    993735  654572930
# - women      1 234647032  890213697
# - prestige   1 811037947 1466604612
# AIC
# <none>      1605.0
# + education 1606.0
# + census    1606.8
# - women     1634.2
# - prestige  1685.1

model.income3 <- lm(income ~ prestige+women,
                    data = new.data2)
summary(model.income3)


# 로지스틱 회귀분석
# 종속변수가 범주형 데이터일 때
# 예) iris 품종 예측

head(iris)
# 종속변수가 숫자형 이어야 함 . 범주형 변수를 숫자로 변환
mod.iris <- glm(as.integer(Species) ~.,
            data= iris)
summary(mod.iris)

# Call:
#   glm(formula = as.integer(Species) ~ ., data = iris)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.59215  -0.15368   0.01268   0.11089   0.55077  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.18650    0.20484   5.792 4.15e-08 ***
#   Sepal.Length -0.11191    0.05765  -1.941   0.0542 .  
# Sepal.Width  -0.04008    0.05969  -0.671   0.5030    
# Petal.Length  0.22865    0.05685   4.022 9.26e-05 ***
#   Petal.Width   0.60925    0.09446   6.450 1.56e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.04800419)
# 
# Null deviance: 100.0000  on 149  degrees of freedom
# Residual deviance:   6.9606  on 145  degrees of freedom
# AIC: -22.874
# 
# Number of Fisher Scoring iterations: 2

# 임의의 품종 예측해보기

# 1
pred.iris <-1.18650 + 5.1 * (-0.11191) +
            3.5 * (-0.04008) +
            1.4 * (0.22865 +
            0.2 * 0.60925)
pred.iris
# 1에 가까우므로 1(setosa)로 판단
unique(iris$Species)
as.integer(unique(iris$Species))

# 2
unknown <- data.frame(rbind(c(5.1, 3.5, 1.4, 0.2)))
names(unknown) <- names(iris) [1:4]
unknown
mod.iris
pred.iris2 <- predict(mod.iris, unknown)
pred.iris2

# 3 (여러 개)

test.iris <- iris[,1:4]
pred.iris3 <- predict(mod.iris, test.iris)
pred.iris3
pred.iris3 <- round(pred.iris3,0)
pred.iris3 # 순서별 예측 품종이 나온다.
#얼마나 성공했나?
pred.iris3 == as.integer(iris[,5])
acc <- mean(pred.iris3 == as.integer(iris[,5]))
acc

# 주의할 점
# 종속변수는 숫자여야 하기에 문자형의 경우 숫자로 변환 후 작업
# 범주형 데이터가 factor의 경우 as.integer()로 숫자변환 가능
class(iris$Species)
iris$Species
as.integer(iris$Species)

#### 군집화 ####


#### k-means clustering ####


#### KNN classification ####


#### k-fold cross validation ####
