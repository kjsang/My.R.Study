####데이터 전처리####

# 분석을 위한 데이터셋 확보했다.
# 하지만 결측값, 이상치, 오입력 등 문제 많음
# 데이터셋 정제를 전처리라고 함
# 분석보다 보통 오래 걸린다.
# 전처리능력이 효율성을 위해 중요

#### 결측값(missing value) ####

# 결측값: 데이터셋에서 입력이 누락된 값
# NA 로 표시
# NA 포함된 경우 연산에 문제

# 벡터에서의 NA
x <- c(1,2,3,NA,5,6,7,8)
sum(x)
sum(x, na.rm=T)  # 결측값 제외하고 연산
sum(is.na(x)) # 결측값 포함 개수 확인
x[is.na(x)] <- 0  # NA를 0으로 치환
x

# 2차원에서의 NA
x <- iris
x[1,2] <- NA; x[1,3] <- NA
x[2,3] <- NA; x[3,4] <- NA
head(x)

#결측값 세기
col_na <- function(y) {
  return(sum(is.na(y)))
}
# sapply 사용 (벡터형태로 칼럼별 함수값 반환)
na_count <- sapply(x, FUN=col_na)
na_count

#결측값(이 포함된 row) 제외한 dataset 생성
complete.cases(x)
x[!complete.cases(x),]
complete.x <- x[complete.cases(x),]
summary(complete.x)

# 결측값을 적당한 값으로 추정해 치환
x <- iris
x[1,2] <- NA; x[1,3] <- NA
x[2,3] <- NA; x[3,4] <- NA
head(x)

# install.packages("mice")
library(mice)
md.pattern(x)
result <- mice(x, m=5, maxits= 50,
               method = "pmm", seed = 500)
?mice
result <- mice(x)
impute_x <- complete(result, 2)
head(x)
head(impute_x)
head(iris)


#### 이상치(outlier) ####
# 정상 범위 밖에 있는 값
# 입력 실수일 수도, 실제 값일 수도
# 전체 데이터 분포 특성에 영향
# 불량찾을 때
# 사기거래 탐지 (은행거래)
# 이상치 제외할지, 포함할지 판단 필요

# 순서
# 1. 논리적으로 있을 수 없는 값이 있나? ex) 몸무게 -34kg
# 2. 상식에 벗어난 값? 신입사원 나이 127살
# 3. boxplot 활용

# 이상치 보기
st <- data.frame(state.x77)
boxplot(st$Income)
boxplot.stats(st$Income)$out

# 이상치를 NA처리한 후 NA 포함 행 제거
out.val <- boxplot.stats(st$Income)$out
st$Income[st$Income %in% out.val] = NA
st$Income
newdata <- st[complete.cases(st), ]
boxplot(newdata$Income)



#### 정렬(sort, order, rank) ####

iris$Sepal.Length
order(iris$Sepal.Length)
sort(iris$Sepal.Length) # 오름차순
sort(iris$Sepal.Length, # 내림차순
     decreasing = T)
iris[order(iris$Sepal.Length, decreasing = F),]
iris[order(iris$Species, iris$Sepal.Length),]

# order: 정렬 시 순서값
# rank: 현재 값의 순위
a <- sample(1:50, 10)
a
a[9] <- NA
sort(a)
rank(a)
order(a)
rank(a, na.last=T) # NA값 제거 (F, keep, NA)
order(a, na.last = "NA") # NA값 제거 (T, F, NA)


#### 분리(split), 선택(subset) ####
# split: 주어진 기준에 따라 여러 개로 분리
sp <- split(iris, iris$Species)
sp   # list형태

summary(sp)
class(sp)  # 데이터 특성 확인
sp$setosa

# subset: 조건에 맞는 행(row) 추출
subset(iris, Species == "setosa")
subset(iris, Sepal.Length > 5.1)
subset(iris, Sepal.Length > 5.1,
       select = c(Petal.Length, Petal.Width))
# 요것만 보겠다 하면 select = 볼 것

# which 조건에 맞는 값의 인덱스
x <- sample(1:10, 7)
x
which(x>5) # 조건에 맞는 값의 "인덱스" 산출
which(iris$Species=="setosa")
iris
which.max(iris$Sepal.Length)
which.min(iris$Sepal.Length)


#### 샘플링(sampling) ####
# 데이터 일부를 임의 추출(비복원 추출)
x <- 1:100
y <- sample(x, size = 10, replace = F)
# replace 생략 가능
y

# iris에서 50개 행 임의추출
iris.sample.index <- sample(nrow(iris), 50)
iris.sample.index
iris.sample <- iris[iris.sample.index,]
head(iris.sample)
iris.sample

# combination
combn(5,3) # 5개 중 3개 뽑는 조합
x = c("red", "green", "blue", "black", "white")
com <- combn(x, 2)
com
for(i in 1:ncol(com)) {
  cat(com[,i], "\n")
}

#### 데이터 요약(aggregate) ####
# 2차원 데이터를 기준변수(칼럼)에 따라 집계
# iris 품종별 꽃잎/꽃받침 폭/길이 평균
agg <- aggregate(iris[,1:4],
                 by=list(iris$Species),
                 FUN = mean)
agg

letters[c(1:3)]

#### 데이터 병합(merge) ####
# 공통 칼럼 매개로 2개의 2차원 배열을 병합
x <- data.frame(name=letters[c(1:3)],
                math = c(90,80,40))
y <- data.frame(name=letters[c(1:4)],
                korean = c(80,70,50,50))
x
y

merge(x,y, by=c("name"))
merge(x,y, by=c("name"), all.x = T)
merge(x,y, by=c("name"), all.y = T) # 결측값
merge(x,y, by=c("name"), all = T) # 결측값

# 칼럼 이름 다른 경우
x <- data.frame(name=letters[c(1:3)],
                math = c(90,80,40))
y <- data.frame(nickname=letters[c(1:4)],
                korean = c(80,70,50,50))
merge(x,y,
      by.x=c("name"),
      by.y = c("nickname"))


