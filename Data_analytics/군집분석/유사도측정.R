#################
## 유사도 측정 ##
#################

# 1. 데이터 불러오기
install.packages("flexclust")
library(flexclust)
data(nutrient)
str(nutrient)
# 17종 식품별 에너지 단백질 지방 아연 등
# 다섯 가지 영양소 정보

# 2. 케이스 간 거리 계산
# 거리계산 데이터는 데이터프레임 또는 메트릭스
d <- dist(nutrient, method = "euclidean")
class(d)
as.matrix(d)[1:5, 1:5]
# 행렬 인덱싱 방법으로 보여주기

# 3. 변수값을 재척도화(rescaling)
# 큰 범위를 갖는 변수는 군집분석 결과에 상대적으로 큰 영향
# 서로다른 단위 함께 계산하면 왜곡 가능성
# 소득과 성적으로 유사도 판단 어려움
# 때문에 최소-최대 정규화 필요 or
# Z점수 표준화

# 범주형 변수는 더미변수로 변환해야함
# 재카드거리, 다이스거리로 측정할 수 있음
# 범주형데이터 거리 계산
library(MASS)
data(survey)
str(survey)

# 범주 확인
levels(survey$Sex)
levels(survey$Smoke)

# 범주형변수만으로 구성된 데이터셋 구성 (편의상)
survey.dummy <- survey[c("Sex", "Smoke")]
head(survey.dummy)

# 더미변수 슥삭 만들어주는 패키지 등장
install.packages("fastDummies")
library(fastDummies)

survey.dummy <- dummy_cols(survey.dummy,
                           remove_selected_columns = T, # 기존변수 삭제여부
                           remove_first_dummy = T, # 첫번째 변수를 기준변수
                           ignore_na = T)
head(survey.dummy, 5)

d <- dist(survey.dummy, method = "binary")
as.matrix(d)[1:5, 1:5]

# 범주형변수, 서열형, 연속형 모든 데이터의 거리행렬 구성할 때에는 daisy
install.packages("cluster")
library(cluster)
d <- daisy(survey, metric="gower")
as.matrix(d)[1:5, 1:5]
