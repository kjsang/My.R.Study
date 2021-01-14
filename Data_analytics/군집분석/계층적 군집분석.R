#####################
## 계층적 군집분석 ##
#####################

# 모든 케이스가 각자 하나의 군집
# 이후 단계별 유사군집끼리 합쳐짐
# 최종적으로 하나만 남음

# 하나의 군집 내 두 개의 케이스 포함의 경우
# 단일연결법, 완전연결법, 평균연결법, 중심연결법, 최소분산연결법 등 있음

install.packages("flexclust")
library(flexclust)
data(nutrient)
str(nutrient)
head(nutrient)

# 군집분석 이전에 이름을 모두 소문자로 변경
nutrition <- nutrient
row.names(nutrition) <- tolower (row.names(nutrition)) # 소문자변경경
head(nutrition)

# 측정단위가 다를 때 scale 함수로 바꿔주기
nutrition.scaled <- scale(nutrition)
nutrition.scaled

# 식품 간 거리 계산
d <- dist(nutrition.scaled)

# 계층적 군집분석  
?hclust
# method	
# the agglomeration method to be used. 
# This should be (an unambiguous abbreviation of) 
# one of "ward.D", "ward.D2", "single", "complete", 
# "average" (= UPGMA), "mcquitty" (= WPGMA), 
# "median" (= WPGMC) or "centroid" (= UPGMC).

hclust(d, method = "average")
clustering.average <- hclust(d, method = "average")

plot(clustering.average, hang=1,
     col="darkgreen",
     xlab = "Food",
     main = "Hierarchical Clustering with Average Linkage")


# 엔비클러스트는 최적의 군집 갯수를 알려주는 지표 제공
install.packages("NbClust")
library(NbClust)
nc <- NbClust(nutrition.scaled,
              distance = "euclidean",
              method = "average",
              min.nc = 3, max.nc = 15) # 군집 최대/최소 군집수
# nc$Best.nc는 행렬 형식으로 저장됨
nc$Best.nc

# 지지받은 군집 갯수 구하기
table(nc$Best.nc[1,])
# 0  3  4  5  9 10 13 14 15 
# 2  5  3  7  1  1  2  1  4
# 군집갯수 별 지지 지표를 나타낸다.
# 5개가 좋을 것으로 보임
# 5개 군집으로 나눠보곘음

clusters <- cutree(clustering.average, k=5)
clusters
# 군집으로 할당해줌

table(clusters)
# 1  2  3  4  5 
# 7 16  1  2  1 
# 케이스별 군집 갯수

# 그럼 케이스가 어떻게 군집을 이루는지 시각화
plot(clustering.average, hang=1,
     col="darkgreen",
     xlab = "Food",
     main = "Hierarchical Clustering with Average Linkage")
rect.hclust(clustering.average, k=5)

# 5개 군집의 영양성분별 평균 구하기
aggregate(nutrition, by=list(cluster=clusters), mean)

# 케이스 갯수와 함께 표준화값으로 출력
a <-aggregate(nutrition, by=list(cluster=clusters), mean)
n <- as.vector(table(clusters))
cbind(a, n)
# 해석해보기
# 군집화가 끝날 때까지 소속군집이 안 바뀐다
# 케이스 갯수 많을 때 어려움 (대용량에는 어렵다)
# k평균 군집분석은 이 대안이 된다.