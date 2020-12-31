# R로 하는 프로그래밍의 기본
# R은 데이터분석 도구이면서 프로그램 언어이다.
# 프로그래밍이란?: 컴퓨터가 문제를 해결할 수 있도록 절차를 서술해 놓은 것
# 프로그램을 만드는 과정을 프로그래밍이라고 한다.

####if문####

# 조건문: 참이면 실행, 거짓이면 다른 실행
# 참, 거짓 여부에 따라 다른 처리가 필요할 때 사용한다.

# if(logical expression) {  # 조건문
#   statements              # 참이면 실행
# } else {
#   alternatie statements   # 거짓이면 실행
# }

# 예시1
a <- 10
if (a>5) {
  print(a)    # 참이면 실행
} else {
  print(a/10)
  print(a*10) # 거짓이면 실행
}

#예시2
a <- 10
b <- 20
if(a>5 & b>5) {    # and
  print (a+b)
}
if(a>15 | b>15) { b  # or
  print (a*b)
}

#예시3 ifelse()
a <- 10
b <- 20
ifelse(a>b, c <- a, c <- b) #(조건, 참, 거짓)


####반복문####
for(i in 1:10) {
  print(i)
}

# 구구단 만들기
for (i in 1:10) {
  cat("2*",i,"=",2*i,"\n")
}


# 짝수인지 확인
for(i in 1:20) {
  if(i%%2==0) {
    print(i)
  }
}

# 반복문과 if 응용
v1 <- 101:200
for(i in 1:length(v1)) {
  if(v1[i]%%2==0) {
    print(v1[i]*2)
  } else {
    print(v1[i]+2)
  }
}

# 1부터 100까지 합 구하기 (수열)
sum <- 0
for(i in 1:200) {
  sum <-  sum + i
}
print(sum)

# 반복문 while
# 특정 조건이 만족될 때까지 반복하는 것
i <- 1
while(i <= 10) {
  print(i)
  i <- i+1
}

#iris 데이터에서 Sepal.Length가 5~6사이인 표본만 골라보기
data(iris)
subset(iris, Sepal.Length >= 5.0 &
         Sepal.Length <= 6.0)[, 1:2]

idx <- c()