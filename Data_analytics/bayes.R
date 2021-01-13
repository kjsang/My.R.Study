#### 나이브베이즈(Naive Bayes) ####

# 베이즈정리(Bayes' theorem) 바탕으로
# 관심 있는 사건이 발생할 확률 추정
# 사전에 알고 있는 정보(예측변수)를 바탕으로
# 특정 사건(결과변수)이 발생할 확률을 계산
# 사건에 해당하는 결과변수는 범주형 변수이어야 하며,
# 예측변수는 범주형 변수를 가정

# 경험에 기반한 선험적, 불확실성 내포하는 수치 기반
# 추가정보를 바탕으로 사전확률 갱신
# 귀납적 추론 방법

install.packages(c("rethinking", "greta"))


# Step 1. All possible ways (likelihood distribution)
# 우도분포 활용한 추정

# 동전을 10번 던져서 앞면이 8번, 뒷면이 2번 나왔을 때, 
# 이 동전의 앞면이 나올 확률 분포를 그려보면 다음과 같다.



rangeP <- seq(0, 1, length.out = 100)
plot(rangeP, dbinom(x = 8, prob = rangeP, size = 10),
     type = "l", xlab = "P(앞면)", ylab = "Density")


# Step 2. Update your belief (prior distribution)
# 그런데, 우리는 동전의 앞뒷면이 보통 0.5라고 생각하지 않는가? 
# 이를 사전확률이라고 말한다 (선입견)
# 선입견을 적용한 확률분포는 어떻게 될까?

lines(rangeP, dnorm(x = rangeP, mean = .5, sd = .1) / 15,
      col = "red")

lik <- dbinom(x = 8, prob = rangeP, size = 10)
prior <- dnorm(x = rangeP, mean = .5, sd = .1)
lines(rangeP, lik * prior, col = "green")   



# Step 3. Make it sum up to one (standardising the posterior)
# 하나로 합치기
# 확률밀도함수 또는 확률질량함수는 하나로 통합할 수 있음
# 확률곱 / 확률합

unstdPost <- lik * prior
stdPost <- unstdPost / sum(unstdPost)
lines(rangeP, stdPost, col = "blue")
legend("topleft", legend = c("Lik", "Prior", "Unstd Post", "Post"),
       text.col = 1:4, bty = "n")




# Define real pars mu and sigma, sample 100x
trueMu <- 5
trueSig <- 2
set.seed(100)
randomSample <- rnorm(100, trueMu, trueSig)
# Grid approximation, mu %in% [0, 10] and sigma %in% [1, 3]
grid <- expand.grid(mu = seq(0, 10, length.out = 200),
                    sigma = seq(1, 3, length.out = 200))
# Compute likelihood
lik <- sapply(1:nrow(grid), function(x){
  sum(dnorm(x = randomSample, mean = grid$mu[x],
            sd = grid$sigma[x], log = T))
})
# Multiply (sum logs) likelihood and priors
prod <- lik + dnorm(grid$mu, mean = 0, sd = 5, log = T) +
  dexp(grid$sigma, 1, log = T)
# Standardize the lik x prior products to sum up to 1, recover unit
prob <- exp(prod - max(prod))
# Sample from posterior dist of mu and sigma, plot
postSample <- sample(1:nrow(grid), size = 1e3, prob = prob)
plot(grid$mu[postSample], grid$sigma[postSample],
     xlab = "Mu", ylab = "Sigma", pch = 16, col = rgb(0,0,0,.2))
abline(v = trueMu, h = trueSig, col = "red", lty = 2)