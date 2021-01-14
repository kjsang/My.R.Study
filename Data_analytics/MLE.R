# 1. 정규분포 ----------------------------------------
# https://www.r-bloggers.com/normal-distribution-functions/
library(tidyverse)

xseq <- seq(-4, 4, .001)
normal_densities <- dnorm(xseq, 0,1)
normal_cumulative <- pnorm(xseq, 0, 1)
normal_smpl <- rnorm(length(xseq), 0, 1)

normal_df <- data.frame(xseq, normal_densities, normal_cumulative, normal_smpl)

normal_density_p <- normal_df %>% 
  ggplot(aes(x=xseq, y=normal_densities)) +
  geom_line(color="darkgreen") +
  theme_bw(base_family="NanumGothic") +  
  labs(x = '데이터 (x)', y ='밀도', title="정규분포 확률밀도함수")

normal_cummulative_p <- normal_df %>% 
  ggplot(aes(x=xseq, y=normal_cumulative)) +
  geom_line(color="darkorange") +
  theme_bw(base_family="NanumGothic") +  
  labs(x = '데이터 (x)', y ='누적확률', title="정규분포 누적밀도함수")

normal_sample_p <- normal_df %>% 
  ggplot(aes(x=normal_smpl)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = 'darkblue', alpha=0.5) +
  theme_bw(base_family="NanumGothic") +  
  labs(x = '데이터 (x)', y ='표본수', title="표준정규분포에서 추출한 난수")

normal_data_fit_p <- ggplot(normal_df, aes(normal_smpl)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = 'darkblue', alpha=0.5) +
  theme_bw(base_family="NanumGothic") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = '데이터', y ='빈도', title="정규분포 - 데이터 + 정규분포곡선") +
  stat_function(
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd) * n * bw
    }, args = c(mean = mean(normal_df$normal_smpl, na.rm = T), sd = sd(normal_df$normal_smpl, na.rm =T), n = length(xseq), bw = 0.26))

gridExtra::grid.arrange(normal_density_p, normal_sample_p, normal_data_fit_p, normal_cummulative_p, nrow=2)


# 2. 데이터에 적합한 분포 검정 및 모수 추정-------------------
# ---------------------descdist(normal_smpl, discrete = FALSE)
install.packages("fitdistrplus")
library(fitdistrplus)

descdist(normal_smpl, discrete = FALSE)
normal_smpl_fit <- fitdist(normal_smpl, "norm", method="mle")
plot(normal_smpl_fit)
