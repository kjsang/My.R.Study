# 모델을 만들어보자.
pacman::p_load(tidymodels, readr, dotwhisker)
library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

urchins <- # 성게 데이터를 가져와보자
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

ggplot(urchins, # 지지플랏 그려보자.
       aes(x = initial_volume, 
           y = width, 
           group = food_regime, # food regime별 그래프를 그리고 싶어
           col = food_regime)) + # 식량상황으로 컬러
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) + # 선형회귀
  scale_color_viridis_d(option = "plasma", end = .7)
#> `geom_smooth()` using formula 'y ~ x'


# 적합한 모델을 만들어볼까?
# 그 전에, 이 데이터는 연속형인 예측변수와 범주형인 예측변수가 모두 있기 때문에 아노바분석이 적합하다. 한 번 봐야겠다.

linear_reg() %>% #> Linear Regression Model Specification (regression) 
  set_engine("lm") -> lm_mod

lm_fit <-    lm_mod %>%    
  fit(width ~ initial_volume * food_regime, data = urchins)

tidy(lm_fit)

tidy(lm_fit) %>%  
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) 

new_points <- expand.grid(initial_volume = 20,                            food_regime = c("Initial", "Low", "High"))

mean_pred <- predict(lm_fit, new_data = new_points)


conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred
#> # A tibble: 3 x 2
#>   .pred_lower .pred_upper
#>         <dbl>       <dbl>
#> 1      0.0555      0.0729
#> 2      0.0499      0.0678
#> 3      0.0870      0.105

# Now combine: 
plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "urchin size")