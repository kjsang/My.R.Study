##### Chapter 3: Classification using Nearest Neighbors --------------------
pacman::p_load("class", "gmodels", "tidyverse")
## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
wbcd <- read.csv(".//Chapter03//wisc_bc_data.csv", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
wbcd 
# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# confirm that normalization worked
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)




# 타이디문법으로 재구성
install.packages("modeldata")
pacman::p_load("dplyr", "class", "gmodels", "tidyverse", "ggplot2", "rsample", "recipes", "caret", "modeldata") # 없는 패키지는 자동으로 다운로드받아줌
library(modeldata)

options(scipen = 999)
ggplot2::theme_set(ggplot2::theme_light())


".//Chapter03//wisc_bc_data.csv" %>% 
  read.csv(stringsAsFactors = FALSE) %>% 
  as.tibble %>% 
  mutate_if(is.ordered, factor, ordered = FALSE) -> wbcd

wbcd %>% # 양성과 음성 수 보기
  select(-id) %>% 
  glimpse() %>% 
  mutate(diagnosis = str_replace_all(diagnosis, "M", "Malignant")) %>% 
  mutate(diagnosis = str_replace_all(diagnosis, "B", "Benign")) %>% 
  count(diagnosis)

wbcd %>% # 양성과 음성 비율 보기
  select(-id) %>% 
  mutate(diagnosis = str_replace_all(diagnosis, "M", "Malignant")) %>% 
  mutate(diagnosis = str_replace_all(diagnosis, "B", "Benign")) %>% 
  count(diagnosis) %>% 
  mutate(prop=format(prop.table(n)*100, digits=3))

library(tidymodels) # 머신러닝용 타이디패키지
wbcd %>% # 훈련데이터, 테스트데이터 생성 (7:3 비율로 나누었다!)
  select(-id) %>% 
  mutate(diagnosis = str_replace_all(diagnosis, "M", "Malignant")) %>% 
  mutate(diagnosis = str_replace_all(diagnosis, "B", "Benign")) %>% 
  ungroup() %>% 
  rsample::initial_split(prop = 0.7, strata = diagnosis) -> wbcd_split

wbcd_split %>% # 데이터 어떻게 생겼나?
  training() %>% # 테스트 데이터 보려면 testing()
  glimpse() 

wbcd_split %>% training() -> train_df
wbcd_split %>% testing() -> test_df

# 이제부터 직관적인 분석이 시작됩니다.
df_rec <- recipe(diagnosis ~., data = train_df) %>% 
  step_downsample(diagnosis) %>% # 훈련데이터 정제작업
  step_center(-diagnosis) %>% # 숫자 데이터를 평균0으로 정규화
  step_scale(-diagnosis) %>% # 숫자 데이터를 표준편차 1로 정규화
  prep()

df_juiced <- juice(df_rec)

df_juiced %>% pivot_longer(-diagnosis) %>% 
  ggplot() +
  geom_histogram(aes(value, fill = diagnosis)) +
  facet_wrap(~name)

baked_test <- bake(df_rec, new_data = test_df)

install.packages("kknn")
#make a knn spec
knn_spec <- nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

#make a rf spec
rf_spec <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

#use the knn spec to fit the pre-processed training data
knn_fit <- knn_spec %>% 
  fit(diagnosis ~., data = df_juiced)

#use the rf spec to fit the pre-processed training data
rf_fit <- rf_spec %>% 
  fit(diagnosis ~., data = df_juiced)

rf_fit

knn_fit %>% 
  predict(baked_test) %>% 
  bind_cols(baked_test) %>% 
  metrics(truth = diagnosis, estimate = .pred_class)

knn_fit %>% 
  predict(df_juiced) %>% 
  bind_cols(df_juiced) %>% 
  metrics(truth = diagnosis, estimate = .pred_class)

rf_fit %>% 
  predict(df_juiced) %>% 
  bind_cols(df_juiced) %>% 
  metrics(truth = diagnosis, estimate = .pred_class)


results_train <- knn_fit %>% 
  predict(df_juiced) %>% 
  mutate(model = "knn", 
         truth = df_juiced$diagnosis) %>% 
  bind_rows(rf_fit %>% 
              predict(df_juiced) %>% 
              mutate(model = "rf",
                     truth = df_juiced$diagnosis)
            
  ) %>% 
  mutate(accuracy = if_else(.pred_class == truth, "yes", "no"))


results_test <- knn_fit %>% 
  predict(baked_test) %>% 
  mutate(model = "knn", 
         truth = baked_test$diagnosis) %>% 
  bind_rows(rf_fit %>% 
              predict(baked_test) %>% 
              mutate(model = "rf",
                     truth = baked_test$diagnosis)
            
  ) %>% 
  mutate(accuracy = if_else(.pred_class == truth, "yes", "no"))


results_train %>% 
  ggplot() +
  geom_bar(aes(accuracy, fill = accuracy)) +
  facet_wrap(~ model)

results_test %>% 
  ggplot() +
  geom_bar(aes(accuracy, fill = accuracy)) +
  facet_wrap(~ model)
