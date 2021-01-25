library(kernlab)

data <- read.csv('bmi.csv')
head(data)
str(data)
summary(data)
dim(data)
colnames(data)

idx <- sample(1:nrow(data), 0.9 * nrow(data))
training <- data[idx, ]
testing <- data[-idx, ]

# kernel 커널 함수를 명시한다. 
model <- ksvm(label ~ ., data = training, kernel = 'vanilladot')
pred <- predict(model, testing)

table(pred, testing$label)

aggrement <- pred == testing$label

prop.table(table(aggrement))

model_rbf <- ksvm(label ~ ., data = training, kernel = 'rbfdot')
pred_rbf <- predict(model_rbf, testing)

table(pred_rbf, testing$label)

aggrement_rbf <- pred_rbf == testing$label

prop.table(table(aggrement_rbf))

########################################################

library(kernlab)

str(iris)
unique(iris$Species)

idx <- sample(1:nrow(iris), 0.6 * nrow(iris))
training <- iris[idx, ]
testing <- iris[-idx, ]

model <- ksvm(Species ~ ., data = training, kernel = 'vanilladot')
pred <- predict(model, testing)
prop.table(table(pred == testing$Species))

#######################################################

library(kernlab)

data <- read.csv('zoo_data.csv')
testing <- read.csv('zoo_testing.csv')
head(data)
str(data)
summary(data)
dim(data)
colnames(data)

model <- ksvm(type ~ ., data = data, kernel = 'rbfdot')
pred <- round(predict(model, testing), 0)

table(pred, testing$type)

aggrement <- pred == testing$type

prop.table(table(aggrement))

#########################################################

library(kernlab)

data <- read.csv('letterdata.csv')
head(data)
str(data)
summary(data)
dim(data)
colnames(data)

idx <- sample(1:nrow(data), 0.7 * nrow(data))
training <- data[idx, ]
testing <- data[-idx, ]

model <- ksvm(letter ~ ., data = training, kernel = 'rbfdot')
pred <- predict(model, testing)

table(pred, testing$letter)
prop.table(table(pred == testing$letter))

##############################################################

library(kernlab)

data <- read.csv('mushrooms.csv')
head(data)
str(data)
summary(data)
dim(data)
colnames(data)

data <- data[, -17]

idx <- sample(1:nrow(data), 0.7 * nrow(data))
training <- data[idx, ]
testing <- data[-idx, ]

model <- ksvm(type ~ ., data = training, kernel = 'rbfdot')
pred <- predict(model, testing)

table(pred, testing$type)
prop.table(table(pred == testing$type))

##################################################################

library(kernlab)

data <- read.csv('../../R Basic Source/31.KNN/likelyhood.csv')
head(data)
str(data)
summary(data)
dim(data)
colnames(data)

idx <- sample(1:nrow(data), 0.1 * nrow(data))
training <- data[idx, ]
testing <- data[-idx, ]

model <- ksvm(likelyhood ~ ., data = training, kernel = 'rbfdot')
pred <- predict(model, testing)

table(pred, testing$likelyhood)
prop.table(table(pred == testing$likelyhood))