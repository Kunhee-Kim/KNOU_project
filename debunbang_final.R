# import
library(dplyr)
library(ggplot2)
library(splitTools)
library(pROC)
library(caret)
library(car)

# read data
data <- read.csv('kindey stone urine analysis.csv')
data$target <- as.factor(data$target)

# check extreme outliers
summary(data)

numeric_cols <- sapply(data, is.numeric)
par(mfrow=c(2,3))
for(i in names(data[, numeric_cols])) {
  hist(data[[i]], main=i, xlab=i, col='blue')
}


# train-val split
set.seed(12)
inds <- partition(data$target, p = c(train = 0.7, validation = 0.3))
str(inds)
train_data <- data[inds$train, ]
val_data <- data[inds$validation, ]


# LR model 1
model1 <- glm(target ~ gravity + ph + osmo + cond + urea + calc,
              data = train_data, family = binomial)

# backward selection 1
model1_step <- step(model1, direction = "backward")
summary(model1_step)
vif(model1_step)

# AUC
par(mfrow = c(1,1))
roc1_train <- roc(train_data$target, predict(model1_step, newdata = train_data, type="response"))
plot(roc1_train)
print(auc(roc1_train))
roc1_val <- roc(val_data$target, predict(model1_step, newdata = val_data, type="response"))
plot(roc1_val)
print(auc(roc1_val))

# LR model 2
model2 <- glm(target ~ gravity * ph * osmo * cond * urea * calc,
              data = train_data, family = binomial)

# backward selection 2
model2_step <- step(model2, direction = "backward")
summary(model2_step)
vif(model2_step)

# AUC
roc2_train <- roc(train_data$target, predict(model2_step, newdata = train_data, type="response"))
plot(roc2_train)
print(auc(roc2_train))
roc2_val <- roc(val_data$target, predict(model2_step, newdata = val_data, type="response"))
plot(roc2_val)
print(auc(roc2_val))


# LR model 1_1
model1_1 <- glm(target ~ gravity + ph + cond + urea + calc,
              data = train_data, family = binomial)

# backward selection 1_1
model1_1_step <- step(model1_1, direction = "backward")
summary(model1_1_step)
vif(model1_1_step)

# sensitivity, specificity
val_predictions1_1 <- predict(model1_1_step, newdata = val_data, type="response")
val_predictions1_1 <- factor(ifelse(val_predictions1_1 >= 0.5, 1, 0))
conf_matrix1_1 <- confusionMatrix(val_predictions1_1, val_data$target)
print(conf_matrix1_1)

# ROC 1_1
roc1_1_train <- roc(train_data$target, predict(model1_1_step, newdata = train_data, type="response"))
plot(roc1_1_train)
print(auc(roc1_1_train))
roc1_1_val <- roc(val_data$target, predict(model1_1_step, newdata = val_data, type="response"))
plot(roc1_1_val)
print(auc(roc1_1_val))

# LR model 2_2
model2_2 <- glm(target ~ gravity + calc + ph * cond * urea,
                data = train_data, family = binomial)

# backward selection 2_2
model2_2_step <- step(model2_2, direction = "backward")
summary(model2_2_step)
vif(model2_2_step)

# sensitivity, specificity
val_predictions2_2 <- predict(model2_2_step, newdata = val_data, type="response")
val_predictions2_2 <- factor(ifelse(val_predictions2_2 >= 0.5, 1, 0))
conf_matrix2_2 <- confusionMatrix(val_predictions2_2, val_data$target)
print(conf_matrix2_2)

# ROC 2_2
roc2_2_train <- roc(train_data$target, predict(model2_2_step, newdata = train_data, type="response"))
plot(roc2_2_train)
print(auc(roc2_2_train))
roc2_2 <- roc(val_data$target, predict(model2_2_step, newdata = val_data, type="response"))
plot(roc2_2)
print(auc(roc2_2))

