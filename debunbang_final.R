# import
library(dplyr)
library(ggplot2)
library(splitTools)
library(pROC)

# read data
data <- read.csv('kindey stone urine analysis.csv')
data$target <- as.factor(data$target)

# train-val split
set.seed(12)
inds <- partition(data$target, p = c(train = 0.7, validation = 0.3))
str(inds)
train_data <- data[inds$train, ]
val_data <- data[inds$validation, ]


# LR model 1
model1 <- glm(target ~ gravity + ph + osmo + cond + urea + calc., data = train_data, family = binomial)

# backward selection 1
model1_step <- step(model1, direction = "backward")
summary(model1_step)

# ROC 1
roc1 <- roc(val_data$target, predict(model1_step, newdata = val_data, type="response"))
plot(roc1)
print(auc(roc1))

# LR model 2
model2 <- glm(target ~ gravity * ph * osmo * cond * urea * calc, data = train_data, family = binomial)

# backward selection 2
model2_step <- step(model2, direction = "backward")
summary(model2_step)

# ROC 2
# use validation set
roc2 <- roc(val_data$target, predict(model2_step, newdata = val_data, type="response"))
plot(roc2)
print(auc(roc2))

