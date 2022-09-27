install.packages("corrplot")
library(corrplot)
install.packages("caret")
library(caret)
install.packages("e1071")
library("e1071")
install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)
library(caret)

pima<-read.csv("diabetes.csv")
colnames(pima)
head(pima)
corrplot(cor(pima[, -9]), type = "lower", method = "number")

dev.off()
# Preparing the DataSet
set.seed(123)
n <- nrow(pima)
train <- sample(n, trunc(0.70*n))
pima_training <- pima[train, ]
pima_testing <- pima[-train, ]


# Training The Model
glm_fm1 <- glm(Outcome ~., data = pima_training, family = binomial)
#############################
glm_fm2 <- update(glm_fm1, ~. - Triceps_Skin - Serum_Insulin - Age )
summary(glm_fm2)

par(mfrow = c(2,2))
plot(glm_fm2)

# Testing the Model
glm_probs <- predict(glm_fm2, newdata = pima_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
#print("Confusion Matrix for logistic regression");
table(Predicted = glm_pred, Actual = pima_testing$Outcome)
class(glm_pred)
class(pima_testing$Outcome)
table(Predicted = glm_pred, Actual = pima_testing$Diabetes)
table(Predicted = glm_pred, Actual = pima_testing$Outcome)

set.seed(1000)
#try anotehr model
class(pima_testing$Outcome)
pima$Outcome <- as.factor(pima$Outcome)
intrain <- createDataPartition(y = pima$Outcome, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

rf_pima <- randomForest(Outcome ~., data = pima_training, mtry = 8, ntree=50, importance = TRUE)

# Testing the Model
rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)


importance(rf_pima)
par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'black')
plot(rf_pima, main = "Error vs no. of trees grown")