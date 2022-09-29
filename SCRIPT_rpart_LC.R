#En este SCRIPT voy a intentar correr el árbol de decisión para el dataset de diabetes


# Paquetes instalados -----------------------------------------------------
install.packages("randomForest")
install.packages("tree")


# Bibliotecas cargadas ----------------------------------------------------
library(corrplot) #correlación
library(caret) #ML
library("e1071")
library(tree) # random forest
library(randomForest) #random forest


# Cargando la data --------------------------------------------------------
diab <- read.csv("diabetes.csv")
colnames(diab)
head(diab)
summary(diab)


corrplot(cor(diab[, -9]), type = "lower", method = "number") #se excluye outcome
## Pregnancies y age son las más correlacionadas
## Le sigue skin Thickness y BMI

prop.table(table(diab_testing$Outcome))


# Preparación de los set de Entretaniento y Test --------------------------------------
set.seed(123) #para que los resultados sean reproducibles, sino la aleatoridad no me va a permitir reproducirlos
n <- nrow(diab) #el n es el número de muestras
train <- sample(n, trunc(0.70*n)) #seleccionamos el 70% de las n para entrenar
diab_training <- diab[train, ]
prop.table(table(diab_training$Outcome))
diab_testing <-diab[-train, ] #el training se hace con el 30% restante
prop.table(table(diab_testing$Outcome))

# Entrenamiento -----------------------------------------------------------
glm_fm1 <- glm(Outcome ~., data = diab_training, family = binomial) #ajustando generalized linear models
summary(glm_fm1)
## family es la distribucion de error
#############################
glm_fm2 <- update(glm_fm1, ~. - Triceps_Skin - Serum_Insulin - Age ) #tricep, y serum son variables que no existen??
summary(glm_fm2)

par(mfrow = c(2,2)) #para plotear dos graficos
plot(glm_fm2)

# Testeando el modelo
glm_probs <- predict(glm_fm2, newdata = diab_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)

#print("Confusion Matrix for logistic regression");
table(Predicted = glm_pred, Actual = diab_testing$Outcome)
class(glm_pred)
class(diab_testing$Outcome)
table(Predicted = glm_pred, Actual = diab_testing$Diabetes)
table(Predicted = glm_pred, Actual = diab_testing$Outcome)

set.seed(1000)
#try another model
class(diab_testing$Outcome)
diab$Outcome <- as.factor(diab$Outcome)
intrain <- createDataPartition(y = diab$Outcome, p = 0.7, list = FALSE)
train <- diab[intrain, ]
test <- diab[-intrain, ]

rf_diab <- randomForest(Outcome ~., data = diab_training, mtry = 8, ntree=50, importance = TRUE)

# Testing the Model
rf_probs <- predict(rf_diab, newdata = diab_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)


importance(rf_diab)
par(mfrow = c(1, 2))
varImpPlot(rf_diab, type = 2, main = "Variable Importance", col = 'black')
plot(rf_diab, main = "Error vs no. of trees grown")



# USANDO OTRO PAQUETE -----------------------------------------------------

##Probando con r.part.plot
library(rpart)
library(rpart.plot)

# Viendo los datos
head(diab)
tail(diab) 
## Es importante ver que los datos no estén ordenados de cierta manera, sino introducimos sesgos. Debemos desordenarlos 
## para analizarlos

table(is.na(diab)) #corroborar que no haya NAs

set.seed(123) #para que los resultados sean reproducibles, sino la aleatoridad no me va a permitir reproducirlos
n <- nrow(diab) #el n es el número de muestras
train <- sample(n, trunc(0.80*n)) #seleccionamos el 70% de las n para entrenar

diab_training <- diab[train, ]
diab_testing <-diab[-train, ] #el training se hace con el 30% restante

prop.table(table(diab_training$Outcome))
prop.table(table(diab_testing$Outcome)) #las proporciones no son tan similares


# Construyendo el modelo --------------------------------------------------

fit <- rpart(Outcome~., data = diab_training, method = 'class')
rpart.plot(fit, extra = 106, cex =0.60)

predict <-predict(fit, diab_testing, type = 'class')

##comparar la clasificación hecha con la real
table_mat <- table(diab_testing$Outcome, predict)
table_mat # predijo 129 sanos correctamente, pero 21 enfermos son en realidad sanos

#Accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test #0.73
