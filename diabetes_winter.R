#install.packages("Amelia")
#install.packages("corrplot")
#install.packages("caret")
#install.packages("e1071")
#install.packages("tree")
#install.packages("randomForest")
#install.packages("mice")
#install.packages("GGally")
##############################################
#----Load libraries-----
library(corrplot)
library(caret)
library("e1071")
library(tree)
library(Amelia)
library(Amelia)
library(randomForest)
library(mice)
library(GGally)
#----1. Load Data ----
diabetes<-read.csv("diabetes.csv")
colnames(diabetes)
head(diabetes)
# View(diabetes)

#----Prepare the DataSet----
diabetes$Outcome<-as.factor(diabetes$Outcome)

#----Cleaning ----
diabetes[,1:8][diabetes[,1:8]==0]<- NA
# descripcion de NAs en el dataset
missmap(diabetes)
#Imputacion de datos
colnames(diabetes)
#Transferir datos perdidos predichos
#############
#mice_complete <- mice(diabetes[,c(  "BloodPressure",
#                              "Pregnancies", 
#                                  "BMI", "SkinThickness")],
#  method="rf")

#diabetes$SkinThickness <- mice_complete$SkinThickness 
#diabetes$Insulin <-    mice_complete$Insulin 
#diabetes$BMI <- mice_complete$BMI 
#diabetes$Glucose <- mice_complete$Glucose 

#Visualizando la variable de numero de embarazos 

ggplot(diabetes, aes(x=Pregnancies, 
                     fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + 
  labs(title="Distribución de embarazos") + 
  theme_bw() 

#########################
#Visualizando la variable de Glucosa 
ggplot(diabetes, aes(Glucose, colour = Outcome)) + 
  geom_freqpoly(binwidth = 1) + 
  labs(title="Distribución Glucosa") + 
  theme_bw() 


#Visualizando la variable de Insulina 
ggplot(diabetes, aes(x=Insulin, fill=Outcome,
                     color=Outcome)) +
  geom_histogram(binwidth = 1) + 
  labs(title="Distribución de la insulina")+ theme_bw() 


#IVisualizando la variable de presion sanguinea ggplot(diabetes, aes(x=BloodPressure , col=Outcome))+ geom_freqpoly(binwidth = 1) + labs(title="Distribución de presión sanguínea") + theme_bw() #Visualizando la variable de grosor de piel 
ggplot(diabetes, aes(x=SkinThickness, col=Outcome))+ 
  geom_freqpoly(binwidth = 1) + 
  labs(title="Distribución de grosor de piel") + theme_bw() 

#analyze codependency
ggpairs(diabetes)
##############################3
#-----Construyendo el modelo----
set.seed(123)
#recomendacion del 0.7 al 0.85
particion_diab<-createDataPartition(y=diabetes$Outcome,
                                    p=0.75, list = FALSE)

set_entrenamiento<-diabetes[particion_diab,]; dim(set_entrenamiento)
set_validacion<-diabetes[-particion_diab,]; dim(set_validacion)
  
# Checar dimensiones de los datos partidos
prop.table(table(diabetes$Outcome)) * 100 
prop.table(table(set_entrenamiento$Outcome)) * 100 
prop.table(table(set_validacion$Outcome)) * 100 
#Seleccionando la variable de clasificacion 

#---- PASO 7 mATRIZ DE CONFUSION ----
#nb naive bayes
head(set_entrenamiento)
dim(set_entrenamiento)
colnames(set_entrenamiento)
x = set_entrenamiento[,-9] #Seleccionando la variable de clasificacion y = set_entrenamiento$Outcome #Construyendo el modelo library(e1071) 
y = set_entrenamiento$Outcome 

model <- train(x,y,'nb',
               trControl=trainControl(method='cv',number=10)) 
# No anda, muchas NAs¿


confusionMatrix(model)

varImpPlot(model, 
           type = 2,
           main = "Variable Importance",col = 'black')
#################################
#----variables-----#


#calculo precision, sensibilidad
#################################
pima<-diabetes
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
pima$Outcome <- as.factor(pima$Outcome)

library(caret)
set.seed(1000)
intrain <- createDataPartition(y = pima$Outcome, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

set.seed(123)
rf_pima <- randomForest(Outcome ~., data = pima_training, mtry = 8, ntree=50, importance = TRUE)

# Testing the Model
rf_probs <- predict(rf_pima, newdata = pima_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)
importance(rf_pima)
par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'black')
plot(rf_pima, main = "Error vs no. of trees grown")