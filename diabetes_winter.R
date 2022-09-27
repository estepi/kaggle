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
mice_complete <- mice(diabetes[,c(  "Glucose",
                                    "BMI", 
                                    "BloodPressure",
                                    "Pregnancies", 
                                    "SkinThickness", 
                                    "Insulin")],
  method="rf")

diabetes$Glucose<-  complete(mice_complete)$Glucose
diabetes$BMI <- complete(mice_complete)$BMI 
diabetes$BloodPressure<-  complete(mice_complete)$BloodPressure
diabetes$Pregnancies <- complete(mice_complete)$Pregnancies
diabetes$SkinThickness <- complete(mice_complete)$SkinThickness 
diabetes$Insulin <- complete(mice_complete)$Insulin
missmap(diabetes)

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
#otro plot
varImpPlot(model$results, 
           type = 2,
           main = "Variable Importance",col = 'black')
