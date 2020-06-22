
load(file=url("https://github.com/martintinch0/CienciaDeDatosParaCuriosos/raw/master/data/independientes.RData"))
library(caret)
library(randomForest)
library(tidyverse)
library(furrr)
plan(multiprocess)
# Intentamos tener los mismos resultados
set.seed(10)
# Lista que guarda los resultados
resultados <- list()
# Elegimos los grupos
grupos <- createFolds(independientes %>% pull(REGISTRADO), k = 5, list = FALSE)
independientes <- independientes %>% mutate(grupos=grupos)
# Entrenamos cada uno de los valores para mtry
salida <- future_map_dbl(1:5, function(opciones){
  # Para cada uno de los grupos
  mean(future_map_dbl(1:5, function(k){
    # Generamos el grupo de entrenamiento
    dataTraining <-independientes %>% filter(grupos != k)  %>% select(-grupos)
    # Entrenamos el random forest
    rf <- randomForest(formula= REGISTRADO ~.,data=dataTraining,mtry=opciones, ntree=100)
    # Predecimos sobre testing
    dataTesting <- dataTraining <-independientes %>% filter(grupos == k)  %>% select(-grupos)
    pred <- predict(rf,newdata = dataTesting)
    tabla <- table(pred,dataTesting %>% pull(REGISTRADO))
    acc <- sum(diag(tabla))/sum(tabla)
  }))
},.progress=TRUE)

# Solo un arbol
salidaArbol <- future_map_dbl(1:5, function(opciones){
  mean(future_map_dbl(1:5, function(k){
    # Generamos el grupo de entrenamiento
    dataTraining <-independientes %>% filter(grupos != k)  %>% select(-grupos)
    # Entrenamos el random forest
    rf <- randomForest(formula= REGISTRADO ~.,data=dataTraining,mtry=opciones,ntree=1)
    # Predecimos sobre testing
    dataTesting <- dataTraining <-independientes %>% filter(grupos == k)  %>% select(-grupos)
    pred <- predict(rf,newdata = dataTesting)
    tabla <- table(pred,dataTesting %>% pull(REGISTRADO))
    acc <- sum(diag(tabla))/sum(tabla)
  }))
  
},.progress=TRUE)






