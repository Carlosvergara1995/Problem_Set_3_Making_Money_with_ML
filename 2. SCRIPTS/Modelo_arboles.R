#Modelo Arboles
set.seed(123)

#divimos nuestras datos en train, validacion 
inTrain <- createDataPartition(
  y = train_s$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)


train1 <- train_s[inTrain,]
valit <- train_s[-inTrain,]


# Creamos las particiones para hacer validación cruzada
cv8 <- trainControl(number = 8, method = "cv")
cv5 <- trainControl(number = 5, method = "cv")

#
modeloa <- train(price ~ .,
                 data = train1, 
                 method = "rpart", 
                 trControl = cv8)

#escogemos el mejor modelo

fancyRpartPlot(modeloa$finalModel)


#analizamos las predicciones del modelo





