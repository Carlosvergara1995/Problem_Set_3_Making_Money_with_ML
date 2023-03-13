set.seed(123)
library(pacman)
p_load(gbm, tidyverse,rio,skimr,dplyr, caret, magrittr, glmnet,smotefamily,ROSE, rpart, rattle,MLmetrics,ranger, SuperLearner, randomForest)


#modelos regresión boosting
#divimos nuestras datos en train, validacion 
inTrain <- createDataPartition(
  y = train_s$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

valit <- train_s[-inTrain,]
train1 <- train_s[inTrain,]
test <- test_s %>% mutate(price = NULL)

modeloreg <- lm(price~., data=train1)


# Definición de pesos iniciales para las observaciones
pesos <- rep(1, nrow(train1))

# Entrenamiento del modelo de boosting

modelo_boosting <- gbm(price ~ ., 
                       data = train1, distribution = "gaussian", n.trees = 1000, 
                       interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.5, 
                       n.minobsinnode = 10, weights = pesos)

# Evaluación del modelo en el conjunto de prueba
predicciones_boosting_insample <- predict(modelo_boosting, newdata = train1, n.trees = 1000)
MAPE(y_pred = predicciones_boosting_insample, y_true = train1$price)


# Definición de nuevos pesos según la precisión de las predicciones
pesos_nuevos <- rep(1, nrow(train1))
pesos_nuevos[predicciones_boosting_insample > train1$price] <- 3
pesos_nuevos[predicciones_boosting_insample < train1$price] <- 0.33

# Ajuste de los pesos en el modelo de boosting
modelo_boosting2 <- gbm(price ~ ., 
                        data = train1, distribution = "gaussian", n.trees = 1000, 
                        interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.5, 
                        n.minobsinnode = 10, weights = pesos_nuevos)

# Evaluación del modelo ajustado en el conjunto de prueba
predicciones_pesadasin <- predict(modelo_boosting2, newdata = train1, n.trees = 1000)
MAPE(y_pred = predicciones_pesadasin, y_true = train1$price)

predicciones_pesadasout <- predict(modelo_boosting2, newdata = train1, n.trees = 1000)
MAPE(y_pred = predicciones_pesadasout, y_true = valit$price)