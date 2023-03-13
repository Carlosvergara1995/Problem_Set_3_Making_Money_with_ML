#Problmem_Set_3
# Modelos arboles.
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
#_______________________________________________________________

#######Preparación del espacio########

library(pacman)
p_load(tidyverse,rio,skimr,dplyr, caret, magrittr, glmnet,smotefamily,ROSE, rpart, rattle,MLmetrics,ranger, SuperLearner, randomForest)


#Modelo Arboles
set.seed(123)

#importamos las bases finales 
test_s <- readRDS("~/Desktop/git hut repositorios/Problem_Set_3_Making_Money_with_ML/3. STORE/test_s.rds")
train_s <- readRDS("~/Desktop/git hut repositorios/Problem_Set_3_Making_Money_with_ML/3. STORE/train_s.rds")

#divimos nuestras datos en train, validacion 
inTrain <- createDataPartition(
  y = train_s$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

valit <- train_s[-inTrain,]
train1 <- train_s[inTrain,]
test <- test_s %>% mutate(price = NULL)

# Creamos las particiones para hacer validación cruzada
cv10 <- trainControl(number = 10, method = "cv")
cv8 <- trainControl(number = 8, method = "cv")

#
modeloa <- train(price ~ .,
                 data = train1, 
                 method = "rpart", 
                 trControl = cv8)

#escogemos el mejor modelo

fancyRpartPlot(modeloa$finalModel)
#enontramos que la variable bathrooms predice bien los resultados

#analizamos las predicciones del modelo
y_hat_insample1 = predict(modeloa, newdata = train1)
y_hat_outsample1 = predict(modeloa, newdata = valit)

MAPE(y_pred = y_hat_insample1, y_true = train1$price)
MAE(y_pred = y_hat_insample1, y_true = train1$price)

MAPE(y_pred = y_hat_outsample1, y_true = valit$price)
MAE(y_pred = y_hat_outsample1, y_true = valit$price)

#Ramdom Forest####

#creamos una grilla

grilla <- expand.grid(mtry = c(4,7,12),
                      splitrule= "variance",
                      min.node.size =c(10,30,100))

modelob <- train(price~.,
                 data=train1,
                 trControl= cv8,
                 metric ="RMSE",
                 tuneGrid=grilla,
                 method = "ranger")


# graficamos
ggplot(modelob$results, aes(x = min.node.size, y = RMSE, 
                            color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  labs(title = "Resultados del grid search",
       x = "Mínima cantidad de observaciones por hoja",
       y = "RMSE (Cross-Validation)") +
  scale_color_discrete("Número de predictores seleccionados al azar") +
  theme_bw() +
  theme(legend.position = "bottom")


#encontramos que el modelo optimo tiene una cantidad optima de 4 
#variables con un aproximado de 10 nodos

#analizamos este modelo dentro y fuera de muestra
y_hat_insample2 = predict(modelob, newdata = train1)
y_hat_outsample2 = predict(modelob, newdata = valit)

MAPE(y_pred = y_hat_insample2, y_true = train1$price)
MAE(y_pred = y_hat_insample2, y_true = train1$price)

MAPE(y_pred = y_hat_outsample2, y_true = valit$price)
MAE(y_pred = y_hat_outsample2, y_true = valit$price)

#
MAPE(y_pred = y_hat_outsample2, y_true = test$price)
MAE(y_pred = y_hat_outsample2, y_true = test$price)


