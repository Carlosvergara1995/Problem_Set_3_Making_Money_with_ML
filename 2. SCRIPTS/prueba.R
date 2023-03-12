## Se llaman los paquetes para su uso en el Script:
rm(list=ls())
install.packages("pacman")
install.packages("mice")
library(bnstruct)
library(mice)
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary,sf,leaflet,tmaptools,osmdata,nngeo,rgeos,rnaturalearth,class,modeest,mice,caret,parallel,doParallel,foreach)



#Se llaman las bases de datos: 
setwd('..')
setwd("3. STORE")

df_test <- import("test.csv")
df_train <- import("train.csv")
df <- import("df_con_variables.rds")

###Creamos cluster con base en la cercania geometrica
# Extraer las coordenadas de los puntos
coords <- st_coordinates(df$geometry)

# Aplicar k-means para crear 10 grupos
set.seed(123) # para reproducibilidad de resultados
kmeans_output <- kmeans(coords, centers = 15)

# Agregar la columna "group" al data frame "df" con la asignación de grupo para cada punto
df <- df %>% mutate(grupo = kmeans_output$cluster)
df$grupo <- factor(df$grupo, exclude=NULL)
df$year <- factor(df$year, exclude=NULL)

#IMPUTAMOS BANOS
# Contar las frecuencias de los valores únicos en la columna "BATHROOMS"
bathrooms_freq <- table(df$bathrooms)

# Seleccionar el valor más frecuente (la moda)
bathrooms_mode <- as.numeric(names(which.max(bathrooms_freq)))

# Reemplazar los valores faltantes con la moda
df$bathrooms[is.na(df$bathrooms)] <- bathrooms_mode



# Crear objeto mice
# Seleccionar variables y convertir a data frame
df_selected <- df[, c("bathrooms","bedrooms","property_type","Garaje","rooms","surface_total","surface_covered","grupo")]
df_selected$geometry <- NULL
df_selected <- as.data.frame(df_selected)



mice_object <- mice(df_selected, m = 5, method = "rf", maxit = 50, seed = 123)

# Imputar los valores faltantes
df_imputed <- complete(mice_object)
df$bedrooms<- df_imputed$bedrooms
df$rooms<- df_imputed$rooms
df$surface_total<- df_imputed$surface_total
df$surface_covered<- df_imputed$surface_covered


###########dividimos ##################


train <- df[complete.cases(df$price), ]
test  <- df[is.na(df$price), ]

test_final <- test[, !(names(df) %in% c("title", "description", "geometry","city","property_id","operation_typy"))]
train_data <- train[, !(names(df) %in% c("title", "description", "geometry","city","property_id","operation_typy"))]
train_data$geometry <- NULL
train_data <- train_data %>%
  select(-operation_type)

train_data$property_type <- factor(train_data$property_type, exclude=NULL)



################prueba

# Definimos los valores del grid de hiperparámetros
param_grid <- expand.grid(alpha = seq(0, 1, length = 10)
                          ,lambda = c(0, 0.001, 0.01, 0.1, 1, 10, 100,1000))

# Crea un modelo base con los hiperparámetros predeterminados
base_model <- train(price ~ ., data = train_data, method = "glmnet")

# Especifica el método de validación cruzada
ctrl <- trainControl(method = "cv", number = 5)



# Realiza la búsqueda en la grilla de hiperparámetros
model <- train(price ~ ., data = train_data, method = "glmnet",
               trControl = ctrl,
               tuneGrid = param_grid,
               preProcess = c("center", "scale"))
# Selecciona el mejor modelo
best_model <- model$bestTune
best_lambda <- best_model$lambda

predictions <- predict(model, newdata = test_final)


round(colMeans(is.na(df)) * 100, 2)

# Unir id con predicciones en un nuevo dataframe
results <- data.frame(property_id = df_test$property_id, price = predictions)
# Exportar a csv
write.csv(results, "predicciones.csv", row.names = FALSE)

best_lambda



##########SUPER LEARN
p_load("SuperLearner","randomForest")


ySL <- train_data$price
XSL <- train_data %>%
  select(-price)
XSL <- train_data %>%
  select(-month)

# Escalar los datos
preproc <- preProcess(XSL, method = c("center", "scale"))
XSL <- predict(preproc, XSL)


#Definimos modelos
# Customize the defaults for random forest.
custon_ranger = create.Learner("SL.ranger", params = list(num.trees = 1000))
custom_rf = create.Learner("SL.randomForest",
                           tune = list(mtry = round(c(1, sqrt(4), 3))))
custom_rf$names

sl.lib2 <- c("SL.randomForest", "SL.lm",custon_ranger$names,custom_rf$names)
sl.lib2




# Definir el control de validación cruzada
cv <- SuperLearner.CV.control(V = 5)

datos_prueba <- test_final %>%
  select(-geometry)

datos_prueba$geometry <- NULL


# Ajustar el modelo usando SuperLearner con validación cruzada
fitY <- SuperLearner(Y = ySL, X = data.frame(XSL), SL.library = sl.lib2, 
                     method = "method.NNLS", cvControl = cv)



fitY
yhat_Sup <- predict(fitY, newdata = data.frame(datos_prueba))


# Unir id con predicciones en un nuevo dataframe
results <- data.frame(property_id = df_test$property_id, price = yhat_Sup)
# Exportar a csv
write.csv(results, "predicciones2.csv", row.names = FALSE)

############TUNEANDO ARBOLES
datos_prueba <- test_final %>%
  select(-geometry)

datos_prueba$geometry <- NULL

X_train  <-XSL
y_train  <-ySL
X_test   <-datos_prueba
p_load(tidyverse,rpart,caret)
set.seed(1011)
  

rf_grid <- expand.grid(
  mtry = c(3, 4, 5), 
  nodesize = c(1, 3, 5)
)

rf_model <- train(
  y_train ~ ., 
  data = train_data, 
  method = "rf", 
  trControl = trainControl(method = "cv", number = 10), 
  tuneGrid = rf_grid,
  tuneLength = 9
)
# Realizar predicciones sobre los datos de prueba
predictions <- predict(rf_model, test_data)

