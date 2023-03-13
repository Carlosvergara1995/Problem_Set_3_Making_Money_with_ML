install.packages("pacman")
install.packages("mice")
require(pacman)
require(mice)
library(caret)
p_load(fancyRpartPlot,tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary,sf,leaflet,tmaptools,osmdata,nngeo,rgeos,rnaturalearth,dplyr)
#cargamos nuestra data
setwd('..')
setwd('3. STORE')

df_con_variables <- readRDS("df_con_variables.rds")

#analizamos las variables que contienen na  ####
names(df_con_variables)[sapply(df_con_variables, function(x) any(is.na(x)))]

#analisis de porcentaje de na
na_pct <- sapply(df_con_variables, function(x) mean(is.na(x)))*100
result <- data.frame(variable = names(df_con_variables), na_pct = na_pct)
result[result$na_pct > 0,]

#analisis de cantidad de na 
na_count <- sapply(df_con_variables, function(x) sum(is.na(x)))
result <- data.frame(variable = names(df_con_variables), na_count = na_count)
result[result$na_count > 0,]

#BAÃ‘OS ####
# Extract bathroom information as character string
bathroom_str <- str_extract(df_con_variables$description, "(?i)(\\d+)\\s*(bano|banos|banios|banio|baio|baios)\\b|(?i)\\b(bano|banos|banios|banio|baio|baios)\\b\\s*(\\d+)")

# Clean and convert string to numeric
bathroomsc <- as.numeric(gsub("(?i)\\b(bano|banos|banios|banio|baio|baios)\\b|\\s", "", bathroom_str, ignore.case = TRUE))

table(bathroomsc)

# reemplazamos los na de la variable bathroom con los datos extraÃ­dos
data <- data_frame(df_con_variables) #analizar la conversiÃ³nde data frame
data$bathrooms <- ifelse(is.na(data$bathrooms), bathroomsc, data$bathrooms)
sum(is.na(data$bathrooms))

# se reemplazaron 4214 na con los datos de descripcciÃ³n

# Convertir valores atÃ­picos en NAs
data$bathrooms<- ifelse(data$bathrooms > 15, NA, data$bathrooms)

# Verificar que los valores atÃ­picos hayan sido convertidos a NAs
sum(is.na(data$bathrooms))


# Calcular el promedio de baÃ±os para apartamentos y casas
mean_bath_apto <- round(mean(data$bathrooms[data$property_type == "Apartamento"], na.rm = TRUE))
mean_bath_casa <- round(mean(data$bathrooms[data$property_type == "Casa"], na.rm = TRUE))

# Reemplazar los NA en funciÃ³n de property_type
data$bathrooms <- ifelse(
  is.na(data$bathrooms) & data$property_type == "Apartamento",
  mean_bath_apto,
  ifelse(
    is.na(data$bathrooms) & data$property_type == "Casa",
    mean_bath_casa,
    data$bathrooms
  )
)

# Redondear la variable bathrooms a nÃºmeros enteros
data$bathrooms <- round(data$bathrooms)

# Verificar que se hayan reemplazado los NA correctamente
sum(is.na(data$bathrooms))

#surface_total ####

# Extract total surface information as character strings
surface_str <- str_extract(df_con_variables$description, "(?i)(\\d+)\\s*(mts2|mts|metros|metroscuadrados|m2|metros2|mcuadrados|mecuadrado|metro)\\b|(?i)\\b(mts2|mts|metros|metroscuadrados|m2|metros2|mcuadrados|mecuadrado|metro)\\b\\s*(\\d+)")

# Clean and convert strings to numeric
total_surface <- as.numeric(gsub("(?i)\\b(mts2|mts|metros|metroscuadrados|m2|metros2|mcuadrados|mecuadrado|metro)\\b|\\s", "", surface_str, ignore.case = TRUE))

# Replace NAs in 'total_surface' variable with the extracted data
data$surface_total<- ifelse(is.na(data$surface_total), total_surface, data$surface_total)

sum(is.na(data$surface_total))



# se reemplazaron 12047 na

#suponemos que algunos na son debido a que el surface covered es igual a surface total
data$surface_total <- ifelse(is.na(data$surface_total), data$surface_covered, data$surface_total)


# quitamos atipicos en area
data$surface_total[data$property_type == "Apartamento" & data$surface_total > 30000] <- NA
data$surface_total[data$property_type == "Apartamento" & data$surface_total < 36] <- NA
data$surface_total[data$property_type == "Apartamento" & data$surface_total < 45 & data$bedrooms > 1] <- NA
data$surface_total[data$property_type == "Casa" & data$surface_total < 50] <- NA

##Usamos vecinos cercanos para los datos faltantes, pero primero creamos grupos con knn
# Extraer las coordenadas de los puntos
coords <- st_coordinates(data$geometry)
# Aplicar k-means para crear 10 grupos
set.seed(123) # para reproducibilidad de resultados
kmeans_output <- kmeans(coords, centers = 200)

# Agregar la columna "group" al data frame "df" con la asignación de grupo para cada punto
data <- data %>% mutate(grupo = kmeans_output$cluster)
data$grupo <- as.factor(data$grupo)
data_imputacion<-data %>%select("grupo", "bathrooms", "bedrooms", "rooms", "surface_total", "year")

###### Crear un objeto mice para realizar la imputación #####

# Definir el método de imputación para cada columna
# Crear un objeto mice para realizar la imputación
imp <- mice(data_imputacion,m=5,maxit=50,meth='pmm',seed=500)
completedData <- complete(imp,1)
# Reemplazar los valores de data con los valores de completedData
data$grupo <- completedData$grupo

data$bedrooms <- completedData$bedrooms
data$rooms <- completedData$rooms
data$surface_total <- completedData$surface_total

data$year <- as.factor(data$year)
data$property_type <- as.factor(data$property_type)
data$parking <- as.factor(data$parking)
data$Garaje <- as.factor(data$Garaje)
data$Terraza <- as.factor(data$Terraza)
data$grupo <- as.factor(data$grupo)

##Quitamos las variables que no necesitamos

data_final <- data %>%select(-month,-city,-surface_covered,-description,-operation_type,-title,-geometry)
train <- data_final[complete.cases(data_final$price), ]
test  <- data_final[is.na(data_final$price), ]
train <- train %>% select(-property_id)



################prueba

# Definimos los valores del grid de hiperparámetros
param_grid <- expand.grid(alpha = seq(0, 1, length = 20)
                          ,lambda = c(0, 0.001, 0.01, 0.1, 1, 10))

# Crea un modelo base con los hiperparámetros predeterminados
base_model <- train(price ~ ., data = train, method = "glmnet")

# Especifica el método de validación cruzada
ctrl <- trainControl(method = "cv", number = 10)

# Realiza la búsqueda en la grilla de hiperparámetros
model <- train(price ~ ., 
               data = train, method = "glmnet",
               trControl = ctrl,
               tuneGrid = param_grid,
               preProcess = c("center", "scale"),
               metric = "RMSE")
model
predictions <- predict(model, newdata = test)

# Unir id con predicciones en un nuevo dataframe
df_test <- import("test.csv")
results <- data.frame(property_id = df_test$property_id, price = predictions)
# Exportar a csv
write.csv(results, "predicciones_regresion_lineal.csv", row.names = FALSE)



#Ramdom Forest####

#creamos una grilla

grilla <- expand.grid(mtry = c(4,7,12),
                      splitrule= "variance",
                      min.node.size =c(10,30,100))
cv8 <- trainControl(number = 8, method = "cv")
modelob <- train(price~.,
                 data=train,
                 trControl= cv8,
                 metric ="RMSE",
                 tuneGrid=grilla,
                 method = "ranger",
                 preProcess = c("center", "scale"),)


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


y_hat_insample2 = predict(modelob, newdata = test)
df_test <- import("test.csv")
results <- data.frame(property_id = df_test$property_id, price = y_hat_insample2)
# Exportar a csv
write.csv(results, "predicciones_con_data2.csv", row.names = FALSE)



