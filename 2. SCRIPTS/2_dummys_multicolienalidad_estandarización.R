#Problmem_Set_3
# Variables Dymmies
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
#_______________________________________________________________

library(pacman)
p_load(tidyverse,rio,skimr,dplyr, caret, magrittr, glmnet,smotefamily,ROSE, rpart, rattle,MLmetrics,ranger, SuperLearner, randomForest)

#cargamos nuestra base 

df1<- df
#asignamos factor a las variables y dummyficamos ####
df <- df %>%
  mutate( property_type= factor(property_type),
         parking= factor(parking),
         Terraza=factor(Terraza),
         Garaje=factor(Garaje),
         )

dumificador <- dummyVars(formula = ~ . , data = df, fullRank = T)
db <- predict(dumificador, newdata = df)
db <- as.data.frame(db)
#analizamos multicolinealidad
reg <- lm( price ~., data = db)
summary(reg)


#separamos nuestras bases en train y test ####
# Subconjunto de datos train
train <- subset(db, !is.na(price))

# Subconjunto de datos test
test <- subset(db, is.na(price))

train_s<- train
test_s <- test
#estandarizamos nuestras variables
variables_numericas <- c("surface_total", "bedrooms", "bathrooms",
                         "dist_bus_station", "dist_bank", "dist_restaurant","dist_school",
                         "dist_park")
escalador <- preProcess(train[, variables_numericas],
                        method = c("center", "scale"))
train_s[, variables_numericas] <- predict(escalador, train[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, test[, variables_numericas])


#exportamos la base estandarizada y dummificada####
#saveRDS(train_s, file = "train_s.rds")
#saveRDS(test_s, file = "test_s.rds")
