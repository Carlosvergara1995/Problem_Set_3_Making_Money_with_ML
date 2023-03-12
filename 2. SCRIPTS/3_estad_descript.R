#Problmem_Set_3
# Estadísticas descriptivas.
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
#_______________________________________________________________

library(pacman)
p_load(tidyverse,rio,skimr,dplyr, caret, magrittr, glmnet,smotefamily,ROSE, rpart, rattle,MLmetrics)

#cargamos nuestras bases:

test_s <- import("test_s.rds")
train_s <- import("train_s.rds")

#### Estadisticas descriptivas para entrenamiento (train) ####

#Se crean las estadisticas descriptivas:

dim(train_s)
colnames(train_s)
summary(train_s)

tbl_summary(train_s, statistic = list (all_continuous()~"{mean} ({sd})")) # generales
tbl_summary(train_s, by= price, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

#Se exportan las tablas:

stat.desc(train_s)
descriptivas_train <- stat.desc(train_s)
descriptivas_train$Estadisticas <- row.names(descriptivas_train) 
descriptivas_train <- descriptivas_train %>% select(Estadisticas, everything())
write_xlsx(descriptivas_train, "d_train_p.xlsx")


