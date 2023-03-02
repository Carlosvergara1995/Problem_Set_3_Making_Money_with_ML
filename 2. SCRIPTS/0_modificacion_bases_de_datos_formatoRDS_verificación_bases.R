#Problmem_Set_3
# Transformación de bases de datos a formato Rds.
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
#_______________________________________________________________

#######Preparación del espacio########

## Se llaman los paquetes para su uso en el Script:
rm(list=ls())
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary)
#Se llaman las bases de datos: 
setwd("~/OneDrive - Universidad de los Andes/2023/2023-1/BIG DATA/TALLERES GRUPALES/TALLER No. 3/Problem_Set_3_Making_Money_with_ML/3. STORE")

df_test <- import("test.csv")
df_train <- import("train.csv")

## Se salvan las bases de datos como rds.

saveRDS(df_test,"df_test.rds")
saveRDS(df_train,"df_train.rds")

#Con las bases cargadas, se verifican para evidenciar variables distintas:

colnames(df_train)
colnames(df_test)
summary(df_test)
summary(df_train)

diff_variables <- setdiff(names(df_train), names(df_test))

#Se evidencia que las bases de datos no cuentan con variables distintas. 
