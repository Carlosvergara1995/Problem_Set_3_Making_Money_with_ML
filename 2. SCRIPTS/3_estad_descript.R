#Problmem_Set_3
# Estadísticas descriptivas.
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
#_______________________________________________________________

library(pacman)
p_load(tidyverse,
       rvest,
       writexl,
       rio,
       skimr,
       sf,
       pastecs,
       stargazer,
       PerformanceAnalytics,
       naniar,
       gtsummary,
       stringr,
       rgeos, 
       plotly, 
       leaflet, 
       tmaptools,
       osmdata,
       flextable
)

install.packages("plotly")

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
write_xlsx(descriptivas_train, "descrip_train_s.xlsx")

##Gráficos: 

#Precio de venta: 

p <- ggplot(train_s, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta", y = "Cantidad de unmuebles") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

#Distancias con parques:

p2 <- ggplot(train_s, aes(x = dist_park, y = price)) +
  geom_point(col = "red", alpha = 0.4) +
  labs(x = "Distancia", 
       y = "Valor venta inmueble",
       title = "Parques y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p2) 

#Distancias con colegios:

p3 <- ggplot(train_s, aes(x = dist_school, y = price)) +
  geom_point(col = "green", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Colegios y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p3) 

#Distancias con restaurantes:

p4 <- ggplot(train_s, aes(x = dist_restaurant, y = price)) +
  geom_point(col = "orange", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Restaurantes y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p4) 

#Distancias con bancos:

p5 <- ggplot(train_s, aes(x = dist_bank, y = price)) +
  geom_point(col = "purple", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Bancos y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p5) 

#Distancias con estaciones de bus:

p6 <- ggplot(train_s, aes(x = dist_bus_station, y = price)) +
  geom_point(col = "pink", alpha = 0.4) +
  labs(x = "Distancia ", 
       y = "Valor venta inmueble",
       title = "Estaciones de bus y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p6) 

#### Estadisticas descriptivas para entrenamiento (test) ####

#Se crean las estadisticas descriptivas:

dim(test_s)
colnames(test_s)
summary(test_s)

tbl_summary(test_s, statistic = list (all_continuous()~"{mean} ({sd})")) # generales

#Se exportan las tablas:

stat.desc(test_s)
descriptivas_test <- stat.desc(test_s)
descriptivas_test$Estadisticas <- row.names(descriptivas_test) 
descriptivas_test <- descriptivas_test %>% select(Estadisticas, everything())
write_xlsx(descriptivas_test, "descrip_test_s.xlsx")

##
