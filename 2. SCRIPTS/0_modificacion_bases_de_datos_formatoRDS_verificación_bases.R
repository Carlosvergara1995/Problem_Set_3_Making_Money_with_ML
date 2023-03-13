#Problmem_Set_3
# Transformación de bases de datos a formato Rds.
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
#_______________________________________________________________

#######Preparación del espacio########

## Se llaman los paquetes para su uso en el Script:
rm(list=ls())
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary,sf,leaflet,tmaptools,osmdata,nngeo,rgeos,rnaturalearth)
#Se llaman las bases de datos: 
setwd('..')
setwd("3. STORE")

df_test <- import("test.csv")
df_train <- import("train.csv")

## Se salvan las bases de datos como rds.

#saveRDS(df_test,"df_test.rds")
#saveRDS(df_train,"df_train.rds")

#Con las bases cargadas, se verifican para evidenciar variables distintas:

colnames(df_train)
colnames(df_test)
summary(df_test)
summary(df_train)

diff_variables <- setdiff(names(df_train), names(df_test))

#Se evidencia que las bases de datos no cuentan con variables distintas. 

df = bind_rows(df_train,df_test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
head(df)

# creamos variables nuevas a partir de la descripcion y el title

df$parking <- grepl("parqueadero(s)?", df$title, ignore.case = TRUE) | grepl("parqueadero(s)?", df$description, ignore.case = TRUE)

df$Terraza<-grepl("terraza(s)?", df$title, ignore.case = TRUE) | grepl("terraza(s)?", df$description, ignore.case = TRUE)

df$Garaje<-grepl("garaje(s)?", df$title, ignore.case = TRUE) | grepl("garaje(s)?", df$description, ignore.case = TRUE)



#Se verifica la nueva base de datos: 

summary(df)

sum(is.na(df$bedrooms))
sum(is.na(df$surface_total))
sum(is.na(df$surface_covered))
sum(is.na(df$bathrooms))
sum(is.na(df$rooms))

# se defínen las zonas a analizar 

chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

bogota <- getbb(place_name = "Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)

chapinero <- st_transform(chapinero,st_crs(df))
bogota <- st_transform(bogota,st_crs(df))

df_chapinero <- df[chapinero,]

#head(df_chapinero)

available_features()



available_tags("amenity")
bar_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_chapinero , col="red")

###Para bogota
bar_bogota <- opq(bbox = st_bbox(df)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)


###estaciones de bus Chapinero
osm_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_chapinero <- osm_chapinero %>% osmdata_sf()

bus_station_chapinero <- osm_sf_chapinero$osm_points %>% select(osm_id,amenity)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_chapinero , col="red")

###Estaciones de bus Para Bogota

osm_bogota <- opq(bbox = st_bbox(df)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_bogota <- osm_bogota %>% osmdata_sf()

bus_station_bogota <- osm_sf_bogota$osm_points %>% select(osm_id,amenity)


###Bancos chapinero

bank_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bank_chapinero , col="red")

### Bancos bogota
bank_bogota <- opq(bbox = st_bbox(df)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

### Restaurantes Chapinero
restaurant_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant_chapinero , col="red")

### Restaurantes bogota
restaurant_bogota <- opq(bbox = st_bbox(df)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

### Escuelas chapinero
school_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=school_chapinero , col="red")

### Escuelas bogota
school_bogota <- opq(bbox = st_bbox(df)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

##PARQUES CHAPINERO
parques_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

##PARQUES bogota
parques_bogota <- opq(bbox = st_bbox(df)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

centroides_chapinero <- gCentroid(as(parques_chapinero$geometry, "Spatial"), byid = T)
centroides_bogota <- gCentroid(as(parques_bogota$geometry, "Spatial"), byid = T)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = parques_chapinero, col = "green",
              opacity = 0.8, popup = parques_chapinero$name) %>%
  addCircles(lng = centroides_chapinero$x, 
             lat = centroides_chapinero$y, 
             col = "red", opacity = 1, radius = 1)

centroides_chapinero_sf <- st_as_sf(centroides_chapinero, coords = c("x", "y"))
dist_matrix <- st_distance(x = df_chapinero, y = centroides_chapinero_sf)

##Bogota
centroides_bogota_sf <- st_as_sf(centroides_bogota, coords = c("x", "y"))
dist_matrix_b <- st_distance(x = df, y = centroides_bogota_sf)


#### posicion
posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
areas <- st_area(parques_chapinero)

##Bogota
posicion_b <- apply(dist_matrix_b, 1, function(x) which(min(x) == x))
areas_b <- st_area(parques_bogota)



#dist_bares
dist_bar <- st_distance(x = df_chapinero, y = bar_chapinero)
dist_bar_b <- st_distance(x = df, y = bar_bogota)


#chapinero-distancia al parque de la 93
df_chapinero$dist_bar = apply(dist_bar , 1 , min)

parkch = getbb(place_name = "Parque de la 93", 
               featuretype = "amenity",
               format_out = "sf_polygon")
parkch %>% head()

leaflet() %>% addTiles() %>% addPolygons(data=parkch , col="green")

####estaciones bus
dist_bus_station <- st_distance(x = df_chapinero, y = bus_station_chapinero)
df_chapinero$dist_bus_station = apply(dist_bus_station , 1 , min)

##Bogota bus
dist_bus_station_b <- st_distance(x = df, y = bus_station_bogota)
df$dist_bus_station = apply(dist_bus_station_b , 1 , min)

##Bancos
dist_bank <- st_distance(x = df_chapinero, y = bank_chapinero)
df_chapinero$dist_bank = apply(dist_bank , 1 , min)

#bancos BOGOTA
df$dist_bank = apply(st_distance(x = df, y = bank_bogota) , 1 , min)

#Restaurantes Chapinero
dist_restaurant <- st_distance(x = df_chapinero, y = restaurant_chapinero)
df_chapinero$dist_restaurant = apply(dist_restaurant , 1 , min)
df$dist_restaurant = apply(st_distance(x = df, y = restaurant_bogota) , 1 , min)

##Escuelas
dist_school <- st_distance(x = df_chapinero, y = school_chapinero)
df_chapinero$dist_school = apply(dist_school , 1 , min)
df$dist_school = apply(st_distance(x = df, y = school_bogota) , 1 , min)

##Parques
dist_park <- st_distance(x = df_chapinero, y = parkch)
df_chapinero$dist_park = apply(dist_park , 1 , min)
df$dist_park = apply(st_distance(x = df, y = parques_bogota) , 1 , min)

colnames(df) # vemos que se han agregado las columnas creadas a partir de datos espaciales

colMeans(is.na(df)) * 100


#Se evidencia que existen NAs en las variables "surface_total", "surface_covered", "bathrooms" y "rooms".




saveRDS(df,"df_con_variables.rds")
