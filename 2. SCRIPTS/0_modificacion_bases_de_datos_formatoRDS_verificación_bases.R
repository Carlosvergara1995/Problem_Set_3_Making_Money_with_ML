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

#p_load(tidyverse, sf, tmaptools)
p_load(tidyverse,rio,sf,leaflet,tmaptools,osmdata,nngeo,rgeos)

df_train = read.csv("train.csv")
df_test = read.csv("test.csv")

df = bind_rows(df_train,df_test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
head(df)

# creamos variables nuevas a partir de la descripcion y el title
df$parking<-grepl("parqueadero", df$title, ignore.case = TRUE) | grepl("parqueadero", df$description, ignore.case = TRUE)


df$Terraza<-grepl("terraza", df$title, ignore.case = TRUE) | grepl("terraza", df$description, ignore.case = TRUE)

df$Garaje<-grepl("garaje", df$title, ignore.case = TRUE) | grepl("garaje", df$description, ignore.case = TRUE)

sum(is.na(df$bedrooms))

# se defínen las zonas a analizar 
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)

chapinero <- st_transform(chapinero,st_crs(df))

df_chapinero <- df[chapinero,]
#head(df_chapinero)
table(df_chapinero$bedrooms)

available_features()

available_tags("amenity")
bar_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_chapinero , col="red")

osm_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_chapinero <- osm_chapinero %>% osmdata_sf()

bus_station_chapinero <- osm_sf_chapinero$osm_points %>% select(osm_id,amenity)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_chapinero , col="red")

osm_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_chapinero <- osm_chapinero %>% osmdata_sf()

bus_station_chapinero <- osm_sf_chapinero$osm_points %>% select(osm_id,amenity)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_chapinero , col="red")

bank_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bank_chapinero , col="red")

restaurant_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant_chapinero , col="red")

school_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=school_chapinero , col="red")

school_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=school_chapinero , col="red")

parques_chapinero <- opq(bbox = st_bbox(df_chapinero)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

centroides_chapinero <- gCentroid(as(parques_chapinero$geometry, "Spatial"), byid = T)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = parques_chapinero, col = "green",
              opacity = 0.8, popup = parques_chapinero$name) %>%
  addCircles(lng = centroides_chapinero$x, 
             lat = centroides_chapinero$y, 
             col = "red", opacity = 1, radius = 1)

centroides_chapinero_sf <- st_as_sf(centroides_chapinero, coords = c("x", "y"))
dist_matrix <- st_distance(x = df_chapinero, y = centroides_chapinero_sf)

posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
areas <- st_area(parques_chapinero)

dist_bar <- st_distance(x = df_chapinero, y = bar_chapinero)
#dist_bar

#chapinero-distancia al parque de la 93
df_chapinero$dist_bar = apply(dist_bar , 1 , min)

parkch = getbb(place_name = "Parque de la 93", 
               featuretype = "amenity",
               format_out = "sf_polygon")
parkch %>% head()

leaflet() %>% addTiles() %>% addPolygons(data=parkch , col="green")

dist_bus_station <- st_distance(x = df_chapinero, y = bus_station_chapinero)
df_chapinero$dist_bus_station = apply(dist_bus_station , 1 , min)

dist_bank <- st_distance(x = df_chapinero, y = bank_chapinero)
df_chapinero$dist_bank = apply(dist_bank , 1 , min)

dist_restaurant <- st_distance(x = df_chapinero, y = restaurant_chapinero)
df_chapinero$dist_restaurant = apply(dist_restaurant , 1 , min)

dist_school <- st_distance(x = df_chapinero, y = school_chapinero)
df_chapinero$dist_school = apply(dist_school , 1 , min)

dist_park <- st_distance(x = df_chapinero, y = parkch)
df_chapinero$dist_park = apply(dist_park , 1 , min)

colnames(df_chapinero) # vemos que se han agregado las columnas creadas a partir de datos espaciales


