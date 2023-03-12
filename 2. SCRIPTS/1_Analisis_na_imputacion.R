p_load(stringr, dplyr,rpart)
#cargamos nuestra data
df_con_variables <- readRDS("~/Desktop/git hut repositorios/Problem_Set_3_Making_Money_with_ML/3. STORE/df_con_variables.rds")

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

#BAÑOS ####
# Extract bathroom information as character string
bathroom_str <- str_extract(df_con_variables$description, "(?i)(\\d+)\\s*(bano|banos|banios|banio|baio|baios)\\b|(?i)\\b(bano|banos|banios|banio|baio|baios)\\b\\s*(\\d+)")

# Clean and convert string to numeric
bathroomsc <- as.numeric(gsub("(?i)\\b(bano|banos|banios|banio|baio|baios)\\b|\\s", "", bathroom_str, ignore.case = TRUE))

table(bathroomsc)

# reemplazamos los na de la variable bathroom con los datos extraídos
data <- data_frame(df_con_variables) #analizar la conversiónde data frame
data$bathrooms <- ifelse(is.na(data$bathrooms), bathroomsc, data$bathrooms)
sum(is.na(data$bathrooms))

# se reemplazaron 4214 na con los datos de descripcción

# Convertir valores atípicos en NAs
data$bathrooms<- ifelse(data$bathrooms > 15, NA, data$bathrooms)

# Verificar que los valores atípicos hayan sido convertidos a NAs
sum(is.na(data$bathrooms))


# Calcular el promedio de baños para apartamentos y casas
mean_bath_apto <- round(mean(data$bathrooms[data$property_type == "Apartamento"], na.rm = TRUE))
mean_bath_casa <- round(mean(data$bathrooms[data$property_type == "Casa"], na.rm = TRUE))

# Reemplazar los NA en función de property_type
data$bathrooms <- ifelse(
  is.na(data$bathrooms) & data$property_type == "Apartamento",
  mean_bath_apto,
  ifelse(
    is.na(data$bathrooms) & data$property_type == "Casa",
    mean_bath_casa,
    data$bathrooms
  )
)

# Redondear la variable bathrooms a números enteros
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

# se reemplazaron 4317 na
# Calcular el promedio de surface_total por número de bedrooms
mean_surface_total <- ave(data$surface_total, data$bedrooms, FUN = function(x) mean(x, na.rm = TRUE))

# Reemplazar los NA con el promedio correspondiente
data$surface_total <- ifelse(is.na(data$surface_total), mean_surface_total[match(data$bedrooms, unique(data$bedrooms))], data$surface_total)

sum(is.na(data$surface_total))


#Imputacion de datos####
df <- data[, !(names(data) %in% c("surface_covered", "rooms", "title", "description", "city", "property_id", "month", "year", "geometry", "operation_type"))]
