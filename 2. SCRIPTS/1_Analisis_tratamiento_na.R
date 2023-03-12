library(stringr)

# Extract bathroom information as character string
bathroom_str <- str_extract(df_chapinero$description, "(?i)\\b(bano|banos)\\b\\s*(\\d+)")

# Clean and convert string to numeric
bathroomsc <- as.numeric(gsub("(?i)\\b(bano|banos)\\b", "", bathroom_str, ignore.case = TRUE))


table(bathroomsc)

# remmplazamos los na de la variable bethroom con los datos anteriores 

df_chapinero$bathrooms <- ifelse(is.na(df_chapinero$bathrooms), bathroomsc, df_chapinero$bathrooms)



