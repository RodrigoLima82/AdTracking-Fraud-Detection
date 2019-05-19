# Aplicando Engenharia de Atributos em Variaveis Numericas

# Transformar o objeto de data
df$click_time <- as.POSIXct(df$click_time)

# Extraindo dia e hora
df$click_day <- as.integer(format(df$click_time, "%d"))
df$click_hour <- as.integer(format(df$click_time, "%H"))

# Transformando variáveis numéricas em variáveis categóricas
df$is_attributed <- as.factor(df$is_attributed)


# Remover colunas nao utilizadas
df$click_time <- NULL
df$attributed_time <- NULL

str(df)
