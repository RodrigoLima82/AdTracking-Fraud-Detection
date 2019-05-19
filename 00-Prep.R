# Script para checar as colunas do dataset

# Carregando os Pacotes
library(data.table)

# Carregando o Dataset # 
# Os dados brutos contém 100.000 linhas e 8 colunas (atributos). 
# A coluna "is_attributed" é o alvo.
dt <- fread("dados/train_sample.csv")
df <- as.data.frame(dt)

# Remove dt
rm('dt')

# Visualizando dados do dataframe
View(df)
str(df)


# Nome das variáveis
# ip, app, device, os, channel, click_time, attributed_time, is_attributed