# Análise de Correlação 

# Carregando os Pacotes
library(corrplot)
library(corrgram)

# obtendo somente as colunas numericas
colunas_numericas <- sapply(dados_treino_new, is.numeric)
colunas_numericas

# Filtrando as colunas numericas para correlacao
data_cor <- cor(dados_treino_new[,colunas_numericas])
data_cor
head(data_cor)

# Criando um corrplot
corrplot(data_cor, method = 'color')

# Criando um corrgram
corrgram(dados_treino_new, order=TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)
