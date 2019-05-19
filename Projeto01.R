## Pre-processamento dos dados

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

## Feature Engineering
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

## Analise Exploratoria de Dados

# Carregando os Pacotes
library(dplyr)

# Verificar se existem valores ausentes (missing) em cada coluna
# Nenhum valor encontrado
any(is.na(df))

# Analisando dados por agrupamentos
df %>% group_by(ip) %>% tally()
df %>% group_by(ip, click_day) %>% tally()

# Adicionando nova coluna no dataframe
df_count_ip <- df %>% 
  count(ip, sort = TRUE, name = "ip_count")

df <- merge(df, df_count_ip, by=c("ip"))
rm('df_count_ip')

df_count_ip_day <- df %>% 
  count(ip, click_day, sort = TRUE, name = "ip_day_count")

df <- merge(df, df_count_ip_day, by=c('ip','click_day'))
rm('df_count_ip_day')

str(df)
View(df)

# Normalizar as variaveis numericas 
cols <- c('ip', 'app', 'device', 'os', 'channel','click_day','click_hour','ip_count', 'ip_day_count') 
df[, cols] <- scale(df[, cols])

# Verificando overfitting dos dados
# 99.773 registros indicam que o app nao foi baixado
# 227 registros indicam que o app foi baixado
table(df$is_attributed)
prop.table(table(df$is_attributed))

##  Feature Selection (Selecao de Variaveis)
# Carregando os Pacotes
library(ROSE)

# Gerando dados de treino e de teste
splits <- createDataPartition(df$is_attributed, p=0.7, list=FALSE)

# Separando os dados
dados_treino <- df[ splits,]
dados_teste <- df[-splits,]

# Verificando o numero de linhas
nrow(dados_treino)
nrow(dados_teste)

# Treinando o modelo usando Baive Bayes e fazendo predicoes
## devido ao problema de overfitting, o resultado esta tendencioso
## necessario corrigir o problema de overfitting
modeloNB <- NaiveBayes(is_attributed ~. , data=dados_treino)
predNB <- predict(modeloNB, dados_teste)
confusionMatrix(predNB$class, dados_teste$is_attributed)

# AUC
roc.curve(dados_teste$is_attributed, predNB$class)

# Resolvendo problema de Overfitting usando pacote ROSE
#over sampling
dados_treino_new <- ROSE(is_attributed ~ . , data=dados_treino)$data
table(dados_treino_new$is_attributed)
prop.table(table(dados_treino_new$is_attributed))

# Treinando um novo modelo com os novos dados de treino 
modeloNB_v2 <- NaiveBayes(is_attributed ~ . , data=dados_treino_new)
predNB_v2 <- predict(modeloNB_v2, dados_teste)
confusionMatrix(predNB_v2$class, dados_teste$is_attributed)

# AUC
roc.curve(dados_teste$is_attributed, predNB_v2$class)

#AUC ROSE
ROSE.holdout <- ROSE.eval(is_attributed ~ ., 
                          data = dados_treino_new, 
                          learner = rpart, 
                          method.assess = "holdout", 
                          extr.pred = function(obj)obj[,2])
ROSE.holdout

## Análise de Correlação 

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

## Cria um modelo preditivo usando randomForest
# Carregando os Pacotes
library(randomForest)

# Cria o modelo preditivo usando randomForest
modeloRF <- randomForest(is_attributed ~ ., 
                         data = dados_treino_new,
                         ntree = 40, 
                         nodesize = 5)
print(modeloRF)

# Score do Modelo
# Previsões com um modelo de classificação baseado em randomForest
# Gerando previsões nos dados de teste
previsoes <- data.frame(observado = dados_teste$is_attributed,
                        previsto = predict(modeloRF, newdata = dados_teste))


# Visualizando o resultado
View(previsoes)

## Avalia Modelo
# Calculando a Confusion Matrix em R
# Carregando os Pacotes
library(ROCR)
library(caret)

# Gerando as classes de dados
class1 <- predict(modeloRF, newdata = dados_teste, type = 'prob')
class2 <- dados_teste$is_attributed

# Gerando a curva ROC
pred <- prediction(class1[,2], class2)
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))

# Gerando Confusion Matrix com o Caret
# Dataframes com valores observados e previstos
previsoes_v2 <- data.frame(observado = dados_teste$is_attributed,
                           previsto = predict(object = modeloRF, newdata = dados_teste))

confusionMatrix(previsoes_v2$observado, previsoes_v2$previsto)

## Otimizando o Modelo preditivo
# Carregando os Pacotes
library(rpart)
library(rpart.plot)

# Criando uma Cost Function
Cost_func <- matrix(c(0, 1.5, 1, 0), nrow = 2, dimnames = list(c("1", "2"), c("1", "2")))

# Criando o Modelo usando rpart
modeloTree <- rpart(is_attributed ~ .,
                    data = dados_treino_new,
                    method = 'class',
                    parms = list(loss = Cost_func))

# Plot do modelo
rpart.plot(modeloTree, fallen.leaves = FALSE, type = 1)

# Analisando Confusion Matrix
pred.tree <- predict(modeloTree, type = "class")
confusionMatrix(pred.tree, dados_treino_new$is_attributed)

