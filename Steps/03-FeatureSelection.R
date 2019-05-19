# Feature Selection (Selecao de Variaveis)

# Carregando os Pacotes
library(ROSE)
library(caret)
library(e1071)
library(rpart)

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
modeloNB <- naiveBayes(is_attributed ~. , data=dados_treino)
predNB <- predict(modeloNB, dados_teste)
confusionMatrix(predNB, dados_teste$is_attributed)

# AUC
roc.curve(dados_teste$is_attributed, predNB)

# Resolvendo problema de Overfitting usando pacote ROSE
#over sampling
dados_treino_new <- ROSE(is_attributed ~ . , data=dados_treino)$data
table(dados_treino_new$is_attributed)
prop.table(table(dados_treino_new$is_attributed))

# Treinando um novo modelo com os novos dados de treino 
modeloNB_v2 <- naiveBayes(is_attributed ~ . , data=dados_treino_new)
predNB_v2 <- predict(modeloNB_v2, dados_teste)
confusionMatrix(predNB_v2, dados_teste$is_attributed)

# AUC
roc.curve(dados_teste$is_attributed, predNB_v2)

#AUC ROSE
ROSE.holdout <- ROSE.eval(is_attributed ~ ., 
                          data = dados_treino_new, 
                          learner = rpart, 
                          method.assess = "holdout", 
                          extr.pred = function(obj)obj[,2])
ROSE.holdout
