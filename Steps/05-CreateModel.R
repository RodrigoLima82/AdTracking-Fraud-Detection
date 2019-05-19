# Cria um modelo preditivo usando randomForest

# Carregando os Pacotes
library(randomForest)

# Cria o modelo preditivo usando randomForest
modeloRF <- randomForest(is_attributed ~ ., 
                         data = dados_treino_new,
                         ntree = 40, 
                         nodesize = 5)
print(modeloRF)