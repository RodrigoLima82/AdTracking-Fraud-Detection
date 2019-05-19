# Otimizando o Modelo preditivo

# Carregando os Pacotes
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

