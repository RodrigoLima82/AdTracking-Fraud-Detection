# Calculando a Confusion Matrix em R

# Carregando os Pacotes
library(ROCR)

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