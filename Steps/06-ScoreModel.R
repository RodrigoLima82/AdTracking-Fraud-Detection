# Previsões com um modelo de classificação baseado em randomForest

# Gerando previsões nos dados de teste
previsoes <- data.frame(observado = dados_teste$is_attributed,
                        previsto = predict(modeloRF, newdata = dados_teste))


# Visualizando o resultado
View(previsoes)