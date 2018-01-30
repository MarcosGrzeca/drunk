load("mardigras/mardigras_compare_new.RData")


load("mardigras/2gram-entidades-mesmas-colunas.Rda")
pred <- predict(testTrain, subset(dataFrameTexto, select = -c(resposta)))
