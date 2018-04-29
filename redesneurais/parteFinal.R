
for (indice in 1:nrow(testes)){
  batch <- testes$batch[indice]
  epoca <- testes$epoca[indice]

  for (iteracao in c(1,2,3)){
    history <- model %>% fit(
      x_train, y_train,
      epochs = epoca,
      batch_size = batch,
      validation_split = 0.2
    )
    final <- avaliacaoFinalSave(model, x_test, y_test, history, tecnica, max_features, outputDim, max_features, iteracao)
  }
}