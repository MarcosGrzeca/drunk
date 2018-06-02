library(tools)
library(keras)

set.seed(10)

source(file_path_as_absolute("redesneurais/getDados.R"))

resultados <- data.frame(matrix(ncol = 4, nrow = 0))
names(resultados) <- c("Técnica", "InputDim", "OutputDim", "Epochs", "Batch", ""  "F1", "Precisão", "Revocação", "Acurácia")

addRow <- function(resultados, baseline, matriz, ...) {
  print(baseline)
  newRes <- data.frame(baseline, matriz$byClass["F1"], matriz$byClass["Precision"], matriz$byClass["Recall"])
  rownames(newRes) <- baseline
  names(newRes) <- c("Baseline", "F1", "Precisão", "Revocação")
  newdf <- rbind(resultados, newRes)
  save.image(file="baselines/compare.RData")
  return (newdf)
}

library(magrittr)
