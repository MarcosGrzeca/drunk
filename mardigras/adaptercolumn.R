library(rowr)
library(RWeka)

#LER DATASET MODELO TREINADO
load("2110/rdas/2gram-entidades-hora-erro-not-null-new-test.Rda")
origem <- maFinal

#LER DATASET VERIFICAR MODELO
load("mardigras/2gram-entidades.Rda")
destino <- maFinal

colsOrigem <- colnames(origem)
colsDestino <- colnames(destino)
aspectosManter <- c()

#colsOrigem <- c("marcos", "drink", "beer")
#colsDestino <- c("drink", "beer")

for(i in 1:length(colsOrigem)) {
    aspectosManter <- c(aspectosManter, colsOrigem[i])
    col <- colsOrigem[i]
    if (!all(colsOrigem[i] %in% colsDestino)) {
      destino[[col]] = 0
    }
}

#PARTE FINAL
dataFrameTexto <- destino[aspectosManter]

save(dataFrameTexto, file = "mardigras/2gram-entidades-mesmas-colunas.Rda")
