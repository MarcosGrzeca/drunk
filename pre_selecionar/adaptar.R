#load("pre_selecionar/tmp.Rdata")
library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

#load("2110/rdas/2gram-q2-not-null.Rda")

load("pre_selecionar/gram-q2-not-null-union.Rda")
maTreinamento <- maFinal
colunasTreinamento <- colnames(maTreinamento)

load("pre_selecionar/gram-candidatos.Rda")
maNovos <- maFinal
maFinal$resposta

countNovos <- 0

for(i in 1:length(colunasTreinamento)) {
  if(colunasTreinamento[i] %in% colnames(maNovos)) {
  } else {
    print(colunasTreinamento[i])
    maNovos[colunasTreinamento[i]] <- sample(0, nrow(maNovos), replace = TRUE)
    countNovos = countNovos + 1
  }
}

maClassificar <- subset(maNovos, select = colunasTreinamento)
maClassificar <- cbind.fill(maClassificar, subset(maNovos, select = idzaoTweet))

maNovos$resposta
maClassificar$resposta

save(maClassificar, file = "pre_selecionar/gram-adaptado-union_train.Rda")