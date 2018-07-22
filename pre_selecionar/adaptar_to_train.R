library(tools)
library(caret)
library(rowr)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

#load("2110/rdas/2gram-q2-not-null.Rda")

load("pre_selecionar/gram-candidatos.Rda")
maTreinamento <- maFinal
colunasTreinamento <- colnames(maTreinamento)

load("pre_selecionar/gram-q2-not-null.Rda")
maNovos <- maFinal

countNovos <- 0

colsManter <- c()

for(i in 1:length(colunasTreinamento)) {
  if (colunasTreinamento[i] %in% colnames(maNovos)) {
    colsManter <- c(colsManter, colunasTreinamento[i])
  } else {
#    print(colunasTreinamento[i])
#    maNovos[colunasTreinamento[i]] <- sample(0, nrow(maNovos), replace = TRUE)
#    countNovos = countNovos + 1
  }
}

maClassificar <- subset(maNovos, select = colsManter)
maClassificar <- cbind.fill(maClassificar, subset(maNovos, select = resposta))

save(maClassificar, file = "pre_selecionar/gram-q2-not-null-adaptado.Rda")
