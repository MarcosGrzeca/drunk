#load("pre_selecionar/tmp.Rdata")
library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)
library(mlbench)

load("2110/rdas/2gram-q2-not-null.Rda")
maTreinamento <- maFinal
colunasTreinamento <- colnames(maTreinamento)

load("pre_selecionar/2gram-candidatos.Rda")
maNovos <- subset(maFinal, select = c(beer))

View(maNovos)

maNovos$beer

for(i in 1:length(colunasTreinamento)) {
  if(colunasTreinamento[i] %in% colnames(maNovos)) {
  } else {
    maNovos[colunasTreinamento[i]] <- sample(0, nrow(maNovos), replace = TRUE)
  }
}

maClassificar <- subset(maNovos, select = colunasTreinamento)

save(maClassificar, file = "pre_selecionar/2gram-adaptado_novos_tweets.Rda")

