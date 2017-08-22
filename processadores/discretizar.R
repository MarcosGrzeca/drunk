discretizarTaxas <- function(dados) {
  dados$adjetivo <- 0
  dados$adjetivo[dados$taxaAdjetivo > 0.20] <- 1
  dados$adjetivo[dados$taxaAdjetivo > 0.40] <- 2
  dados$adjetivo[dados$taxaAdjetivo > 0.60] <- 3
  dados$adjetivo[dados$taxaAdjetivo > 0.80] <- 4
  
  dados$substantivo <- 0
  dados$substantivo[dados$taxaSubstantivo > 0.15] <- 1
  dados$substantivo[dados$taxaSubstantivo > 0.30] <- 2
  dados$substantivo[dados$taxaSubstantivo > 0.45] <- 3
  dados$substantivo[dados$taxaSubstantivo > 0.60] <- 4
  dados$substantivo[dados$taxaSubstantivo > 0.75] <- 5
  dados$substantivo[dados$taxaSubstantivo > 0.90] <- 6
  
  dados$adverbio <- 0
  dados$adverbio[dados$taxaAdverbio > 0.17] <- 1
  dados$adverbio[dados$taxaAdverbio > 0.34] <- 2
  dados$adverbio[dados$taxaAdverbio > 0.51] <- 3
  dados$adverbio[dados$taxaAdverbio > 0.68] <- 4
  
  dados$verbo <- 0
  dados$verbo[dados$taxaVerbo > 0.17] <- 1
  dados$verbo[dados$taxaVerbo > 0.34] <- 2
  dados$verbo[dados$taxaVerbo > 0.51] <- 3
  dados$verbo[dados$taxaVerbo > 0.68] <- 4
  dados <- subset(dados, select = -c(taxaVerbo, taxaAdverbio, taxaSubstantivo, taxaAdjetivo))
  return (dados)
}

discretizarHora <- function(dados) {
  #dados$turno[dados$hora < 6] <- "N"
  #dados$turno[dados$hora >= 6] <- "M"
  #dados$turno[dados$hora >= 14] <- "T"
  #dados$turno[dados$hora >= 22] <- "N"
  
  dados$turno[dados$hora < 6] <- 0
  dados$turno[dados$hora >= 6] <- 1
  dados$turno[dados$hora >= 14] <- 2
  dados$turno[dados$hora >= 22] <- 0
  dados$turno <- as.factor(dados$turno)
  dados <- subset(dados, select = -c(hora))
  return (dados)
}

discretizarSentimentos <- function(dados) {
  #sentimentos
  dados$emotiom <- 0
  dados$emotiom[dados$sentiment < 0] <- -1
  dados$emotiom[dados$sentiment < -0.33] <- -2
  dados$emotiom[dados$sentiment < -0.66] <- -3
  dados$emotiom[dados$sentiment > 0] <- 1
  dados$emotiom[dados$sentiment > 0.33] <- 2
  dados$emotiom[dados$sentiment > 0.66] <- 3
  
  dados$emotiomH <- 0
  dados$emotiomH[dados$sentimentH < 0] <- -1
  dados$emotiomH[dados$sentimentH < -0.5] <- -2
  dados$emotiomH[dados$sentimentH > 0] <- 1
  dados$emotiomH[dados$sentimentH > 0.5] <- 2
  dados <- subset(dados, select = -c(sentiment, sentimentH))
  return (dados)
}