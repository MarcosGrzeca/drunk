options(max.print = 99999999)

library(tools)
source(file_path_as_absolute("functions.R"))
source(file_path_as_absolute("processadores/discretizar.R"))

#Configuracoes
DATABASE <- "icwsm"
clearConsole();

dados <- query("SELECT t.id,
       q1 AS resposta,
       textoParserRisadaEmoticom
        FROM tweets t
        WHERE textparser <> ''
        AND id <> 462478714693890048
               ")

dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$textoParserRisadaEmoticom <- enc2utf8(dados$textoParserRisadaEmoticom)
clearConsole()

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)

setDT(dados)
setkey(dados, id)

stem_tokenizer1 =function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="en")
}

#dados$textParser = sub("'", "", dados$textoParserRisadaEmoticom)

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textoParserRisadaEmoticom, 
                  preprocessor = prep_fun, 
                  #                  tokenizer = stem_tokenizer1,
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)


stop_words = tm::stopwords("en")
vocab = create_vocabulary(it_train, stopwords = stop_words)
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)


#Concatenar resultados
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))



dataFrameTexto$wine
clearConsole()

library(rowr)
library(RWeka)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- subset(maFinal, select = -c(textoParserRisadaEmoticom, id))

#results <- lapply(maFinal,
#  function(x) {
#    print(x)
#    if (x[10] == 1)  {
#      print("TRUE")
#    }
#  }
#)

resultado = matrix(c(0, 0, 0, 0), nrow=2, ncol=2)

acertou = 0;
errou = 0

for (i in 1:nrow(maFinal)){
  achou = 0
  if (maFinal[i, ]$drinking == 1) {
    achou = 1
  }
  if (maFinal[i, ]$tequila == 1) {
    achou = 1
  }
  if (maFinal[i, ]$wine == 1) {
    achou = 1
  }
  if (maFinal[i, ]$vodka == 1) {
    achou = 1
  }
  if (maFinal[i, ]$alcohol == 1) {
    achou = 1
  }
  if (maFinal[i, ]$drunk == 1) {
    achou = 1
  }
  if (maFinal[i, ]$liquor == 1) {
    achou = 1
  }
  if (maFinal[i, ]$brew == 1) {
    achou = 1
  }
  if (maFinal[i, ]$tour == 1) {
    achou = 1
  }
  if (maFinal[i, ]$ale == 1) {
    achou = 1
  }
  if (maFinal[i, ]$booze == 1) {
    achou = 1
  }
  if (maFinal[i, ]$tasting == 1) {
    achou = 1
  }
  if (maFinal[i, ]$cold == 1) {
    achou = 1
  }
  if (maFinal[i, ]$alcoholic == 1) {
    achou = 1
  }
  if (maFinal[i, ]$drink == 1) {
    achou = 1
  }
  if (maFinal[i, ]$drinks == 1) {
    achou = 1
  }
  if (maFinal[i, ]$hammered == 1) {
    achou = 1
  }
  if (maFinal[i, ]$ipa == 1) {
    achou = 1
  }
  if (maFinal[i, ]$beer == 1) {
    achou = 1
  }
  if (maFinal[i, ]$champagne == 1) {
    achou = 1
  }
  if (maFinal[i, ]$bud == 1) {
    achou = 1
  }
  if (maFinal[i, ]$rum == 1) {
    achou = 1
  }
  if (maFinal[i, ]$crawl == 1) {
    achou = 1
  }
  if (maFinal[i, ]$brewing == 1) {
    achou = 1
  }
  if (maFinal[i, ]$pong == 1) {
    achou = 1
  }
  if (maFinal[i, ]$bar == 1) {
    achou = 1
  }
  if (maFinal[i, ]$beverage == 1) {
    achou = 1
  }
  if (maFinal[i, ]$beyonce == 1) {
    achou = 1
  }
  if (maFinal[i, ]$bottle == 1) {
    achou = 1
  }
  if (maFinal[i, ]$bottles == 1) {
    achou = 1
  }
  if (maFinal[i, ]$breakfast == 1) {
    achou = 1
  }
  if (maFinal[i, ]$bullandbearpub == 1) {
    achou = 1
  }
  if (maFinal[i, ]$can == 1) {
    achou = 1
  }
  if (maFinal[i, ]$company == 1) {
    achou = 1
  }
  if (maFinal[i, ]$genesee == 1) {
    achou = 1
  }
  if (maFinal[i, ]$get == 1) {
    achou = 1
  }
  if (maFinal[i, ]$glass == 1) {
    achou = 1
  }
  if (maFinal[i, ]$good == 1) {
    achou = 1
  }
  if (maFinal[i, ]$house == 1) {
    achou = 1
  }
  if (maFinal[i, ]$ice == 1) {
    achou = 1
  }
  if (maFinal[i, ]$light == 1) {
    achou = 1
  }
  if (maFinal[i, ]$max == 1) {
    achou = 1
  }
  if (maFinal[i, ]$much == 1) {
    achou = 1
  }
  if (maFinal[i, ]$nice == 1) {
    achou = 1
  }
  if (maFinal[i, ]$one == 1) {
    achou = 1
  }
  if (maFinal[i, ]$pale == 1) {
    achou = 1
  }
  if (maFinal[i, ]$people == 1) {
    achou = 1
  }
  if (maFinal[i, ]$pub == 1) {
    achou = 1
  }
  if (maFinal[i, ]$qtyler1495 == 1) {
    achou = 1
  }
  if (maFinal[i, ]$really == 1) {
    achou = 1
  }
  if (maFinal[i, ]$risada == 1) {
    achou = 1
  }
  if (maFinal[i, ]$scotch == 1) {
    achou = 1
  }
  if (maFinal[i, ]$shit == 1) {
    achou = 1
  }
  if (maFinal[i, ]$still == 1) {
    achou = 1
  }
  if (maFinal[i, ]$store == 1) {
    achou = 1
  }
  if (maFinal[i, ]$turn == 1) {
    achou = 1
  }
  if (maFinal[i, ]$two == 1) {
    achou = 1
  }
  if (maFinal[i, ]$amp == 1) {
    achou = 1
  }
  if (maFinal[i, ]$dinosaur == 1) {
    achou = 1
  }
  if (maFinal[i, ]$grill == 1) {
    achou = 1
  }
  if (maFinal[i, ]$que == 1) {
    achou = 1
  }
  if (maFinal[i, ]$water == 1) {
    achou = 1
  }
  if (maFinal[i, ]$fucked == 1) {
    achou = 1
  }
  if (maFinal[i, ]$blast == 1) {
    achou = 1
  }
  if (maFinal[i, ]$cans == 1) {
    achou = 1
  }
  if (maFinal[i, ]$athletic == 1) {
    achou = 1
  }
  if (maFinal[i, ]$rochester == 1) {
    achou = 1
  }
  if (maFinal[i, ]$kids == 1) {
    achou = 1
  }
  if (maFinal[i, ]$someone == 1) {
    achou = 1
  }
  if (maFinal[i, ]$wait == 1) {
    achou = 1
  }
  if (maFinal[i, ]$amellywood == 1) {
    achou = 1
  }
  if (maFinal[i, ]$party == 1) {
    achou = 1
  }
  if (maFinal[i, ]$wanna == 1) {
    achou = 1
  }
  if (maFinal[i, ]$final == 1) {
    achou = 1
  }
  if (maFinal[i, ]$cowles == 1) {
    achou = 1
  }
  if (maFinal[i, ]$time == 1) {
    achou = 1
  }
  if (maFinal[i, ]$great == 1) {
    achou = 1
  }
  if (maFinal[i, ]$fucking == 1) {
    achou = 1
  }
  if (maFinal[i, ]$tonight == 1) {
    achou = 1
  }
  if (maFinal[i, ]$cream == 1) {
    achou = 1
  }
  if (maFinal[i, ]$held == 1) {
    achou = 1
  }
  if (maFinal[i, ]$marijuana == 1) {
    achou = 1
  }
  if (maFinal[i, ]$pretty == 1) {
    achou = 1
  }
  if (maFinal[i, ]$love == 1) {
    achou = 1
  }
  if (maFinal[i, ]$better == 1) {
    achou = 1
  }
  if (maFinal[i, ]$fall == 1) {
    achou = 1
  }
  if (maFinal[i, ]$last == 1) {
    achou = 1
  }
  if (maFinal[i, ]$irish == 1) {
    achou = 1
  }

  resultado[maFinal[i, ]$resposta, achou] = resultado[maFinal[i, ]$resposta, achou] + 1
    
  if (achou == maFinal[i, ]$resposta) {
    acertou = acertou + 1 
  } else {
    errou = errou + 1
  }
}


print("ACERTOU  ")
print(acertou)

print(errou)


print(resultado)

save.image("word_embedding_validate.Rda")