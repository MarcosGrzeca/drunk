library(tools)
source(file_path_as_absolute("functions.R"))
DATABASE <- "icwsm-2016"

clearConsole();
dados <- query("SELECT id, idInterno, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags FROM tweets WHERE nlp IS NULL")


library(doMC)
library(rowr)

library(RWeka)
library(NLP)
library(openNLP)
library(magrittr)

library(jsonlite)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pipeline <- list(sent_token_annotator,
                 word_token_annotator,
                 pos_tag_annotator)

fore <- function(x) {
  texto <- as.String(x[2])
  a3 <- annotate(texto, pipeline)
  anota <- annotate(texto, Maxent_Chunk_Annotator(), a3)
  
  conexao <- connect();
  #sqla <-paste("UPDATE `tweets` SET nlp = '", dbEscapeStrings(conexao, toString(toJSON(data.frame(matrix(unlist(anota), nrow=length(anota)))))), "' WHERE idInterno = ", x[1], "", sep="");
  sqla <-paste("UPDATE `tweets` SET nlp = '", toJSON(data.frame(anota)), "' WHERE idInterno = ", x[1], "", sep="");
  queryConnection(conexao, sqla)
  #print(paste("UPDATE `tweets` SET local = '", dbEscapeStrings(conexao, location), "', person = '", dbEscapeStrings(conexao, person), "', organization = '", dbEscapeStrings(conexao, organization), "', money = '", dbEscapeStrings(conexao, money), "' WHERE id = ", x[1], "", sep=""));
}

apply(subset(dados, select = c(idInterno, textoCompleto)), 1, fore)