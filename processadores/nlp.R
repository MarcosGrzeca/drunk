library(tools)
source(file_path_as_absolute("functions.R"))
DATABASE <- "icwsm-2016"

load("teste.Rda")

DATABASE <- "icwsm-2016"
clearConsole();
dados <- query("SELECT id, idInterno, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags FROM tweets")


library(doMC)
library(rowr)

library(RWeka)
library(NLP)
library(openNLP)
library(magrittr)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
money_ann <- Maxent_Entity_Annotator(kind = "money")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann,
                 money_ann)

fore <- function(x) {
  texto <- as.String(x[2]) 
  bio_annotations <- annotate(texto, pipeline)
  bio_doc <- AnnotatedPlainTextDocument(texto, bio_annotations)
  person <- entities(bio_doc, kind = "person")
  location <- entities(bio_doc, kind = "location")
  organization <- entities(bio_doc, kind = "organization")
  money <- entities(bio_doc, kind = "money")
  
  if (identical(person, character(0))) {
    person <- "";
  }
  if (identical(location, character(0))) {
    location <- "";
  }
  if (identical(organization, character(0))) {
    organization <- "";
  }
  if (identical(money, character(0))) {
    money <- "";
  }

  conexao <- connect();
  sqla <-paste("UPDATE `tweets` SET local = '", dbEscapeStrings(conexao, toString(location)), "', person = '", dbEscapeStrings(conexao, toString(person)), "', organization = '", dbEscapeStrings(conexao, toString(organization)), "', money = '", dbEscapeStrings(conexao, toString(money)), "' WHERE idInterno = ", x[1], "", sep="");
  queryConnection(conexao, sqla)
  #print(paste("UPDATE `tweets` SET local = '", dbEscapeStrings(conexao, location), "', person = '", dbEscapeStrings(conexao, person), "', organization = '", dbEscapeStrings(conexao, organization), "', money = '", dbEscapeStrings(conexao, money), "' WHERE id = ", x[1], "", sep=""));
}
apply(subset(dados, select = c(idInterno, textoCompleto)), 1, fore)
