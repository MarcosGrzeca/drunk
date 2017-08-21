#install.packages("hunspell")
#https://cloud.r-project.org/web/packages/hunspell/vignettes/intro.html


library(tools)
source(file_path_as_absolute("functions.R"))
DATABASE <- "icwsm-2016"

clearConsole();
dados <- query("SELECT id, idInterno, textParser, textoParserEmoticom as textoCompleto FROM tweets WHERE situacao = 'N'")

library(hunspell)

#words <- c("beer", "wiskey", "wine")
#correct <- hunspell_check(words)
#print(correct)

conexao <- connect();
fore <- function(x) {
  texto <- x[2]
  bad <- hunspell(texto)
  if (identical(bad[[1]], character(0))) {
  } else {
    erros <- length(bad[[1]])
    query(paste("UPDATE `tweets` SET numeroErros = '", erros, "' WHERE idInterno = ", x[1], "", sep=""));
    teste <- unlist(bad[[1]])
    for(i in 1:length(teste)) {
      sql <- paste("INSERT INTO `tweets_erro_new` (idTweetInterno, palavra) VALUES (", x[1], ", '", dbEscapeStrings(conexao, teste[i]), "')", sep="");
      query(sql)
    }
  }   
}

apply(subset(dados, select = c(idInterno, textParser)), 1, fore)
