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

library(hunspell)
library(qdap)
which_misspelled(text, suggest=FALSE)
which_misspelled(text, suggest=TRUE)

text <- "i'm gOod perrson lol.Go to ciroc #mention thats Cigerettes";

text <- "hasn't"
bad_words <- hunspell(text)
bad_words
hunspell_suggest(bad_words[[1]])






hunspell(text, format = c("text", "man", "latex", "html", "xml"),
         dict = dictionary("en_US"), ignore = en_stats)

hunspell_parse(text, format = c("text", "man", "latex", "html", "xml"),
               dict = dictionary("en_US"))

hunspell_check(words, dict = dictionary("en_US"))
hunspell_suggest(words, dict = dictionary("en_US"))
hunspell_analyze(words, dict = dictionary("en_US"))
hunspell_stem(words, dict = dictionary("en_US"))
hunspell_info(dict = dictionary("en_US"))

#apply(subset(dados, select = c(idInterno, textParser)), 1, fore)


library(Rcpp)
library(microbenchmark)

# pure Rcpp/C++ implementation

sourceCpp("
          #include <Rcpp.h> 
          
          using namespace Rcpp; 
          
          // [[Rcpp::export]]
          std::vector< std::string > ucfirst( std::vector< std::string > strings ) {
          
          int len = strings.size();
          
          for( int i=0; i < len; i++ ) {
          std::transform(strings[i].begin(), strings[i].end(), strings[i].begin(), ::tolower);
          strings[i][0] = toupper( strings[i][0] );
          }
          
          return strings;
          
          }")

r_ucfirst <- function (str) {
  paste(toupper(substring(str, 1, 1)), tolower(substring(str, 2)), sep = "")
}

print(ucfirst("hello"))
## [1] "Hello"

print(r_ucfirst("hello"))
## [1] "Hello"

mb <- microbenchmark(ucfirst("hello"), r_ucfirst("hello"), times=1000)
print(mb)

.libPaths()
