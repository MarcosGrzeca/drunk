#install.packages("hunspell")
#https://cloud.r-project.org/web/packages/hunspell/vignettes/intro.html

args <- commandArgs(TRUE)
text <- args[1]

library(hunspell)

bad_words <- hunspell(text)

if (identical(bad_words[[1]], character(0))) {
  #print("false");
} else {
  print(bad_words)
  hunspell_suggest(bad_words[[1]])
}