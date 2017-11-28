#install.packages("hunspell")
#https://cloud.r-project.org/web/packages/hunspell/vignettes/intro.html

library(hunspell)

install.packages("text2vec")
#library(text2vec)


args <- commandArgs(TRUE)
text <- args[1]

load("C:/Users/Marcos/Documents/GitHub/drunk/word_embedding.Rda")

text = "drunk"

cos_sim = sim2(x = word_vectors, y = word_vectors[text, , drop = FALSE], method = "cosine", norm = "l2")
print(head(sort(cos_sim[,1], decreasing = TRUE), 10))

print("MARCOS")

