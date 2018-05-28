library(text2vec)
library(quanteda)

toks <- tokens("insurgents killed in ongoing fighting")
to <- tokens_skipgrams(toks, n = 2, skip = 0:2, concatenator = " ")
to
