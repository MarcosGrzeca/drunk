#Documentação http://text2vec.org/collocations.html

library(text2vec)

#Carregar dados
text8_file = "wordembedding/text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = "~/")
}
txt = readLines(text8_file, n = 1, warn = FALSE)

#Treinamento
model = Collocations$new(collocation_count_min = 50)
it = itoken(txt)
model$fit(it, n_iter = 3)
model$collocation_stat


test_txt = c("i am living in a new apartment in new york city", 
        "new york is the same as new york city", 
        "san francisco is very expensive city", 
        "who claimed that model works?")
it = itoken(test_txt, n_chunks = 1, progressbar = FALSE)
it_phrases = model$transform(it)
it_phrases$nextElem()

#Treinamento com otimizacoes

it = itoken(txt)
v = create_vocabulary(it, stopwords = tokenizers::stopwords("en"))
v = prune_vocabulary(v, term_count_min = 50)
model2 = Collocations$new(vocabulary = v, collocation_count_min = 50, pmi_min = 0)
model2$partial_fit(it)
model2$collocation_stat
temp = model2$collocation_stat[pmi >= 8 & gensim >= 10 & lfmd >= -25, ]
temp
model2$prune(pmi_min = 8, gensim_min = 10, lfmd_min = -25)
identical(temp, model2$collocation_stat)

#continuar treinamento, um dos mecanismos de parada é quando o resultado (numero de frases) entre 2 iterações permanece o mesmo
model2$partial_fit(it)
model2$prune(pmi_min = 8, gensim_min = 10, lfmd_min = -25)
model2$collocation_stat

#criar vocabularios frases + palavras
it_phrases = model2$transform(it)
vocabulary_with_phrases = create_vocabulary(it_phrases, stopwords = tokenizers::stopwords("en"))
vocabulary_with_phrases = prune_vocabulary(vocabulary_with_phrases, term_count_min = 10)
vocabulary_with_phrases[startsWith(vocabulary_with_phrases$term, "new_"), ]

View(vocabulary_with_phrases)


tcm = create_tcm(it_phrases, vocab_vectorizer(vocabulary_with_phrases))
glove = GloVe$new(50, vocabulary = vocabulary_with_phrases, x_max = 50)
wv_main = glove$fit_transform(tcm, 10)

wv_context = glove$components
wv = wv_main + t(wv_context)
cos_sim = sim2(x = wv, y = wv["new_zealand", , drop = FALSE], method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)
