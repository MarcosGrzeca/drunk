#Documentação https://github.com/mukul13/rword2vec

library(devtools)
#install_github("mukul13/rword2vec")
library(rword2vec)

model=word2vec(train_file = "wordembedding/text8",output_file = "wordembedding/vec.bin",binary=1)
dist=distance(file_name = "wordembedding/vec.bin",search_word = "planeta",num = 20)

bin_to_txt("wordembedding/vec.bin","vector.txt")
