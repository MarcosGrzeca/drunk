library(keras)
samples <- c("The cat sat on the mat.", "The dog ate my homework.")

tokenizer <- text_tokenizer(num_words = 50) %>%
  fit_text_tokenizer(samples)

sequences <- texts_to_sequences(tokenizer, samples)
sequences
one_hot_results <- texts_to_matrix(tokenizer, samples, mode = "binary")
one_hot_results
word_index <- tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")

View(one_hot_results)
