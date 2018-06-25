library(keras)
text_vocabulary_size <- 10000
ques_vocabulary_size <- 10000
answer_vocabulary_size <- 500

text_input <- layer_input(shape = list(NULL),
                          dtype = "int32", name = "text")

encoded_text <- text_input %>%
                layer_embedding(input_dim = 64, output_dim = text_vocabulary_size) %>%
                layer_lstm(units = 32)

question_input <- layer_input(shape = list(NULL),
                              dtype = "int32", name = "question")

encoded_question <- question_input %>%
                    layer_embedding(input_dim = 32, output_dim = ques_vocabulary_size) %>%
                    layer_lstm(units = 16)

concatenated <- layer_concatenate(list(encoded_text, encoded_question))
answer <- concatenated %>%
          layer_dense(units = answer_vocabulary_size, activation = "softmax")

model <- keras_model(list(text_input, question_input), answer)
model %>% compile(
          optimizer = "rmsprop",
          loss = "categorical_crossentropy",
          metrics = c("acc")
)

num_samples <- 1000
max_length <- 100

random_matrix <- function(range, nrow, ncol) {
    matrix(sample(range, size = nrow * ncol, replace = TRUE),
    nrow = nrow, ncol = ncol)
}

text <- random_matrix(1:text_vocabulary_size, num_samples, max_length)
question <- random_matrix(1:ques_vocabulary_size, num_samples, max_length)
answers <- random_matrix(0:1, num_samples, answer_vocabulary_size)

model %>% fit(
            list(text, question), answers,
            epochs = 10, batch_size = 128
)

model %>% fit(
            list(text = text, question = question), answers,
            epochs = 10, batch_size = 128
)

