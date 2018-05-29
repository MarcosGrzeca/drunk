#https://tensorflow.rstudio.com/keras/articles/examples/babi_rnn.html

library(keras)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)

# Function definition -----------------------------------------------------

tokenize_words <- function(x){
  x <- x %>% 
    str_replace_all('([[:punct:]]+)', ' \\1') %>% 
    str_split(' ') %>%
    unlist()
  x[x != ""]
}

parse_stories <- function(lines, only_supporting = FALSE){
  lines <- lines %>% 
    str_split(" ", n = 2) %>%
    map_df(~tibble(nid = as.integer(.x[[1]]), line = .x[[2]]))
  
  lines <- lines %>%
    mutate(
      split = map(line, ~str_split(.x, "\t")[[1]]),
      q = map_chr(split, ~.x[1]),
      a = map_chr(split, ~.x[2]),
      supporting = map(split, ~.x[3] %>% str_split(" ") %>% unlist() %>% as.integer()),
      story_id = c(0, cumsum(nid[-nrow(.)] > nid[-1]))
    ) %>%
    select(-split)
  
  stories <- lines %>%
    filter(is.na(a)) %>%
    select(nid_story = nid, story_id, story = q)
  
  questions <- lines %>%
    filter(!is.na(a)) %>%
    select(-line) %>%
    left_join(stories, by = "story_id") %>%
    filter(nid_story < nid)
  
  if(only_supporting){
    questions <- questions %>%
      filter(map2_lgl(nid_story, supporting, ~.x %in% .y))
  }
  
  questions %>%
    group_by(story_id, nid, question = q, answer = a) %>%
    summarise(story = paste(story, collapse = " ")) %>%
    ungroup() %>% 
    mutate(
      question = map(question, ~tokenize_words(.x)),
      story = map(story, ~tokenize_words(.x)),
      id = row_number()
    ) %>%
    select(id, question, answer, story)
}

vectorize_stories <- function(data, vocab, story_maxlen, query_maxlen){
  
  questions <- map(data$question, function(x){
    map_int(x, ~which(.x == vocab))
  })
  
  stories <- map(data$story, function(x){
    map_int(x, ~which(.x == vocab))
  })
  
  # "" represents padding
  answers <- sapply(c("", vocab), function(x){
    as.integer(x == data$answer)
  })
  
  
  list(
    questions = pad_sequences(questions, maxlen = query_maxlen),
    stories   = pad_sequences(stories, maxlen = story_maxlen),
    answers   = answers
  )
}

# Parameters --------------------------------------------------------------

max_length <- 99999
embed_hidden_size <- 50
batch_size <- 32
epochs <- 40

# Data Preparation --------------------------------------------------------

path <- get_file(
  fname = "babi-tasks-v1-2.tar.gz",
  origin = "https://s3.amazonaws.com/text-datasets/babi_tasks_1-20_v1-2.tar.gz"
)
untar(path, exdir = str_replace(path, fixed(".tar.gz"), "/"))
path <- str_replace(path, fixed(".tar.gz"), "/")

# Default QA1 with 1000 samples
# challenge = '%stasks_1-20_v1-2/en/qa1_single-supporting-fact_%s.txt'
# QA1 with 10,000 samples
# challenge = '%stasks_1-20_v1-2/en-10k/qa1_single-supporting-fact_%s.txt'
# QA2 with 1000 samples
challenge <- "%stasks_1-20_v1-2/en/qa2_two-supporting-facts_%s.txt"
# QA2 with 10,000 samples
# challenge = '%stasks_1-20_v1-2/en-10k/qa2_two-supporting-facts_%s.txt'

train <- read_lines(sprintf(challenge, path, "train")) %>%
  parse_stories() %>%
  filter(map_int(story, ~length(.x)) <= max_length)

test <- read_lines(sprintf(challenge, path, "test")) %>%
  parse_stories() %>%
  filter(map_int(story, ~length(.x)) <= max_length)


train

# extract the vocabulary
all_data <- bind_rows(train, test)

View(all_data$question)
View(all_data$story)

vocab <- c(unlist(all_data$question), all_data$answer, 
           unlist(all_data$story)) %>%
  unique() %>%
  sort()

# Reserve 0 for masking via pad_sequences
vocab_size <- length(vocab) + 1
story_maxlen <- map_int(all_data$story, ~length(.x)) %>% max()
query_maxlen <- map_int(all_data$question, ~length(.x)) %>% max()

# vectorized versions of training and test sets
train_vec <- vectorize_stories(train, vocab, story_maxlen, query_maxlen)
test_vec <- vectorize_stories(test, vocab, story_maxlen, query_maxlen)

story_maxlen <- map_int(all_data$story, ~length(.x)) %>% max()
View(all_data$story)


nrow(train_vec)
View(train_vec)
View(test_vec)

c(story_maxlen)

# Defining the model ------------------------------------------------------

sentence <- layer_input(shape = c(story_maxlen), dtype = "int32")
encoded_sentence <- sentence %>% 
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3)

question <- layer_input(shape = c(query_maxlen), dtype = "int32")
encoded_question <- question %>%
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_repeat_vector(n = story_maxlen)

merged <- list(encoded_sentence, encoded_question) %>%
  layer_add() %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_dropout(rate = 0.3)

preds <- merged %>%
  layer_dense(units = vocab_size, activation = "softmax")

model <- keras_model(inputs = list(sentence, question), outputs = preds)
model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)

model

# Training ----------------------------------------------------------------

View(train_vec$stories)
View(train_vec$questions)

model %>% fit(
  x = list(train_vec$stories, train_vec$questions),
  y = train_vec$answers,
  batch_size = batch_size,
  epochs = epochs,
  validation_split=0.05
)

evaluation <- model %>% evaluate(
  x = list(test_vec$stories, test_vec$questions),
  y = test_vec$answers,
  batch_size = batch_size
)

evaluation