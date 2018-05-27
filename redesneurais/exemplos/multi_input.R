#https://tensorflow.rstudio.com/keras/articles/functional_api.html

#The main input will receive the headline, as a sequence of integers (each integer encodes a word). The integers will be between 1 and 10,000 (a vocabulary of 10,000 words) and the sequences will be 100 words long.

library(keras)

main_input <- layer_input(shape = c(100), dtype = 'int32', name = 'main_input')

lstm_out <- main_input %>% 
  layer_embedding(input_dim = 10000, output_dim = 512, input_length = 100) %>% 
  layer_lstm(units = 32)

 #Here we insert the auxiliary loss, allowing the LSTM and Embedding layer to be trained smoothly even though the main loss will be much higher in the model:
 auxiliary_output <- lstm_out %>% 
 	layer_dense(units = 1, activation = 'sigmoid', name = 'aux_output')

#At this point, we feed into the model our auxiliary input data by concatenating it with the LSTM output, stacking a deep densely-connected network on top and adding the main logistic regression layer
auxiliary_input <- layer_input(shape = c(5), name = 'aux_input')

main_output <- layer_concatenate(c(lstm_out, auxiliary_input)) %>%  
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'sigmoid', name = 'main_output')

#This defines a model with two inputs and two outputs:
model <- keras_model(
  inputs = c(main_input, auxiliary_input), 
  outputs = c(main_output, auxiliary_output)
)

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  loss_weights = c(1.0, 0.2)
)

#We can train the model by passing it lists of input arrays and target arrays:
model %>% fit(
  x = list(headline_data, additional_data),
  y = list(labels, labels),
  epochs = 50,
  batch_size = 32
)

