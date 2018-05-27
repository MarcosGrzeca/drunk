timesteps <- 100 #Number of timesteps in the input sequence numero de linhas
input_features <- 32 #Dimensionality of the input feature space numero de colunas
output_features <- 64 #Dimensionality of the output feature space
random_array <- function(dim) {
  array(runif(prod(dim)), dim = dim)
}
inputs <- random_array(dim = c(timesteps, input_features)) #Input data: random noise for the sake of the example

state_t <- rep_len(0, length = c(output_features)) #Initial state: an all-zero vector

W <- random_array(dim = c(output_features, input_features)) #Creates random weight matrices
U <- random_array(dim = c(output_features, output_features))
b <- random_array(dim = c(output_features, 1))

output_sequence <- array(0, dim = c(timesteps, output_features))
for (i in 1:nrow(inputs)) {
  input_t <- inputs[i,] #input_t is a vector of shape (input_features).
  output_t <- tanh(as.numeric((W %*% input_t) + (U %*% state_t) + b)) #Combines the input with the current state (the previous output) to obtain the current output
  output_sequence[i,] <- as.numeric(output_t) #Updates the result matrix
  state_t <- output_t #Updates the state of the network for the next timestep
}

state_t
