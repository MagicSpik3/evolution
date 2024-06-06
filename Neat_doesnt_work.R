#atttempt at NEAT in R
install.packages("reticulate")

library(reticulate)


use_python("C:/Program Files/Python312/python.exe")

library(reticulate)

# Source the Python script
source_python("neat_xor.py")

# Run the NEAT algorithm
result <- py$run()

# The result will contain the winner and stats
winner <- result[[1]]
stats <- result[[2]]

# Print the winner
print(winner)




# Install RcppNEAT package from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("r-lib/Rcpp")
devtools::install_github("martinmodrak/RcppNEAT")

# Load the package
library(RcppNEAT)

# Define the XOR problem
xor_inputs <- matrix(c(
  0, 0,
  0, 1,
  1, 0,
  1, 1
), ncol = 2, byrow = TRUE)

xor_outputs <- matrix(c(
  0,
  1,
  1,
  0
), ncol = 1)

# NEAT parameters
params <- list(
  pop_size = 150,
  max_generations = 50,
  mutation_rate = 0.05,
  crossover_rate = 0.7
)

# Fitness function
fitness_function <- function(network) {
  total_error <- 0
  for (i in 1:nrow(xor_inputs)) {
    input <- xor_inputs[i, ]
    expected_output <- xor_outputs[i, ]
    output <- predict(network, input)
    total_error <- total_error + sum((output - expected_output)^2)
  }
  return(-total_error)
}

# Initialize the population
population <- initialize_population(params$pop_size, 2, 1)

# Evolve the population
for (generation in 1:params$max_generations) {
  cat("Generation:", generation, "\n")
  for (i in 1:params$pop_size) {
    population[[i]]$fitness <- fitness_function(population[[i]])
  }
  population <- reproduce(population, params)
}

# Find the best network
best_network <- population[[which.max(sapply(population, function(ind) ind$fitness))]]

# Test the best network
for (i in 1:nrow(xor_inputs)) {
  input <- xor_inputs[i, ]
  expected_output <- xor_outputs[i, ]
  output <- predict(best_network, input)
  cat("Input:", input, "Expected Output:", expected_output, "Predicted Output:", output, "\n")
}
